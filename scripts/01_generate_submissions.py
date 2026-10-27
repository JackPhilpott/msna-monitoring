"""Step 1: synthetic survey submissions, with planted errors and an answer key.

Builds the XLSForm from questionnaire/*.csv, then simulates six weeks of data
collection on the sample from msna-sampling:

1. Fieldwork plan: each partner fields teams of two enumerators in proportion to
   its workload, and each team visits about one cluster a day. Teams interview
   primary households first and move to reserves when a household is absent or
   declines.
2. Answers: driven by a hidden vulnerability score per household. IDP
   households are worse off on average; state and LGA effects are random
   draws, so nothing here reflects real conditions.
3. Planted errors: each one is written to an answer key with the action
   cleaning should take (delete, change or flag), so step 2 can be scored.
   Some partners follow up: they reach every cluster, go back to replace each
   interview cleaning will delete with an unused reserve household, and make
   up for clusters lost to access with extra reserve households elsewhere in
   the stratum.

Outputs: questionnaire/msna_reduced_xlsform.xlsx
         data/synthetic/raw/submissions_<state>.csv.gz  (a Kobo-style export)
         data/synthetic/raw/enumerators.csv
         data/synthetic/raw/answer_key.csv  (never available in real data collection)
"""
import math
import sys
import uuid
from datetime import date, datetime, timedelta
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "python"))
from monitoring_tools import (ROOT, from_sampling, load_config, make_xlsx_reproducible,  # noqa: E402
                              output_dir, write_csv)

FCS_GROUPS = ["cereals", "pulses", "dairy", "meat", "vegetables", "fruit", "oil", "sugar"]
RCSI_ITEMS = ["less_preferred", "borrow_food", "limit_portions", "restrict_adults", "reduce_meals"]
HHS_ITEMS = ["no_food", "sleep_hungry", "whole_day"]
LCSI_ITEMS = {"spend_savings": -0.6, "sell_assets": -0.9, "reduce_health_education": -1.4,
              "sell_productive_assets": -1.7, "sell_house_land": -2.6, "beg": -2.4}
NEEDS = ["food", "water", "shelter", "health", "education", "livelihoods", "protection", "nfis"]
OTHER_INCOME_TEXT = ["tailoring", "motorcycle taxi", "firewood collection", "hairdressing", "blacksmith"]
RECODABLE_OTHER = {"farming my own land": "crops", "selling goods in the market": "petty_trade",
                   "rearing goats and sheep": "livestock", "casual work on other farms": "daily_labour"}
WAT = timedelta(hours=1)  # West Africa Time, UTC+1


def logistic(x):
    return 1 / (1 + np.exp(-x))


def pick(rng, n, options: dict):
    keys = list(options)
    p = np.array(list(options.values()), dtype=float)
    return np.array(keys, dtype=object)[rng.choice(len(keys), size=n, p=p / p.sum())]


def build_xlsform() -> Path:
    q = ROOT / "questionnaire"
    path = q / "msna_reduced_xlsform.xlsx"
    settings = pd.DataFrame({"form_title": ["MSNA, reduced questionnaire (synthetic demo)"],
                             "form_id": ["msna_reduced_demo"], "version": ["2026.1"]})
    with pd.ExcelWriter(path, engine="openpyxl") as w:
        pd.read_csv(q / "survey.csv", dtype=str).fillna("").to_excel(w, sheet_name="survey", index=False)
        pd.read_csv(q / "choices.csv", dtype=str).to_excel(w, sheet_name="choices", index=False)
        settings.to_excel(w, sheet_name="settings", index=False)
    make_xlsx_reproducible(path)
    return path


def follow_up_partners(cfg, partners):
    """Partners whose teams reach every cluster and go back to replace interviews cleaning deletes."""
    rng = np.random.default_rng([cfg["seed"], 3])
    k = round(cfg["collection"]["follow_up_share"] * len(partners))
    return set(rng.choice(partners, size=k, replace=False))


def assign_teams(cfg, clusters):
    """Teams per partner in proportion to its workload; each team takes a block of neighbouring clusters.

    Returns the clusters with a team column, and the enumerator roster.
    """
    c = cfg["collection"]
    clusters = clusters.sort_values(["partner", "adm2_pcode", "pop_type", "cluster_id"]).copy()
    rows = []
    for partner, idx in clusters.groupby("partner", sort=True).groups.items():
        letter = partner.split()[-1]
        n_teams = math.ceil(len(idx) / c["clusters_per_team"])
        block = np.arange(len(idx)) * n_teams // len(idx) + 1
        clusters.loc[idx, "team"] = [f"team_{letter}_{t:02d}" for t in block]
        for k in range(1, n_teams * c["team_size"] + 1):
            rows.append({"enumerator_id": f"enum_{letter}_{k:02d}", "partner": partner,
                         "team": f"team_{letter}_{(k - 1) // c['team_size'] + 1:02d}"})
    return clusters.sort_values("cluster_id"), pd.DataFrame(rows)


def visit_days(cfg, rng, clusters):
    """Each team visits about one cluster a day, from a staggered start and with rest days.

    Teams differ in how often they rest, so some finish early and others late.
    Replacement clusters join a team's queue once they are drawn (round 1 on day 12,
    round 2 on day 26). A team takes two clusters in a day only when it would not
    otherwise finish.
    """
    c = cfg["collection"]
    earliest = clusters["round_added"].map({0: 0, 1: 12, 2: 26})
    out = pd.Series(c["days"] - 1, index=clusters.index)
    for _, cl in clusters.groupby("team", sort=True):
        start = int(rng.integers(0, c["team_start_spread"]))
        rest = rng.uniform(*c["rest_day_share"])
        work = [d for d in range(start, c["days"]) if rng.random() >= rest]
        pending = list(cl.sort_values(["round_added", "adm2_pcode", "cluster_id"]).index)
        for i, d in enumerate(work):
            per_day = -(-len(pending) // (len(work) - i))  # ceiling division
            for k in [k for k in pending if earliest[k] <= d][:per_day]:
                out[k] = d
                pending.remove(k)
    return out.to_numpy()


def plan_interviews(cfg, rng, clusters, households, roster):
    """One row per household approached: who, where, when, and whether they consented."""
    c = cfg["collection"]
    teams = roster.groupby(["partner", "team"])["enumerator_id"].apply(list)
    hh_by_cluster = {k: g.sort_values(["role", "rank"], ascending=[True, True]) for k, g in
                     households.groupby("cluster_id")}
    start = date.fromisoformat(str(c["start_date"]))
    rows = []
    for cl, day in zip(clusters.itertuples(), clusters["visit_day"]):
        members = teams[(cl.partner, cl.team)]
        hh = hh_by_cluster.get(cl.cluster_id)
        if hh is None:
            continue
        order = pd.concat([hh[hh["role"] == "primary"], hh[hh["role"] == "reserve"]])
        done = 0
        for h in order.itertuples():
            if done == cfg_target(cfg):
                break
            if h.role == "primary" and rng.random() < c["primary_unavailable"]:
                continue
            consent = "no" if rng.random() < c["non_consent"] else "yes"
            rows.append({"cluster_id": cl.cluster_id, "household_id": h.household_id, "partner": cl.partner,
                         "stratum_id": cl.stratum_id, "pop_type": cl.pop_type, "psu_id": cl.psu_id,
                         "adm1_name": cl.adm1_name, "adm2_pcode": cl.adm2_pcode,
                         "enumerator_id": members[rng.integers(len(members))],
                         "date": start + timedelta(days=int(day)), "hh_lon": h.lon, "hh_lat": h.lat,
                         "consent": consent})
            done += consent == "yes"
    return pd.DataFrame(rows)


def cfg_target(cfg):
    return 6  # households interviewed per cluster, as in msna-sampling


def add_times_and_gps(cfg, rng, df):
    """Interview times: each enumerator starts mid-morning and works through their households
    back to back, walking between them, with a longer trip when moving to a second cluster."""
    c = cfg["collection"]
    n = len(df)
    minutes = np.exp(rng.normal(c["duration_meanlog"], c["duration_sdlog"], n))
    minutes = np.where(df["consent"] == "no", rng.uniform(2, 5, n), minutes)
    first = rng.normal(9.5 * 60, 45, n)  # minutes after midnight
    walk = rng.uniform(15, 60, n)
    trip = rng.uniform(45, 120, n)
    clock, last_cluster, start_local = {}, {}, []
    for i, key in enumerate(zip(df["enumerator_id"], df["date"])):
        t = clock.get(key, first[i])
        if key in last_cluster and last_cluster[key] != df["cluster_id"].iat[i]:
            t += trip[i]
        start_local.append(datetime.combine(key[1], datetime.min.time()) + timedelta(minutes=float(t)))
        clock[key], last_cluster[key] = t + minutes[i] + walk[i], df["cluster_id"].iat[i]
    df["duration_min"] = minutes
    df["start_dt"] = start_local
    df["end_dt"] = [s + timedelta(minutes=float(m)) for s, m in zip(start_local, minutes)]
    dx, dy = rng.normal(0, c["gps_noise_m"], n), rng.normal(0, c["gps_noise_m"], n)
    df["gps_lat"] = df["hh_lat"] + dy / 111_320
    df["gps_lon"] = df["hh_lon"] + dx / (111_320 * np.cos(np.radians(df["hh_lat"])))
    df["gps_precision"] = np.round(rng.uniform(3, 15, n), 1)
    return df


def generate_answers(rng, df, sites):
    """Answers for consenting households, driven by a hidden vulnerability score."""
    yes = df["consent"].eq("yes").to_numpy()
    n = len(df)
    idp = df["pop_type"].eq("idp").to_numpy()
    in_camp = df["psu_id"].map(sites.set_index("site_id")["category"]).eq("in_camp").to_numpy()
    state_eff = {s: rng.normal(0, 0.3) for s in sorted(df["adm1_name"].unique())}
    lga_eff = {l: rng.normal(0, 0.25) for l in sorted(df["adm2_pcode"].unique())}
    v = df["adm1_name"].map(state_eff).to_numpy() + df["adm2_pcode"].map(lga_eff).to_numpy() + \
        0.7 * idp + rng.normal(0, 1, n)
    a = {}

    a["hh_size"] = np.clip(1 + rng.poisson(5.2 + 0.4 * idp), 1, 20)
    a["hoh_gender"] = np.where(rng.random(n) < 0.22 + 0.1 * idp, "female", "male")
    a["hoh_age"] = np.clip(np.round(rng.normal(42, 12, n)), 18, 85).astype(int)
    a["n_children_u5"] = rng.binomial(a["hh_size"] - 1, 0.18)
    a["n_children_school_age"] = rng.binomial(a["hh_size"] - 1 - a["n_children_u5"], 0.33)
    a["displacement_status"] = np.where(
        idp, np.where(rng.random(n) < 0.92, "idp", "returnee"),
        pick(rng, n, {"non_displaced": 0.93, "returnee": 0.05, "idp": 0.02}))
    a["months_displaced"] = np.where(a["displacement_status"] == "idp",
                                     np.clip(np.round(rng.lognormal(3.0, 0.8, n)), 1, 180), np.nan)

    fcs = {"cereals": (3.0, 0.4), "pulses": (0.0, 0.6), "dairy": (-0.6, 0.6), "meat": (-0.7, 0.6),
           "vegetables": (0.6, 0.5), "fruit": (-1.2, 0.4), "oil": (1.4, 0.5), "sugar": (0.3, 0.5),
           "condiments": (2.5, 0.3)}
    for g, (base, slope) in fcs.items():
        a[f"fcs_{g}"] = rng.binomial(7, logistic(base - slope * v))
    for item, base in zip(RCSI_ITEMS, [-0.4, -1.2, -0.8, -1.6, -0.9]):
        a[f"rcsi_{item}"] = rng.binomial(7, logistic(base + 0.9 * v))
    for item, base in zip(HHS_ITEMS, [-1.6, -2.0, -2.8]):
        happened = rng.random(n) < logistic(base + 1.1 * v)
        often = rng.random(n) < logistic(-1.2 + 0.6 * v)
        freq = np.where(often, "often", np.where(rng.random(n) < 0.5, "sometimes", "rarely"))
        a[f"hhs_{item}"] = np.where(happened, "yes", "no")
        a[f"hhs_{item}_freq"] = np.where(happened, freq, None)
    for item, base in LCSI_ITEMS.items():
        p_yes = logistic(base + 0.7 * v)
        r = rng.random(n)
        a[f"lcsi_{item}"] = np.where(r < p_yes, "yes", np.where(r < p_yes * 1.15, "no_exhausted",
                                     np.where(r > 0.93, "not_applicable", "no_not_needed")))

    water_resident = {"borehole": 0.35, "protected_well": 0.15, "unprotected_well": 0.17, "public_tap": 0.08,
                      "piped": 0.05, "surface_water": 0.10, "water_vendor": 0.08, "other": 0.02}
    water_camp = {"public_tap": 0.30, "borehole": 0.30, "water_vendor": 0.15, "unprotected_well": 0.10,
                  "surface_water": 0.10, "other": 0.05}
    a["water_source"] = np.where(in_camp, pick(rng, n, water_camp), pick(rng, n, water_resident))
    a["water_sufficient"] = np.where(rng.random(n) < logistic(1.0 - 0.5 * v), "yes", "no")
    a["sanitation_facility"] = np.where(
        in_camp, pick(rng, n, {"pit_latrine_slab": 0.45, "pit_latrine_no_slab": 0.30, "open_defecation": 0.20,
                               "other": 0.05}),
        pick(rng, n, {"flush_toilet": 0.08, "pit_latrine_slab": 0.30, "pit_latrine_no_slab": 0.37,
                      "open_defecation": 0.23, "other": 0.02}))
    a["handwashing_soap"] = np.where(rng.random(n) < logistic(0.8 - 0.5 * v), "yes", "no")
    a["shelter_type"] = np.where(
        in_camp, pick(rng, n, {"tent": 0.35, "makeshift": 0.45, "collective_centre": 0.15, "unfinished_house": 0.05}),
        np.where(idp, pick(rng, n, {"makeshift": 0.30, "unfinished_house": 0.30, "solid_house": 0.25,
                                    "collective_centre": 0.15}),
                 pick(rng, n, {"solid_house": 0.65, "unfinished_house": 0.25, "makeshift": 0.08, "tent": 0.01,
                               "collective_centre": 0.01})))
    a["shelter_damage"] = np.where(rng.random(n) < logistic(-0.3 + 0.5 * v), "yes", "no")
    need = rng.random(n) < 0.45
    access = rng.random(n) < logistic(0.8 - 0.6 * v)
    a["health_need"] = np.where(need, "yes", "no")
    a["health_access"] = np.where(need, np.where(access, "yes", "no"), None)
    a["health_barrier"] = np.where(need & ~access, pick(rng, n, {"cost": 0.45, "distance": 0.25, "insecurity": 0.12,
                                                                 "no_staff_medicine": 0.13, "other": 0.05}), None)
    school = a["n_children_school_age"]
    a["n_children_attending"] = np.where(school > 0, rng.binomial(school, logistic(0.6 - 0.5 * v)), np.nan)
    a["income_source"] = np.where(
        idp, pick(rng, n, {"crops": 0.10, "livestock": 0.03, "daily_labour": 0.35, "petty_trade": 0.20,
                           "salaried": 0.02, "remittances": 0.05, "assistance": 0.15, "none": 0.08, "other": 0.02}),
        pick(rng, n, {"crops": 0.38, "livestock": 0.10, "daily_labour": 0.20, "petty_trade": 0.15, "salaried": 0.06,
                      "remittances": 0.03, "assistance": 0.02, "none": 0.04, "other": 0.02}))
    a["income_source_other"] = np.where(a["income_source"] == "other",
                                        np.array(OTHER_INCOME_TEXT, dtype=object)[rng.integers(5, size=n)], None)
    a["aid_received"] = np.where(rng.random(n) < logistic(-1.2 + 1.5 * idp + 0.2 * v), "yes", "no")
    weights = np.column_stack([1.5 + 0.5 * v, np.full(n, 0.9), 0.6 + 0.6 * idp, np.full(n, 0.9),
                               np.full(n, 0.5), np.full(n, 0.8), np.full(n, 0.3), 0.4 + 0.3 * idp])
    keys = np.log(np.clip(weights, 0.05, None)) + rng.gumbel(size=weights.shape)
    top3 = np.argsort(-keys, axis=1)[:, :3]
    a["priority_needs"] = [" ".join(NEEDS[j] for j in sorted(row)) for row in top3]

    answers = pd.DataFrame(a)
    answers[~yes] = None  # households that declined have no answers
    return pd.concat([df.reset_index(drop=True), answers], axis=1)


def add_respondent(cfg, data):
    """Who answered, and their age. A few enumerators round ages to the nearest five (age heaping).

    Uses its own random stream, so adding these questions left every other answer unchanged.
    """
    rng = np.random.default_rng([cfg["seed"], 1])
    n = len(data)
    is_hoh = rng.random(n) < 0.7
    data["resp_is_hoh"] = np.where(is_hoh, "yes", "no")
    data["resp_gender"] = np.where(is_hoh, data["hoh_gender"], np.where(rng.random(n) < 0.6, "female", "male"))
    data["resp_age"] = np.where(is_hoh, data["hoh_age"], np.clip(np.round(rng.normal(35, 12, n)), 18, 85))
    enums = np.sort(data["enumerator_id"].unique())
    heapers = rng.choice(enums, size=round(0.08 * len(enums)), replace=False)
    heaped = data["enumerator_id"].isin(heapers).to_numpy() & (rng.random(n) < 0.6)
    for col in ["hoh_age", "resp_age"]:
        data.loc[heaped, col] = 5 * np.round(data.loc[heaped, col].astype(float) / 5)
    data.loc[data["consent"].ne("yes"), ["resp_is_hoh", "resp_gender", "resp_age"]] = None
    return data


def add_setting(cfg, data, clusters, sites):
    """Type of location, fixed per PSU: camps for IDP sites in camps, otherwise urban or rural."""
    rng = np.random.default_rng([cfg["seed"], 2])
    psus = clusters.drop_duplicates("psu_id").sort_values("psu_id")
    camp = psus["psu_id"].map(sites.set_index("site_id")["category"]).eq("in_camp").to_numpy()
    u = rng.random(len(psus))
    setting = np.where(camp, np.where(u < 0.6, "camp_formal", "camp_informal"), np.where(u < 0.25, "urban", "rural"))
    data["setting"] = data["psu_id"].map(dict(zip(psus["psu_id"], setting)))
    return data


def plant_errors(cfg, rng, df, clusters, households, roster, sites, follow_up):
    """Plant errors in a copy of the data and return (data, answer_key)."""
    e = cfg["errors"]
    key = []
    data = df.copy()
    pool = data.index[data["consent"].eq("yes")].to_numpy()

    # enumerators whose interviews are fast and alike
    counts = data.loc[pool, "enumerator_id"].value_counts()
    bad_enums = rng.choice(counts[counts >= 60].index.to_numpy(), e["problem_enumerators"], replace=False)
    bad_rows = data.index[data["enumerator_id"].isin(bad_enums) & data["consent"].eq("yes")]
    template_cols = [col for col in data.columns if col.startswith(("fcs_", "rcsi_", "hhs_", "lcsi_"))] + \
        ["water_source", "water_sufficient", "sanitation_facility", "handwashing_soap", "shelter_type",
         "shelter_damage", "health_need", "health_access", "health_barrier", "income_source", "aid_received",
         "priority_needs"]
    for enum in bad_enums:
        rows = data.index[(data["enumerator_id"] == enum) & data["consent"].eq("yes")]
        template = data.loc[rows[0], template_cols]
        copy_rows = rows[rng.random(len(rows)) < 0.7]
        data.loc[copy_rows, template_cols] = template.to_numpy()
        data.loc[rows, "duration_min"] = np.exp(rng.normal(np.log(17), 0.15, len(rows)))
        for r in rows:
            key.append((r, "problem_enumerator", "flag", f"enumerator {enum}"))
    pool = np.setdiff1d(pool, bad_rows)
    rng.shuffle(pool)

    def take(rate):
        nonlocal pool
        k = int(round(rate * len(df)))
        chosen, pool = pool[:k], pool[k:]
        return chosen

    for r in take(e["too_short"]):
        data.loc[r, "duration_min"] = rng.uniform(4, 12)
        key.append((r, "too_short", "delete", f"{data.loc[r, 'duration_min']:.0f} minutes"))
    for r in take(e["gps_far"]):
        dist, angle = rng.uniform(600, 5000), rng.uniform(0, 2 * np.pi)
        lat = data.loc[r, "gps_lat"]
        data.loc[r, "gps_lat"] = lat + dist * np.sin(angle) / 111_320
        data.loc[r, "gps_lon"] += dist * np.cos(angle) / (111_320 * np.cos(np.radians(lat)))
        key.append((r, "gps_far", "flag", f"{dist / 1000:.1f} km from the household"))
    for r in take(e["consent_no_continued"]):
        data.loc[r, "consent"] = "no"
        key.append((r, "consent_no_continued", "delete", "answers recorded without consent"))
    for r in take(e["fcs_flat"]):
        value = 7 if rng.random() < 0.7 else 0
        data.loc[r, [f"fcs_{g}" for g in FCS_GROUPS]] = value
        key.append((r, "fcs_flat", "flag", f"every food group {value} days"))
    for r in take(e["fcs_hhs_contradiction"]):
        data.loc[r, [f"fcs_{g}" for g in FCS_GROUPS]] = [7, 6, 5, 6, 7, 4, 7, 6]
        for item in HHS_ITEMS:
            data.loc[r, [f"hhs_{item}", f"hhs_{item}_freq"]] = ["yes", "often"]
        key.append((r, "fcs_hhs_contradiction", "flag", "acceptable food consumption with severe hunger"))
    for r in take(e["hh_size_outlier"]):
        data.loc[r, "hh_size"] = int(rng.integers(35, 81))
        key.append((r, "hh_size_outlier", "change", f"household size {data.loc[r, 'hh_size']}"))
    for r in take(e["children_exceed_hh"]):
        data.loc[r, "n_children_school_age"] = data.loc[r, "hh_size"] - data.loc[r, "n_children_u5"] + \
            int(rng.integers(1, 4))
        data.loc[r, "n_children_attending"] = min(data.loc[r, "n_children_school_age"], 2)
        key.append((r, "children_exceed_hh", "flag", "more children than household members"))
    school = np.array([r for r in pool if data.loc[r, "n_children_school_age"] > 0])
    rate = e["attending_exceed_school_age"]
    for r in school[:int(round(rate * len(df)))]:
        data.loc[r, "n_children_attending"] = data.loc[r, "n_children_school_age"] + int(rng.integers(1, 4))
        key.append((r, "attending_exceed_school_age", "flag", "more children attending than of school age"))
    pool = np.setdiff1d(pool, school[:int(round(rate * len(df)))])
    rng.shuffle(pool)
    for r in take(e["wrong_cluster"]):
        data.loc[r, "cluster_id"] = data.loc[r, "cluster_id"].rsplit("_", 1)[0] + "_C9" + str(rng.integers(10))
        key.append((r, "wrong_cluster", "delete", "cluster code not in the sample"))
    for r in take(e["other_to_recode"]):
        text = list(RECODABLE_OTHER)[rng.integers(len(RECODABLE_OTHER))]
        data.loc[r, ["income_source", "income_source_other"]] = ["other", text]
        key.append((r, "other_to_recode", "change", f"'{text}' is {RECODABLE_OTHER[text]}"))

    # extra rows: the same submission uploaded twice, and the same household interviewed twice
    extra, extra_key = [], []
    for r in take(e["duplicate_exact"]):
        extra.append(data.loc[r].copy())
        extra_key.append(("duplicate_exact", "delete", "same submission uploaded twice", data.loc[r, "_uuid"]))
    for r in take(e["duplicate_near"]):
        row = data.loc[r].copy()
        row["_uuid"] = str(uuid.UUID(bytes=rng.bytes(16), version=4))
        shift = timedelta(minutes=float(rng.uniform(60, 180)))
        row["start_dt"], row["end_dt"] = row["start_dt"] + shift, row["end_dt"] + shift
        row["hh_size"] = max(1, row["hh_size"] + int(rng.choice([-1, 1])))
        row["fcs_meat"] = int(np.clip(row["fcs_meat"] + rng.choice([-1, 1]), 0, 7))
        extra.append(row)
        extra_key.append(("duplicate_near", "delete", f"household {row['household_id']} interviewed twice",
                          row["_uuid"]))

    # interviews in clusters that partners had reported inaccessible
    dropped = clusters[clusters["status"] == "inaccessible"].sample(
        n=max(1, int(round(e["dropped_cluster"] * len(df) / 6))), random_state=int(rng.integers(1 << 31)))
    for cl in dropped.itertuples():
        # msna-sampling drew no households for inaccessible clusters, so place 6 around the cluster point
        team = roster[(roster["partner"] == cl.partner)]["enumerator_id"].to_numpy()
        donors = data.loc[data["consent"].eq("yes") & data["stratum_id"].eq(cl.stratum_id)]
        if donors.empty:
            continue
        for k in range(1, 7):
            row = donors.iloc[int(rng.integers(len(donors)))].copy()
            row["cluster_id"], row["household_id"], row["psu_id"] = cl.cluster_id, f"{cl.cluster_id}_H{k:02d}", cl.psu_id
            row["enumerator_id"] = team[rng.integers(len(team))]
            row["gps_lat"] = cl.lat + rng.normal(0, 300) / 111_320
            row["gps_lon"] = cl.lon + rng.normal(0, 300) / (111_320 * np.cos(np.radians(cl.lat)))
            row["_uuid"] = str(uuid.UUID(bytes=rng.bytes(16), version=4))
            extra.append(row)
            extra_key.append(("dropped_cluster", "delete", f"cluster {cl.cluster_id} was reported inaccessible",
                              row["_uuid"]))

    # Follow-up partners return to interview unused reserve households: in place of each
    # interview cleaning will delete, and, where clusters were lost to access without a
    # replacement, extra households in the stratum's remaining clusters to make up its target.
    # These are clean interviews, so they are not in the answer key; their answers are copied
    # from a clean interview in the same stratum, each used once and never the same
    # enumerator's (cleaning would rightly flag an enumerator whose interviews repeat).
    c = cfg["collection"]
    last_day = date.fromisoformat(str(c["start_date"])) + timedelta(days=c["days"] - 1)
    keyed = {r for r, *_ in key}
    clean = data[data["consent"].eq("yes") & ~data.index.isin(keyed)]
    used = set(data["household_id"])
    reserves = households[households["role"].eq("reserve") & ~households["household_id"].isin(used)]
    spare = {k: list(g.sort_values("rank").itertuples()) for k, g in reserves.groupby("cluster_id")}

    donors_used = set()

    def revisit(visit, h):
        donors = clean[clean["stratum_id"].eq(visit["stratum_id"]) & clean["enumerator_id"].ne(visit["enumerator_id"])
                       & ~clean.index.isin(donors_used)]
        if donors.empty:
            return False
        pick = donors.index[int(rng.integers(len(donors)))]
        donors_used.add(pick)
        row = donors.loc[pick].copy()
        row["cluster_id"], row["household_id"], row["psu_id"] = h.cluster_id, h.household_id, visit["psu_id"]
        row["enumerator_id"] = visit["enumerator_id"]
        row["date"] = min(visit["date"] + timedelta(days=int(rng.integers(c["follow_up_days"][0],
                                                                            c["follow_up_days"][1] + 1))), last_day)
        row["start_dt"] = datetime.combine(row["date"], datetime.min.time()) + \
            timedelta(minutes=float(rng.uniform(9.5 * 60, 14 * 60)))
        row["duration_min"] = float(np.exp(rng.normal(c["duration_meanlog"], c["duration_sdlog"])))
        row["hh_lat"], row["hh_lon"] = h.lat, h.lon
        row["gps_lat"] = h.lat + rng.normal(0, c["gps_noise_m"]) / 111_320
        row["gps_lon"] = h.lon + rng.normal(0, c["gps_noise_m"]) / (111_320 * np.cos(np.radians(h.lat)))
        row["_uuid"] = str(uuid.UUID(bytes=rng.bytes(16), version=4))
        extra.append(row)
        return True

    for r, error, *_ in key:
        lost = data.loc[r]
        if error in ("too_short", "consent_no_continued", "wrong_cluster") and lost["partner"] in follow_up:
            options = spare.get(lost["household_id"].rsplit("_", 1)[0], [])
            if options and revisit(lost, options[0]):
                options.pop(0)

    visits = data[data["cluster_id"].isin(clusters["cluster_id"])].drop_duplicates("cluster_id")
    target = clusters[clusters["draw"].eq("stage1")].groupby("stratum_id").size() * cfg_target(cfg)
    active_n = clusters[clusters["status"].eq("active")].groupby("stratum_id").size()
    shortfall = (target - active_n.reindex(target.index, fill_value=0) * cfg_target(cfg))
    for stratum, need in shortfall[shortfall > 0].items():
        stratum_visits = visits[visits["stratum_id"].eq(stratum)].sort_values("cluster_id")
        if stratum_visits.empty or stratum_visits["partner"].iat[0] not in follow_up:
            continue
        rotation = [v for _, v in stratum_visits.iterrows()]
        k = added = 0
        while added < need and any(spare.get(v["cluster_id"]) for v in rotation):
            v = rotation[k % len(rotation)]
            k += 1
            if spare.get(v["cluster_id"]):
                if not revisit(v, spare[v["cluster_id"]][0]):
                    break
                spare[v["cluster_id"]].pop(0)
                added += 1

    key_df = pd.DataFrame([{"_uuid": data.loc[r, "_uuid"], "error_type": t, "expected_action": a, "detail": d}
                           for r, t, a, d in key])
    if extra:
        data = pd.concat([data, pd.DataFrame(extra)], ignore_index=True)
        key_df = pd.concat([key_df, pd.DataFrame([{"_uuid": u, "error_type": t, "expected_action": a, "detail": d}
                                                  for t, a, d, u in extra_key])], ignore_index=True)
    return data, key_df


def to_export(data):
    """Shape the data like a Kobo export: form fields, then _gps and _ system columns."""
    out = data.copy()
    out["end_dt"] = [s + timedelta(minutes=float(m)) for s, m in zip(out["start_dt"], out["duration_min"])]
    fmt = "%Y-%m-%dT%H:%M:%S.000+01:00"
    out["start"] = [d.strftime(fmt) for d in out["start_dt"]]
    out["end"] = [d.strftime(fmt) for d in out["end_dt"]]
    out["today"] = [d.strftime("%Y-%m-%d") for d in out["start_dt"]]
    out["_submission_time"] = [(d - WAT + timedelta(hours=6)).strftime("%Y-%m-%dT%H:%M:%S") for d in out["end_dt"]]
    out["partner"] = out["partner"].str.lower().str.replace(" ", "_")
    out["_gps_latitude"] = out["gps_lat"].round(6)
    out["_gps_longitude"] = out["gps_lon"].round(6)
    out["_gps_precision"] = out["gps_precision"]
    for need in NEEDS:
        out[f"priority_needs/{need}"] = np.where(
            out["priority_needs"].isna(), None,
            out["priority_needs"].fillna("").str.split().apply(lambda s, k=need: int(k in s)))
    survey = pd.read_csv(ROOT / "questionnaire" / "survey.csv", dtype=str)
    fields = [n for t, n in zip(survey["type"], survey["name"]) if isinstance(n, str)
              and not t.startswith(("begin_group", "end_group")) and n != "gps"]
    cols = fields + [f"priority_needs/{need}" for need in NEEDS] + \
        ["_gps_latitude", "_gps_longitude", "_gps_precision", "_uuid", "_submission_time"]
    out = out.sort_values(["_submission_time", "_uuid"]).reset_index(drop=True)
    out["_index"] = np.arange(1, len(out) + 1)
    for col in ["resp_age", "hh_size", "hoh_age", "n_children_u5", "n_children_school_age", "months_displaced",
                "n_children_attending"] + [c for c in cols if c.startswith(("fcs_", "rcsi_"))]:
        out[col] = out[col].astype("Int64")
    return out[cols + ["_index"]], out


def main() -> int:
    cfg = load_config()
    rng = np.random.default_rng(cfg["seed"])
    src = from_sampling(cfg)
    form = build_xlsform()

    clusters = pd.read_csv(src / "clusters_after_resampling.csv").sort_values("cluster_id")
    households = pd.concat([pd.read_csv(f) for f in sorted((src / "households").glob("*.csv"))],
                           ignore_index=True)
    sites = pd.read_csv(src / "idp_sites_synthetic.csv")

    active = clusters[clusters["status"] == "active"].copy()
    follow_up = follow_up_partners(cfg, sorted(clusters["partner"].unique()))
    missed = rng.random(len(active)) < cfg["collection"]["clusters_not_reached"]
    active = active[~missed | active["partner"].isin(follow_up)].copy()
    active, roster = assign_teams(cfg, active)
    active["visit_day"] = visit_days(cfg, rng, active)
    plan = plan_interviews(cfg, rng, active, households, roster)
    plan["_uuid"] = [str(uuid.UUID(bytes=rng.bytes(16), version=4)) for _ in range(len(plan))]
    plan = add_times_and_gps(cfg, rng, plan)
    data = add_respondent(cfg, generate_answers(rng, plan, sites))
    data, key = plant_errors(cfg, rng, data, clusters, households, roster, sites, follow_up)
    data = add_setting(cfg, data, clusters, sites)
    export, full = to_export(data)

    raw = output_dir(cfg) / "raw"
    for old in raw.glob("submissions_*.csv.gz"):
        old.unlink()
    for state, part in export.groupby(full["adm1_name"], sort=True):
        write_csv(part, raw / f"submissions_{state.lower()}.csv.gz")
    write_csv(roster, raw / "enumerators.csv")
    write_csv(key.sort_values(["error_type", "_uuid"]), raw / "answer_key.csv")

    per_day = full.groupby("today").size()
    print(f"XLSForm: {form.relative_to(ROOT)}")
    print(f"Fieldwork: {active['cluster_id'].nunique():,} clusters visited over {per_day.size} days by "
          f"{len(roster)} enumerators; peak {per_day.max():,} submissions on {per_day.idxmax()}")
    print(f"Submissions: {len(export):,} ({(export['consent'] == 'yes').sum():,} with consent)")
    print(f"Planted errors: {len(key):,} rows in the answer key, "
          + ", ".join(f"{k} {v}" for k, v in key["error_type"].value_counts().sort_index().items()))
    return 0


if __name__ == "__main__":
    sys.exit(main())
