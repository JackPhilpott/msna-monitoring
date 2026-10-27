"""Checks on the synthetic submissions and the answer key. Skipped until step 1 has run."""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))
from monitoring_tools import load_config, output_dir  # noqa: E402

RAW = output_dir(load_config()) / "raw"
pytestmark = pytest.mark.skipif(not (RAW / "answer_key.csv").exists(), reason="run step 1 first")

ERROR_TYPES = {"too_short", "duplicate_exact", "duplicate_near", "gps_far", "consent_no_continued", "fcs_flat",
               "fcs_hhs_contradiction", "hh_size_outlier", "children_exceed_hh", "attending_exceed_school_age",
               "wrong_cluster", "dropped_cluster", "other_to_recode", "problem_enumerator"}


@pytest.fixture(scope="module")
def raw():
    return pd.concat([pd.read_csv(f, dtype=str) for f in sorted(RAW.glob("submissions_*.csv.gz"))])


@pytest.fixture(scope="module")
def key():
    return pd.read_csv(RAW / "answer_key.csv")


def test_every_error_type_is_planted(key):
    assert set(key["error_type"]) == ERROR_TYPES


def test_every_planted_error_points_at_a_submission(raw, key):
    assert key["_uuid"].isin(raw["_uuid"]).all()


def test_expected_actions_are_known(key):
    assert set(key["expected_action"]) <= {"delete", "change", "flag"}


def test_submission_ids_are_unique_apart_from_planted_duplicates(raw, key):
    repeated = raw["_uuid"][raw["_uuid"].duplicated()]
    assert set(repeated) <= set(key.loc[key["error_type"] == "duplicate_exact", "_uuid"])


def test_food_days_are_whole_days_in_range(raw):
    days = raw.filter(regex=r"^(fcs|rcsi)_").apply(pd.to_numeric).to_numpy().ravel()
    days = days[~np.isnan(days)]  # blank for households that declined
    assert len(days) > 0
    assert ((days >= 0) & (days <= 7) & (days == np.round(days))).all()


def test_select_multiple_columns_match_the_answer(raw):
    answered = raw[raw["priority_needs"].notna()].head(500)
    for need in ["food", "water", "shelter", "health"]:
        flag = answered[f"priority_needs/{need}"].astype(float).astype(int)
        assert (flag == answered["priority_needs"].str.split().apply(lambda s: int(need in s))).all()
