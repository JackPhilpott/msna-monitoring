# Step 2: data cleaning.
#
# Runs 14 checks on the raw export. Each finding goes into the cleaning log as
# one of three actions: remove (the interview goes to the deletion log),
# change (a value is corrected, with old and new values recorded) or flag (sent
# to the partner for follow-up; the value stays). The clean dataset gets the
# standard food-security composites. Because the errors were planted, the
# step then scores itself against the answer key.
#
# Inputs: data/synthetic/raw/, data/synthetic/from_sampling/
# Outputs: data/synthetic/clean/{clean_data_<state>.csv.gz, cleaning_log.csv,
#          deletion_log.csv, enumerator_flags.csv, check_performance.csv}

source(here::here("R", "functions.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(lubridate)
})

cfg <- read_config()
ck <- cfg$checks
num <- function(x) suppressWarnings(as.numeric(x))

raw <- read_submissions(cfg) |>
  mutate(.row = row_number(), `_index` = as.integer(`_index`),
         duration_min = as.numeric(difftime(ymd_hms(end), ymd_hms(start), units = "mins"))) |>
  arrange(`_index`)
clusters <- read_csv(out_path(cfg, "from_sampling", "clusters_after_resampling.csv"), show_col_types = FALSE)
households <- list.files(out_path(cfg, "from_sampling", "households"), full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  list_rbind() |>
  select(household_id, hh_lon = lon, hh_lat = lat)
raw <- raw |>
  left_join(select(clusters, cluster_id, cluster_status = status, stratum_id, pop_type, adm1_name, adm2_name),
            by = "cluster_id") |>
  left_join(households, by = "household_id") |>
  mutate(gps_distance_m = haversine_m(num(`_gps_longitude`), num(`_gps_latitude`), hh_lon, hh_lat))
consented <- raw$consent == "yes"

log_rows <- function(d, check_id, check, question, issue, action, old = NA, new = NA) {
  if (!nrow(d)) return(NULL)
  tibble(.row = d$.row, `_uuid` = d$`_uuid`, `_index` = d$`_index`, enumerator_id = d$enumerator_id,
         partner = d$partner, check_id = check_id, check = check, question = question,
         old_value = as.character(old), new_value = as.character(new), action = action, issue = issue)
}

checks <- list()
r <- raw
checks$C01 <- log_rows(filter(r, consent == "no"), "C01", "no_consent", "consent",
                       "Respondent did not consent", "remove", "no")
checks$C02 <- log_rows(filter(r, duplicated(`_uuid`)), "C02", "duplicate_submission", "_uuid",
                       "Same submission uploaded more than once", "remove")
dup_hh <- r |> filter(consented) |> arrange(start) |> filter(duplicated(household_id) & !duplicated(`_uuid`))
checks$C03 <- log_rows(dup_hh, "C03", "duplicate_household", "household_id",
                       "Household already interviewed in an earlier submission", "remove", dup_hh$household_id)
short <- filter(r, consented, duration_min < ck$min_duration_min)
checks$C04 <- log_rows(short, "C04", "too_short", "duration", sprintf("Interview took %.0f minutes", short$duration_min),
                       "remove", round(short$duration_min, 1))
checks$C05 <- log_rows(filter(r, is.na(cluster_status)), "C05", "cluster_not_in_sample", "cluster_id",
                       "Cluster code not in the sample", "remove", filter(r, is.na(cluster_status))$cluster_id)
checks$C06 <- log_rows(filter(r, cluster_status %in% "inaccessible"), "C06", "cluster_inaccessible", "cluster_id",
                       "Cluster had been reported inaccessible", "remove",
                       filter(r, cluster_status %in% "inaccessible")$cluster_id)
far <- filter(r, consented, gps_distance_m > ck$max_gps_distance_m)
checks$C07 <- log_rows(far, "C07", "gps_far", "gps", sprintf("GPS point %.1f km from the household", far$gps_distance_m / 1000),
                       "flag", round(far$gps_distance_m))
fcs_cols <- paste0("fcs_", names(FCS_WEIGHTS))
fcs_mat <- sapply(fcs_cols, function(c) num(r[[c]]))
flat <- consented & apply(fcs_mat, 1, function(x) all(!is.na(x)) && length(unique(x)) == 1)
checks$C08 <- log_rows(r[flat, ], "C08", "fcs_flat", "fcs_*", "Every food group given the same number of days",
                       "flag", fcs_mat[flat, 1])
r_fcs <- fcs_score(r); r_hhs <- hhs_score(r)
contra <- consented & !is.na(r_fcs) & r_fcs > 35 & !is.na(r_hhs) & r_hhs >= 4
checks$C09 <- log_rows(r[contra, ], "C09", "fcs_hhs_contradiction", "fcs_score / hhs_score",
                       "Acceptable food consumption with severe hunger", "flag",
                       sprintf("FCS %.1f, HHS %d", r_fcs[contra], r_hhs[contra]))
big <- filter(r, consented, num(hh_size) > ck$max_hh_size)
checks$C10 <- log_rows(big, "C10", "hh_size_outlier", "hh_size", "Household size above the plausible maximum",
                       "change", big$hh_size, NA)
kids <- filter(r, consented, num(n_children_u5) + num(n_children_school_age) > num(hh_size))
checks$C11 <- log_rows(kids, "C11", "children_exceed_hh", "n_children_u5 / n_children_school_age",
                       "More children than household members", "flag",
                       sprintf("%s + %s children, household of %s", kids$n_children_u5, kids$n_children_school_age, kids$hh_size))
att <- filter(r, consented, num(n_children_attending) > num(n_children_school_age))
checks$C12 <- log_rows(att, "C12", "attending_exceed_school_age", "n_children_attending",
                       "More children attending school than of school age", "flag", att$n_children_attending)

# "Other" answers that match an existing option are recoded to it
recode_rules <- c("casual|labour|work on" = "daily_labour", "goat|sheep|cattle|livestock|rearing" = "livestock",
                  "market|selling|trade|shop" = "petty_trade", "farm|crop|land" = "crops")
recode_to <- function(text) {
  hit <- vapply(names(recode_rules), function(p) grepl(p, tolower(text)), logical(length(text)))
  hit <- matrix(hit, nrow = length(text))
  ifelse(rowSums(hit) > 0, unname(recode_rules)[max.col(hit, ties.method = "first")], NA_character_)
}
oth <- filter(r, consented, income_source == "other") |> mutate(to = recode_to(income_source_other)) |>
  filter(!is.na(to))
checks$C13 <- log_rows(oth, "C13", "other_recoded", "income_source",
                       sprintf("'Other' answer '%s' matches an existing option", oth$income_source_other),
                       "change", "other", oth$to)

# Enumerator-level patterns: interviews much faster than everyone else's, or answers that repeat
pattern_cols <- c(fcs_cols, grep("^(rcsi_|hhs_|lcsi_)", names(r), value = TRUE), "water_source", "sanitation_facility",
                  "shelter_type", "income_source", "priority_needs")
enum <- r |>
  filter(consented) |>
  mutate(pattern = do.call(paste, c(across(all_of(pattern_cols)), sep = "|"))) |>
  group_by(enumerator_id, partner) |>
  summarise(interviews = n(), median_duration = median(duration_min),
            identical_share = mean(duplicated(pattern) | duplicated(pattern, fromLast = TRUE)), .groups = "drop") |>
  mutate(overall_median = median(r$duration_min[consented]),
         flag_fast = interviews >= ck$enumerator_min_interviews &
           median_duration < ck$enumerator_duration_ratio * overall_median,
         flag_identical = interviews >= ck$enumerator_min_interviews &
           identical_share > ck$enumerator_max_identical_share,
         flagged = flag_fast | flag_identical)
bad_enum <- filter(enum, flagged)
enum_rows <- filter(r, consented, enumerator_id %in% bad_enum$enumerator_id)
checks$C14 <- log_rows(enum_rows, "C14", "enumerator_pattern", "enumerator_id",
                       "Enumerator's interviews are much faster than others' or repeat the same answers",
                       "flag", enum_rows$enumerator_id)

cleaning_log <- bind_rows(checks) |> arrange(`_index`, check_id)

# Deletions: one entry per removed row, with the first reason that applies
deletion_log <- cleaning_log |>
  filter(action == "remove") |>
  arrange(.row, check_id) |>
  distinct(.row, .keep_all = TRUE) |>
  select(.row, `_uuid`, `_index`, enumerator_id, partner, check_id, reason = issue)

clean <- raw |> filter(!.row %in% deletion_log$.row)
changes <- cleaning_log |> filter(action == "change", .row %in% clean$.row)
for (i in seq_len(nrow(changes))) {
  ch <- changes[i, ]
  col <- if (ch$check_id == "C13") "income_source" else ch$question
  clean[[col]][clean$.row == ch$.row] <- ch$new_value
  if (ch$check_id == "C13") clean$income_source_other[clean$.row == ch$.row] <- NA
}
clean <- clean |>
  mutate(fcs_score = fcs_score(clean), fcs_category = as.character(fcs_category(fcs_score)),
         rcsi_score = rcsi_score(clean), hhs_score = hhs_score(clean),
         hhs_category = as.character(hhs_category(hhs_score)), lcsi_category = lcsi_category(clean),
         duration_min = round(duration_min, 1), gps_distance_m = round(gps_distance_m)) |>
  select(-.row, -hh_lon, -hh_lat, -cluster_status)

# Score the cleaning against the planted errors
key <- read_csv(out_path(cfg, "raw", "answer_key.csv"), show_col_types = FALSE)
expected <- c(too_short = "C04", duplicate_exact = "C02", duplicate_near = "C03", gps_far = "C07",
              consent_no_continued = "C01", fcs_flat = "C08", fcs_hhs_contradiction = "C09",
              hh_size_outlier = "C10", children_exceed_hh = "C11", attending_exceed_school_age = "C12",
              wrong_cluster = "C05", dropped_cluster = "C06", other_to_recode = "C13", problem_enumerator = "C14")
action_word <- c(delete = "remove", change = "change", flag = "flag")
caught <- key |>
  mutate(check_id = unname(expected[error_type])) |>
  left_join(distinct(cleaning_log, `_uuid`, check_id, action), by = c("_uuid", "check_id")) |>
  group_by(error_type, check_id, expected_action) |>
  summarise(planted = n(), caught = sum(!is.na(action)),
            right_action = sum(action == action_word[expected_action], na.rm = TRUE), .groups = "drop") |>
  mutate(recall = round(caught / planted, 3))
routine <- cleaning_log$`_uuid`[cleaning_log$check_id == "C01" & !cleaning_log$`_uuid` %in% key$`_uuid`]
per_check <- cleaning_log |>
  group_by(check_id, check, action) |>
  summarise(flagged = n_distinct(.row), planted_hits = sum(`_uuid` %in% key$`_uuid`),
            routine = sum(check_id == "C01" & `_uuid` %in% routine), .groups = "drop") |>
  mutate(false_flags = flagged - planted_hits - routine)
performance <- caught |> left_join(select(per_check, check_id, check, flagged, false_flags), by = "check_id") |>
  arrange(check_id)

write_synthetic(select(cleaning_log, -.row), "clean/cleaning_log.csv", cfg)
write_synthetic(select(deletion_log, -.row), "clean/deletion_log.csv", cfg)
write_synthetic(enum, "clean/enumerator_flags.csv", cfg)
write_synthetic(performance, "clean/check_performance.csv", cfg)
for (state in sort(unique(clean$adm1_name))) {
  write_synthetic(filter(clean, adm1_name == state), sprintf("clean/clean_data_%s.csv.gz", tolower(state)), cfg)
}

message(sprintf("Raw: %s submissions | removed %s | kept %s | values changed %s | flagged for follow-up %s",
                format(nrow(raw), big.mark = ","), format(nrow(deletion_log), big.mark = ","),
                format(nrow(clean), big.mark = ","), format(nrow(changes), big.mark = ","),
                format(n_distinct(cleaning_log$.row[cleaning_log$action == "flag"]), big.mark = ",")))
message(sprintf("Planted errors caught: %s of %s (%.1f%%), right action on %s; false flags: %s; enumerators flagged: %d",
                format(sum(caught$caught), big.mark = ","), format(sum(caught$planted), big.mark = ","),
                100 * sum(caught$caught) / sum(caught$planted), format(sum(caught$right_action), big.mark = ","),
                sum(per_check$false_flags), nrow(bad_enum)))
print(as.data.frame(select(performance, check_id, error_type, planted, caught, recall, flagged, false_flags)),
      row.names = FALSE)
