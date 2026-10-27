# Step 3: validity checks, the gate before the data goes to analysis.
#
# Each check states something that must be true across the stages: of the
# sample from msna-sampling, the raw export, the logs and the clean data. A
# FAIL stops the pipeline; a WARN is reported for follow-up but does not.
# Modelled on the real project's validity-check catalogue.
#
# Inputs: data/synthetic/{from_sampling,raw,clean}/
# Outputs: data/synthetic/clean/{validity_report,achieved_by_stratum}.csv

source(here::here("R", "functions.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

cfg <- read_config()
CLUSTER_SIZE <- 6  # households interviewed per cluster, as in msna-sampling
read_dir <- function(dir, pattern) {
  list.files(out_path(cfg, dir), pattern = pattern, full.names = TRUE) |>
    map(read_csv, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    list_rbind()
}
raw <- read_submissions(cfg)
clean <- read_dir("clean", "^clean_data_.*\\.csv\\.gz$")
deleted <- read_csv(out_path(cfg, "clean", "deletion_log.csv"), col_types = cols(.default = "c"))
log <- read_csv(out_path(cfg, "clean", "cleaning_log.csv"), col_types = cols(.default = "c"))
clusters <- read_csv(out_path(cfg, "from_sampling", "clusters_after_resampling.csv"), show_col_types = FALSE)
strata <- read_csv(out_path(cfg, "from_sampling", "strata.csv"), show_col_types = FALSE)
lgas <- read_csv(out_path(cfg, "from_sampling", "lgas.csv"), show_col_types = FALSE)
households <- read_dir("from_sampling/households", "\\.csv$")

results <- list()
check <- function(id, description, ok, detail, warn_only = FALSE) {
  status <- if (ok) "PASS" else if (warn_only) "WARN" else "FAIL"
  results[[id]] <<- tibble(check_id = id, description = description, status = status, detail = detail)
}
fmt <- function(x) format(x, big.mark = ",")

check("V01", "Every raw submission is either in the clean data or in the deletion log",
      nrow(raw) == nrow(clean) + nrow(deleted),
      sprintf("%s raw = %s clean + %s deleted", fmt(nrow(raw)), fmt(nrow(clean)), fmt(nrow(deleted))))
check("V02", "No submission or household appears twice in the clean data",
      !anyDuplicated(clean$`_uuid`) && !anyDuplicated(clean$household_id),
      sprintf("%d duplicate submissions, %d duplicate households",
              sum(duplicated(clean$`_uuid`)), sum(duplicated(clean$household_id))))
active <- filter(clusters, status == "active")
check("V03", "Every clean interview is in an active cluster, with a household from that cluster's list",
      all(clean$cluster_id %in% active$cluster_id) &&
        all(paste(clean$cluster_id, clean$household_id) %in% paste(households$cluster_id, households$household_id)),
      sprintf("%d interviews outside active clusters, %d households not on their cluster's list",
              sum(!clean$cluster_id %in% active$cluster_id),
              sum(!paste(clean$cluster_id, clean$household_id) %in% paste(households$cluster_id, households$household_id))))
inacc <- clusters$cluster_id[clusters$status == "inaccessible"]
check("V04", "No clean interview is in a cluster reported inaccessible",
      !any(clean$cluster_id %in% inacc), sprintf("%d interviews", sum(clean$cluster_id %in% inacc)))
# Clusters may go past the cluster size where a partner made up for clusters lost to access,
# but never past the households listed for them (primary and reserve)
per_cluster <- count(clean, cluster_id) |> left_join(count(households, cluster_id, name = "listed"), by = "cluster_id")
check("V05", "No cluster has more interviews than households on its list",
      all(per_cluster$n <= per_cluster$listed, na.rm = TRUE),
      sprintf("largest cluster: %d interviews; %d clusters topped up past %d to make up for inaccessible ones",
              max(per_cluster$n), sum(per_cluster$n > CLUSTER_SIZE), CLUSTER_SIZE))
assigned <- lgas |> filter(surveyed) |> mutate(partner_code = tolower(gsub(" ", "_", partner)))
lga_of <- clusters |> select(cluster_id, adm2_pcode)
mismatch <- clean |> left_join(lga_of, by = "cluster_id") |>
  left_join(select(assigned, adm2_pcode, partner_code), by = "adm2_pcode") |>
  filter(partner != partner_code)
check("V06", "Every interview was done by the partner assigned to its LGA", nrow(mismatch) == 0,
      sprintf("%d interviews by another partner", nrow(mismatch)))
num_cols <- c(grep("^fcs_(cereals|pulses|dairy|meat|vegetables|fruit|oil|sugar|condiments)$", names(clean), value = TRUE),
              grep("^rcsi_(?!score)", names(clean), value = TRUE, perl = TRUE))
out_of_range <- sum(sapply(num_cols, function(c) sum(!is.na(clean[[c]]) & !as.numeric(clean[[c]]) %in% 0:7)))
too_big <- sum(as.numeric(clean$hh_size) > cfg$checks$max_hh_size, na.rm = TRUE)
check("V07", "Clean values are within the questionnaire's ranges (days 0 to 7, household size)",
      out_of_range == 0 && too_big == 0,
      sprintf("%d food or coping values outside 0 to 7, %d household sizes above %d", out_of_range, too_big,
              cfg$checks$max_hh_size))
skip_breaks <- sum(!is.na(clean$months_displaced) & clean$displacement_status != "idp") +
  sum(!is.na(clean$health_barrier) & clean$health_access %in% "yes")
check("V08", "Skip logic holds: follow-up questions are only answered when they apply", skip_breaks == 0,
      sprintf("%d answers to questions that should have been skipped", skip_breaks))
changes <- filter(log, action == "change")
applied <- changes |>
  mutate(col = ifelse(check_id == "C13", "income_source", question)) |>
  left_join(select(clean, `_uuid`, hh_size, income_source), by = "_uuid") |>
  filter(!is.na(hh_size) | !is.na(income_source)) |>
  mutate(now = ifelse(col == "hh_size", hh_size, income_source),
         ok = ifelse(is.na(new_value), is.na(now), now == new_value))
check("V09", "Every change in the cleaning log is applied in the clean data", all(applied$ok),
      sprintf("%d of %d changes applied", sum(applied$ok), nrow(applied)))

local_sampling <- here::here(cfg$sampling$local_repo, "data", "synthetic")
if (dir.exists(local_sampling)) {
  files <- c(cfg$sampling$files, file.path(cfg$sampling$households_dir, list.files(file.path(local_sampling, cfg$sampling$households_dir))))
  stale <- files[tools::md5sum(file.path(local_sampling, files)) != tools::md5sum(out_path(cfg, "from_sampling", files))]
  check("V10", "The copy of the sample matches msna-sampling's current outputs", length(stale) == 0,
        if (length(stale)) paste("out of date:", paste(stale, collapse = ", ")) else sprintf("%d files identical", length(files)))
} else {
  check("V10", "The copy of the sample matches msna-sampling's current outputs", TRUE,
        "skipped: no local msna-sampling checkout to compare with")
}

achieved <- active |>
  group_by(stratum_id) |>
  summarise(target = n() * CLUSTER_SIZE, .groups = "drop") |>
  left_join(count(clean, stratum_id, name = "achieved"), by = "stratum_id") |>
  left_join(select(strata, stratum_id, pop_type, adm1_name, adm2_name, partner), by = "stratum_id") |>
  mutate(achieved = coalesce(achieved, 0L), pct = round(100 * achieved / target, 1))
low <- filter(achieved, pct < 80)
check("V11", "Every stratum reached at least 80% of its target", nrow(low) == 0,
      sprintf("%d of %d strata below 80%% (lowest %.0f%%)", nrow(low), nrow(achieved), min(achieved$pct)),
      warn_only = TRUE)

report <- bind_rows(results)
write_synthetic(report, "clean/validity_report.csv", cfg)
write_synthetic(arrange(achieved, pct), "clean/achieved_by_stratum.csv", cfg)
for (i in seq_len(nrow(report))) {
  message(sprintf("%s  %-4s  %s\n            %s", report$check_id[i], report$status[i], report$description[i],
                  report$detail[i]))
}
if (any(report$status == "FAIL")) {
  message("Validity gate: FAILED - fix the failing checks before analysis")
  quit(status = 1)
}
message(sprintf("Validity gate: passed (%d checks, %d warnings)", nrow(report), sum(report$status == "WARN")))
