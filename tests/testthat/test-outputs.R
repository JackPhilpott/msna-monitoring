library(testthat)
# Checks on the pipeline's outputs. Skipped until the pipeline has run.
source(here::here("R", "functions.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

cfg <- read_config()
have_outputs <- file.exists(out_path(cfg, "clean", "validity_report.csv"))

test_that("cleaning catches every planted error with the expected action", {
  skip_if_not(have_outputs, "run the pipeline first")
  perf <- read_csv(out_path(cfg, "clean", "check_performance.csv"), show_col_types = FALSE)
  expect_equal(nrow(perf), 14)
  expect_equal(perf$caught, perf$planted)
  expect_equal(perf$right_action, perf$planted)
})

test_that("the validity gate has no failures", {
  skip_if_not(have_outputs, "run the pipeline first")
  report <- read_csv(out_path(cfg, "clean", "validity_report.csv"), show_col_types = FALSE)
  expect_false(any(report$status == "FAIL"))
})

test_that("non-consent submissions carry no answers, apart from the planted errors", {
  skip_if_not(have_outputs, "run the pipeline first")
  raw <- read_submissions(cfg)
  key <- read_csv(out_path(cfg, "raw", "answer_key.csv"), show_col_types = FALSE)
  declined <- raw |> filter(consent == "no", !`_uuid` %in% key$`_uuid`)
  expect_gt(nrow(declined), 0)
  expect_true(all(is.na(declined$hh_size)))
})

dash <- function(...) here::here(cfg$outputs$dashboard_data, ...)
have_dashboard <- file.exists(dash("submissions.rds"))

test_that("the dashboard's submissions match the cleaning outputs", {
  skip_if_not(have_dashboard, "run step 5 first")
  subs <- readRDS(dash("submissions.rds"))
  deleted <- read_csv(out_path(cfg, "clean", "deletion_log.csv"), show_col_types = FALSE)
  clean <- list.files(out_path(cfg, "clean"), "^clean_data_.*\\.csv\\.gz$", full.names = TRUE) |>
    lapply(read_csv, col_types = cols(.default = "c")) |> bind_rows()
  expect_equal(nrow(subs), nrow(read_submissions(cfg)))
  expect_equal(sum(subs$deletion_status %in% "confirmed"), nrow(deleted))
  # every kept interview counts as achieved: completed, and not a confirmed deletion
  expect_equal(sum(subs$interview_outcome == "completed" & !subs$deletion_status %in% "confirmed"), nrow(clean))
  # only the planted wrong cluster codes fail to match a sampled cluster
  expect_equal(sum(is.na(subs$matched_cluster_id)), sum(subs$flag_not_in_sample))
  expect_true(all(grepl("^partner_[a-t]$", subs$org_id)))
})

test_that("the dashboard's frame, targets and boundaries hold together", {
  skip_if_not(have_dashboard, "run step 5 first")
  strata <- read_csv(dash("strata.csv"), show_col_types = FALSE)
  access <- read_csv(dash("accessibility_strata.csv"), show_col_types = FALSE)
  clusters <- read_csv(dash("clusters.csv"), show_col_types = FALSE)
  partners <- read_csv(dash("partner_lga_assignment.csv"), show_col_types = FALSE)
  wards <- sf::st_read(dash("boundaries", "wards.geojson"), quiet = TRUE)

  targets <- inner_join(strata, access, by = "strata_id")
  expect_equal(nrow(targets), nrow(strata))
  expect_true(all(targets$target_revised <= targets$target_sample, na.rm = TRUE))
  expect_true(all(is.na(targets$target_revised) == (targets$pct_pop_remaining == 0)))
  expect_true(all(strata$adm2_pcode %in% partners$adm2_pcode))
  expect_true(all(paste(clusters$adm2_pcode, clusters$adm3_name) %in% paste(wards$adm2_pcode, wards$wardname)))
  sizes <- file.size(list.files(dash(), recursive = TRUE, full.names = TRUE))
  expect_true(all(sizes < 2 * 2^20))
})
