library(testthat)
# Server-side test of the Shiny app: every page's outputs render without error,
# and the sidebar filters narrow what they show. Skipped until step 5 has run.

app_dir <- here::here("dashboard_app")
skip_if_not(file.exists(file.path(app_dir, "data", "submissions.rds")), "run step 5 first")

outputs <- c(
  "home-glance", "home-snapshot", "home-priorities",
  "progress-kpi_achieved", "progress-kpi_still_needed", "progress-kpi_pct", "progress-kpi_collected",
  "progress-kpi_days_remaining", "progress-kpi_days_required", "progress-trend_plot", "progress-region_plot",
  "progress-partner_bar_pct", "progress-partner_bar_abs", "progress-partner_table",
  "map-map", "table-table",
  "quality-kpi_flagged", "quality-flag_plot", "quality-flagged_table",
  "partner_report-kpi_target", "partner_report-kpi_achieved", "partner_report-lga_table",
  "export-subs_count", "export-preview",
  "enumerator-kpi_n_enum", "enumerator-scatter", "enumerator-top_bar", "enumerator-drill_trend", "enumerator-table",
  "integrity-kpi_gps_dup", "integrity-kpi_off_hours", "integrity-kpi_whipple", "integrity-hour_hist",
  "integrity-age_hist", "integrity-gps_dup_table", "integrity-overmax_table",
  "representativeness-kpi_achieved_hh", "representativeness-setting_bar", "representativeness-pyramid",
  "representativeness-hh_size_table"
)

test_that("every dashboard output renders, with and without filters", {
  old <- setwd(app_dir)
  on.exit(setwd(old))
  app <- shiny::shinyAppDir(".")
  shiny::testServer(app, {
    end <- max(submissions_raw$submission_date)
    session$setInputs(target_basis = "original", f_daterange = c(FIELDING_START, end),
                      `partner_report-report_partner` = "partner_e", `enumerator-drill_enum` = "enum_E_01")
    for (o in outputs) expect_no_error(output[[o]], message = o)
    all_achieved <- sum(filtered_stratum()$achieved_n)
    expect_equal(nrow(filtered_stratum()), nrow(strata_frame))

    # one state, one population group, the revised target, and the first three weeks only
    session$setInputs(f_state = "Borno", f_poptype = "idp", target_basis = "revised",
                      f_daterange = c(FIELDING_START, FIELDING_START + 20))
    for (o in outputs) expect_no_error(output[[o]], message = o)
    expect_true(all(filtered_stratum()$adm1_name == "Borno"))
    expect_true(all(filtered_stratum()$pop_type == "idp"))
    expect_true(all(filtered_subs()$submission_date <= FIELDING_START + 20))
    expect_lt(sum(filtered_stratum()$achieved_n), all_achieved)
    expect_true(all(filtered_stratum()$target_active == filtered_stratum()$target_sample_current))
  })
})

test_that("the partner reports build as Excel and PDF", {
  old <- setwd(app_dir)
  on.exit(setwd(old))
  suppressWarnings(source("global.R", local = TRUE))
  xlsx <- tempfile(fileext = ".xlsx")
  pdf <- tempfile(fileext = ".pdf")
  build_partner_excel("partner_a", xlsx, "revised")
  build_partner_pdf("partner_a", pdf)
  expect_gt(file.size(xlsx), 5000)
  expect_gt(file.size(pdf), 3000)
  expect_equal(openxlsx::read.xlsx(xlsx, "Summary", colNames = FALSE)[2, 2], "Synthetic data – not real findings")
})
