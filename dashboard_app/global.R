# Shared data, lookups and progress calculations for the MSNA monitoring dashboard.
#
# A port of the dashboard used on the 2026 northern Nigeria MSNA, running on
# this repo's synthetic data. scripts/05_dashboard_data.R writes everything in
# data/ in the shape the original dashboard read.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(leaflet)
  library(sf)
  library(DT)
  library(plotly)
  library(scales)
  library(openxlsx)
  library(ggplot2)
  library(cowplot)
  library(shinyWidgets)
})

DATA_DIR <- "data"
SYNTHETIC_LABEL <- "Synthetic data – not real findings"
SETTINGS <- yaml::read_yaml(file.path(DATA_DIR, "settings.yml"))
read_data <- function(name, ...) read_csv(file.path(DATA_DIR, name), show_col_types = FALSE, ...)
read_geo <- function(name) st_read(file.path(DATA_DIR, "boundaries", name), quiet = TRUE)

submissions_raw <- readRDS(file.path(DATA_DIR, "submissions.rds"))

FIELDING_START <- as.Date(SETTINGS$collection_start)
FIELDING_PLANNED_END <- as.Date(SETTINGS$planned_end)
COLLECTION_CLOSED_DATE <- max(submissions_raw$submission_date, na.rm = TRUE)
fmt_long_date <- function(d) paste(as.integer(format(d, "%d")), format(d, "%B %Y"))

# Strata (LGA x population group), including those dropped when every cluster became inaccessible
strata_frame <- read_data("strata.csv")
TOTAL_PLANNED_INTERVIEWS <- sum(strata_frame$target_sample[strata_frame$coverage_status == "covered"], na.rm = TRUE)
TOTAL_COVERED_LGAS <- length(unique(strata_frame$adm2_pcode[strata_frame$coverage_status == "covered"]))

cluster_frame <- read_data("clusters.csv", col_types = cols(.default = "c", target_households = "d"))

admin0_sf <- read_geo("admin0.geojson")
admin1_sf <- read_geo("admin1.geojson") %>%
  mutate(in_assessment = adm1_pcode %in% unique(strata_frame$adm1_pcode))
admin2_sf <- read_geo("admin2.geojson") %>%
  filter(adm2_pcode %in% unique(strata_frame$adm2_pcode))
wards_sf <- read_geo("wards.geojson")

# PSU shapes (hexagons for non-IDP clusters, points for IDP sites) with their
# cluster's details. Clusters partners reported inaccessible are drawn grey.
read_psu_layer <- function(name) {
  read_geo(name) %>%
    left_join(cluster_frame, by = "cluster_id") %>%
    mutate(accessible_status = if_else(sample_status == "inaccessible", "Inaccessible", "Accessible"))
}
psu_hexagons_sf <- read_psu_layer("psu_hexagons_non_idp.geojson")
psu_sites_sf <- read_psu_layer("psu_sites_idp.geojson")

ROW_LEVEL_PII_COLUMNS <- c(
  "resp_gender", "resp_age", "resp_hoh_yn", "hoh_gender", "hoh_age", "hh_size",
  "latitude_submitted", "longitude_submitted"
)

# Exports never include respondent details or exact GPS, even synthetic ones
strip_row_level_pii <- function(df) {
  df %>% select(-any_of(ROW_LEVEL_PII_COLUMNS))
}

REGION_LABELS <- c(NE = "North-East (NE)", NW = "North-West (NW)", NC = "North-Central (NC)")

POP_TYPE_COLORS <- c(non_idp = "#2E6F9E", idp = "#D9822B")
POP_TYPE_LABELS <- c(non_idp = "Non-IDP", idp = "IDP")
COMBINED_COLOR <- "#1B2A4A"
UNMATCHED_COLOR <- "#9AA3AF"

STATUS_COLORS <- c("Complete" = "#1E7B4D", "In progress" = "#D99A2B", "Not started" = "#C1443C")
STRATUM_STATUS_COLORS <- c(STATUS_COLORS, "Dropped" = "#7D8791")
CLUSTER_STATUS_COLORS <- c(STATUS_COLORS, "Inaccessible" = "#7D8791")
OVERSAMPLED_BORDER <- "#6C3483"
INACCESSIBLE_BORDER <- "#7D8791"

TARGET_BASIS_ACTIVE_COLOR <- "#2A6FBF"
TARGET_BASIS_INACTIVE_COLOR <- UNMATCHED_COLOR

THEME_NAVBAR_BG <- "#1B2A4A"
THEME_SIDEBAR_BG <- "#3D4B5C"
THEME_SIDEBAR_FG <- "#F4F6F8"

get_ward_choices <- function(lgas) {
  cluster_frame %>%
    filter(adm2_name %in% lgas, !is.na(adm3_name)) %>%
    distinct(adm3_name) %>% arrange(adm3_name) %>% pull(adm3_name)
}

ward_to_lga <- cluster_frame %>%
  filter(!is.na(adm3_name)) %>%
  distinct(adm2_name, adm3_name)

KNOWN_STATE_NAMES <- unique(cluster_frame$adm1_name)
KNOWN_LGA_NAMES <- unique(cluster_frame$adm2_name)
KNOWN_WARD_NAMES <- unique(ward_to_lga$adm3_name)

partner_lga_assignment <- read_data("partner_lga_assignment.csv")
PARTNER_IDS <- sort(unique(c(partner_lga_assignment$org_id, submissions_raw$org_id)))
ORG_LABELS <- setNames(paste("Partner", toupper(sub("^partner_", "", PARTNER_IDS))), PARTNER_IDS)

shared_coverage_adm2 <- partner_lga_assignment %>%
  count(adm2_pcode, name = "n_orgs") %>%
  filter(n_orgs > 1) %>%
  pull(adm2_pcode)
HAS_SHARED_COVERAGE <- length(shared_coverage_adm2) > 0

coverage_orgs_by_adm2 <- split(partner_lga_assignment$org_id, partner_lga_assignment$adm2_pcode)

partner_coverage_label <- function(pc) {
  orgs <- coverage_orgs_by_adm2[[pc]]
  if (is.null(orgs)) "No partner assigned" else paste(unname(ORG_LABELS[orgs]), collapse = ", ")
}

# Ward accessibility as partners reported it, one row per ward and population group
accessibility_ward_df <- read_data("accessibility_wards.csv")
accessibility_sf <- wards_sf %>%
  select(adm2_pcode, wardname) %>%
  inner_join(accessibility_ward_df, by = c("adm2_pcode", "wardname"))

TOTAL_ACCESSIBILITY_PARTNERS <- length(unique(partner_lga_assignment$org_id))
N_ACCESSIBILITY_PARTNERS_REPORTED <- accessibility_ward_df %>%
  filter(status_source == "confirmed_by_partner_report") %>%
  distinct(reporting_partners) %>%
  nrow()

accessibility_strata <- read_data("accessibility_strata.csv")

accessibility_lga_ward_counts <- accessibility_ward_df %>%
  group_by(adm2_pcode) %>%
  summarise(
    n_ward_portions = n_distinct(wardname),
    n_ward_portions_inaccessible = n_distinct(wardname[accessible_status == "Inaccessible"]),
    .groups = "drop"
  )

accessibility_pop_remaining_label <- accessibility_strata %>%
  left_join(strata_frame %>% distinct(strata_id, adm2_pcode), by = "strata_id") %>%
  transmute(
    adm2_pcode,
    label = paste0(
      unname(POP_TYPE_LABELS[pop_type]), ": ", round(pct_pop_remaining), "% (",
      comma(pop_accessible), " of ", comma(pop_design), ")"
    )
  ) %>%
  group_by(adm2_pcode) %>%
  summarise(pop_remaining_label = paste(label, collapse = " | "), .groups = "drop")

accessibility_lga_summary <- accessibility_lga_ward_counts %>%
  left_join(accessibility_pop_remaining_label, by = "adm2_pcode") %>%
  mutate(pop_remaining_label = coalesce(pop_remaining_label, "n/a"))

adm2_name_lookup <- cluster_frame %>%
  distinct(adm1_name, adm2_name, adm2_pcode, adm1_pcode)

filter_base <- strata_frame %>%
  left_join(
    partner_lga_assignment %>% select(adm2_pcode, org_id),
    by = "adm2_pcode",
    relationship = "many-to-many"
  )

partner_adm2 <- split(filter_base$adm2_pcode, filter_base$org_id)
PARTNERS_ASSIGNED <- names(partner_adm2)[lengths(partner_adm2) > 0]

# Choices for one sidebar filter, narrowed by the selections in all the others
compute_filter_choices <- function(dimension, current) {
  df <- filter_base
  if (dimension != "region" && length(current$region) > 0) df <- df %>% filter(region %in% current$region)
  if (dimension != "state" && length(current$state) > 0) df <- df %>% filter(adm1_name %in% current$state)
  if (dimension != "lga" && length(current$lga) > 0) df <- df %>% filter(adm2_name %in% current$lga)
  if (dimension != "poptype" && length(current$poptype) > 0) df <- df %>% filter(pop_type %in% current$poptype)
  if (dimension != "partner" && length(current$partner) > 0) df <- df %>% filter(org_id %in% current$partner)

  switch(dimension,
    region = {
      codes <- sort(unique(df$region))
      setNames(codes, unname(REGION_LABELS[codes]))
    },
    state = sort(unique(df$adm1_name)),
    lga = sort(unique(df$adm2_name)),
    poptype = {
      codes <- sort(unique(df$pop_type))
      setNames(codes, unname(POP_TYPE_LABELS[codes]))
    },
    partner = {
      codes <- unique(df$org_id)
      codes <- codes[order(unname(ORG_LABELS[codes]))]
      setNames(codes, unname(ORG_LABELS[codes]))
    }
  )
}

# Keep what is still valid after the choices change; if nothing is, select everything
smart_selection <- function(current_values, new_choices) {
  still_valid <- intersect(current_values, new_choices)
  if (length(still_valid) == 0) new_choices else still_valid
}

region_choices <- compute_filter_choices("region", list())
state_choices <- compute_filter_choices("state", list())
pop_type_choices <- compute_filter_choices("poptype", list())
org_id_choices <- compute_filter_choices("partner", list())

# Collected = every completed interview. Achieved = completed and not a
# confirmed deletion: what counts toward the sample. A pending flag (open
# follow-up) does not remove an interview from Achieved; only cleaning's
# decision to delete it does.
is_collected <- function(df) {
  df$interview_outcome == "completed"
}

is_achieved <- function(df) {
  df$interview_outcome == "completed" & !is_confirmed_deletion(df)
}

is_confirmed_deletion <- function(df) {
  df$interview_outcome == "completed" & !is.na(df$deletion_status) & df$deletion_status %in% c("confirmed", "contested")
}

cluster_targets <- bind_rows(
  st_drop_geometry(psu_hexagons_sf) %>% select(cluster_id, strata_id, target_households),
  st_drop_geometry(psu_sites_sf) %>% select(cluster_id, strata_id, target_households)
) %>%
  distinct(cluster_id, .keep_all = TRUE) %>%
  mutate(target_households = as.numeric(target_households))

# Revised target: the minimum sample on the population still accessible
strata_target_current <- accessibility_strata %>%
  transmute(strata_id, target_sample_current = target_revised)

TOTAL_PLANNED_INTERVIEWS_CURRENT <- strata_frame %>%
  filter(coverage_status == "covered") %>%
  inner_join(strata_target_current, by = "strata_id") %>%
  pull(target_sample_current) %>%
  sum(na.rm = TRUE)

# Progress per stratum against the chosen target. Achieved counts every
# interview that counts, including any past the stratum's own target;
# credited_achieved_n caps it at the target, so surplus in one stratum never
# offsets a shortfall in another when strata are summed.
compute_progress_by_stratum <- function(subs, target_basis = c("original", "revised")) {
  target_basis <- match.arg(target_basis)
  completed_matched <- subs %>% filter(is_achieved(.))

  achieved <- completed_matched %>%
    filter(!is.na(matched_cluster_id)) %>%
    count(matched_cluster_id, matched_strata_id, name = "cluster_achieved_n") %>%
    group_by(matched_strata_id) %>%
    summarise(achieved_n = sum(cluster_achieved_n), .groups = "drop")

  achieved_reserve <- completed_matched %>%
    filter(matched_status == "reserve") %>%
    count(matched_strata_id, name = "achieved_reserve_n")
  achieved <- achieved %>% left_join(achieved_reserve, by = "matched_strata_id")

  collected <- subs %>% filter(is_collected(.)) %>% count(matched_strata_id, name = "collected_n")

  confirmed_deletion <- subs %>% filter(is_confirmed_deletion(.)) %>%
    count(matched_strata_id, name = "confirmed_deletion_n")

  pending_flagged <- subs %>%
    filter(interview_outcome == "completed", !is.na(deletion_status),
           !deletion_status %in% c("confirmed", "contested")) %>%
    count(matched_strata_id, name = "pending_deletion_n")

  strata_frame %>%
    left_join(achieved, by = c("strata_id" = "matched_strata_id")) %>%
    left_join(collected, by = c("strata_id" = "matched_strata_id")) %>%
    left_join(confirmed_deletion, by = c("strata_id" = "matched_strata_id")) %>%
    left_join(pending_flagged, by = c("strata_id" = "matched_strata_id")) %>%
    left_join(strata_target_current, by = "strata_id") %>%
    mutate(
      achieved_n = coalesce(achieved_n, 0L),
      collected_n = coalesce(collected_n, 0L),
      confirmed_deletion_n = coalesce(confirmed_deletion_n, 0L),
      oversampling_surplus_n = pmax(collected_n - achieved_n - confirmed_deletion_n, 0L),
      pending_deletion_n = coalesce(pending_deletion_n, 0L),
      achieved_reserve_n = coalesce(achieved_reserve_n, 0L),
      pct_reserve_used = ifelse(achieved_n > 0, achieved_reserve_n / achieved_n, NA_real_),

      target_not_computable = is.na(target_sample_current),
      target_sample_current = coalesce(target_sample_current, 0),

      target_active = if (target_basis == "revised") target_sample_current else target_sample,
      target_active_label = if (target_basis == "revised") "Revised Target" else "Original Target",

      credited_achieved_n = pmin(achieved_n, pmax(target_active, 0)),
      remaining_n = pmax(target_active - achieved_n, 0),

      pct_achieved = ifelse(target_active > 0, credited_achieved_n / target_active, NA_real_),

      status = case_when(
        coverage_status == "excluded" | target_not_computable ~ "Dropped",
        target_active <= 0 | remaining_n <= 0 ~ "Complete",
        achieved_n > 0 ~ "In progress",
        TRUE ~ "Not started"
      )
    )
}

progress_by_stratum <- compute_progress_by_stratum(submissions_raw)

compute_cluster_progress <- function(subs) {
  scoped <- subs %>% filter(!is.na(matched_cluster_id))
  achieved <- scoped %>% filter(is_achieved(.)) %>% count(matched_cluster_id, name = "achieved_n")
  collected <- scoped %>% filter(is_collected(.)) %>% count(matched_cluster_id, name = "collected_n")
  confirmed_deletion <- scoped %>% filter(is_confirmed_deletion(.)) %>% count(matched_cluster_id, name = "confirmed_deletion_n")

  pending_flagged <- scoped %>%
    filter(interview_outcome == "completed", !is.na(deletion_status),
           !deletion_status %in% c("confirmed", "contested")) %>%
    count(matched_cluster_id, name = "pending_deletion_n")

  cluster_targets %>%
    select(cluster_id) %>%
    left_join(achieved, by = c("cluster_id" = "matched_cluster_id")) %>%
    left_join(collected, by = c("cluster_id" = "matched_cluster_id")) %>%
    left_join(confirmed_deletion, by = c("cluster_id" = "matched_cluster_id")) %>%
    left_join(pending_flagged, by = c("cluster_id" = "matched_cluster_id")) %>%
    mutate(
      achieved_n = coalesce(achieved_n, 0L),
      collected_n = coalesce(collected_n, 0L),
      confirmed_deletion_n = coalesce(confirmed_deletion_n, 0L),
      oversampling_surplus_n = pmax(collected_n - achieved_n - confirmed_deletion_n, 0L),
      pending_deletion_n = coalesce(pending_deletion_n, 0L)
    )
}

compute_oversampled_clusters <- function(subs) {
  achieved_flag <- is_achieved(subs)

  cluster_target <- bind_rows(
    st_drop_geometry(psu_hexagons_sf) %>% select(cluster_id, adm1_name, adm2_name, adm2_pcode, pop_type, target_households),
    st_drop_geometry(psu_sites_sf) %>% select(cluster_id, adm1_name, adm2_name, adm2_pcode, pop_type, target_households)
  ) %>%
    distinct(cluster_id, .keep_all = TRUE) %>%
    mutate(target_households = as.numeric(target_households))

  achieved_by_cluster <- subs[achieved_flag, ] %>%
    group_by(matched_cluster_id) %>%
    summarise(achieved_n = n(), submitting_org_ids = paste(sort(unique(org_id)), collapse = ", "), .groups = "drop")

  cluster_target %>%
    left_join(achieved_by_cluster, by = c("cluster_id" = "matched_cluster_id")) %>%
    mutate(achieved_n = coalesce(achieved_n, 0L), submitting_org_ids = coalesce(submitting_org_ids, "")) %>%
    filter(target_households > 0, achieved_n > target_households) %>%
    mutate(surplus = achieved_n - target_households, pct_over_target = achieved_n / target_households - 1) %>%
    arrange(desc(surplus))
}

oversampled_clusters <- compute_oversampled_clusters(submissions_raw)

PARTNERS_NOT_STARTED <- setdiff(PARTNERS_ASSIGNED, submissions_raw$org_id)

today_for_pace <- max(submissions_raw$submission_date, na.rm = TRUE)

shared_with_label <- function(adm2_pcodes, org) {
  others <- setdiff(unique(unlist(coverage_orgs_by_adm2[adm2_pcodes])), org)
  if (length(others) == 0) "" else paste(unname(ORG_LABELS[others]), collapse = ", ")
}

# One row per partner: progress across its LGAs, its own pace, and whether it
# is on course to finish by the planned end of collection
build_partner_progress_summary <- function(target_basis = c("original", "revised")) {
  target_basis <- match.arg(target_basis)
  progress <- compute_progress_by_stratum(submissions_raw, target_basis)

  own_achieved_by_stratum <- submissions_raw[is_achieved(submissions_raw), ] %>%
    count(org_id, matched_strata_id, name = "own_achieved_n")

  lapply(PARTNERS_ASSIGNED, function(org) {
    my_adm2 <- partner_adm2[[org]]
    own_org <- own_achieved_by_stratum %>%
      filter(org_id == org) %>%
      select(matched_strata_id, own_achieved_n)
    strata_mine <- progress %>%
      filter(adm2_pcode %in% my_adm2) %>%
      left_join(own_org, by = c("strata_id" = "matched_strata_id")) %>%
      mutate(own_achieved_n = coalesce(as.numeric(own_achieved_n), 0))

    totals <- strata_mine %>%
      mutate(across(
        c(target_sample, target_sample_current, achieved_n, collected_n, confirmed_deletion_n,
          pending_deletion_n, oversampling_surplus_n, credited_achieved_n, remaining_n, own_achieved_n),
        ~ ifelse(status == "Dropped", 0, .)
      )) %>%
      mutate(target_active = if (target_basis == "revised") target_sample_current else target_sample,
             own_credited_n = pmin(own_achieved_n, credited_achieved_n)) %>%
      summarise(target_sample = sum(target_sample, na.rm = TRUE),
                credited_own_n = sum(own_credited_n, na.rm = TRUE),
                target_sample_current = sum(target_sample_current, na.rm = TRUE),
                target_active = sum(target_active, na.rm = TRUE),
                achieved_n = sum(achieved_n, na.rm = TRUE),
                credited_achieved_n = sum(credited_achieved_n, na.rm = TRUE),
                remaining_n = sum(remaining_n, na.rm = TRUE),
                collected_n = sum(collected_n, na.rm = TRUE),
                confirmed_deletion_n = sum(confirmed_deletion_n, na.rm = TRUE),
                pending_deletion_n = sum(pending_deletion_n, na.rm = TRUE),
                oversampling_surplus_n = sum(oversampling_surplus_n, na.rm = TRUE))

    start_date <- suppressWarnings(min(submissions_raw$submission_date[submissions_raw$org_id == org], na.rm = TRUE))
    has_started <- is.finite(start_date)
    days_active <- if (has_started) as.numeric(today_for_pace - start_date) + 1 else NA_real_

    current_pace <- if (has_started && days_active > 0) totals$credited_own_n / days_active else NA_real_
    remaining <- totals$remaining_n

    days_left_to_deadline <- as.numeric(FIELDING_PLANNED_END - today_for_pace) + 1
    required_pace <- if (days_left_to_deadline > 0) remaining / days_left_to_deadline else NA_real_
    projected_finish <- if (!is.na(current_pace) && current_pace > 0 && remaining > 0) {
      today_for_pace + ceiling(remaining / current_pace)
    } else {
      as.Date(NA)
    }

    status <- case_when(
      totals$target_active <= 0 ~ "Complete",
      remaining <= 0 ~ "Complete",
      !has_started ~ "Not started",
      is.na(current_pace) || current_pace <= 0 ~ "Behind pace",
      projected_finish <= FIELDING_PLANNED_END ~ "On pace",
      TRUE ~ "Behind pace"
    )

    tibble(
      org_id = org, partner_label = unname(ORG_LABELS[org]),
      target_sample = totals$target_sample, target_sample_current = totals$target_sample_current,
      target_active = totals$target_active,
      achieved_n = totals$achieved_n, collected_n = totals$collected_n,
      confirmed_deletion_n = totals$confirmed_deletion_n, pending_deletion_n = totals$pending_deletion_n,
      oversampling_surplus_n = totals$oversampling_surplus_n,
      credited_achieved_n = totals$credited_achieved_n, remaining_n = totals$remaining_n,
      credited_own_n = totals$credited_own_n,
      pct_achieved = ifelse(totals$target_active > 0, totals$credited_achieved_n / totals$target_active, NA_real_),
      shared_with = shared_with_label(my_adm2, org),
      start_date = if (has_started) start_date else as.Date(NA),
      days_active = days_active,
      current_daily_pace = current_pace,
      required_daily_pace = required_pace,
      projected_finish_date = projected_finish, status = status
    )
  }) %>%
    dplyr::bind_rows() %>%
    arrange(pct_achieved)
}

partner_progress_summary <- build_partner_progress_summary("original")

DATA_AS_OF_DATE <- suppressWarnings(max(submissions_raw$submission_date, na.rm = TRUE))
AS_OF_CAPTION <- paste0("Final data, collection closed ", format(COLLECTION_CLOSED_DATE, "%d %b %Y"), " · ", SYNTHETIC_LABEL)

partner_progress_by_lga <- function(org_id_val, target_basis = c("original", "revised")) {
  target_basis <- match.arg(target_basis)
  my_adm2 <- partner_adm2[[org_id_val]]
  if (is.null(my_adm2)) my_adm2 <- character(0)

  progress <- compute_progress_by_stratum(submissions_raw, target_basis)

  own_org <- submissions_raw[is_achieved(submissions_raw) & submissions_raw$org_id == org_id_val, ] %>%
    count(matched_strata_id, name = "own_achieved_n")

  progress %>%
    filter(adm2_pcode %in% my_adm2) %>%
    left_join(own_org, by = c("strata_id" = "matched_strata_id")) %>%
    mutate(own_achieved_n = coalesce(as.numeric(own_achieved_n), 0)) %>%
    mutate(all_dropped_lga = status == "Dropped") %>%
    mutate(across(
      c(target_sample, target_sample_current, achieved_n, collected_n, confirmed_deletion_n,
        pending_deletion_n, oversampling_surplus_n, credited_achieved_n, remaining_n, own_achieved_n),
      ~ ifelse(status == "Dropped", 0, .)
    )) %>%
    mutate(target_active = if (target_basis == "revised") target_sample_current else target_sample,
           own_credited_n = pmin(own_achieved_n, credited_achieved_n)) %>%
    group_by(region, adm1_name, adm2_pcode, adm2_name) %>%
    summarise(
      credited_own_n = sum(own_credited_n, na.rm = TRUE),
      target_sample = sum(target_sample, na.rm = TRUE),
      target_sample_current = sum(target_sample_current, na.rm = TRUE),
      target_active = sum(target_active, na.rm = TRUE),
      achieved_n = sum(achieved_n, na.rm = TRUE),
      credited_achieved_n = sum(credited_achieved_n, na.rm = TRUE),
      remaining_n = sum(remaining_n, na.rm = TRUE),
      collected_n = sum(collected_n, na.rm = TRUE),
      confirmed_deletion_n = sum(confirmed_deletion_n, na.rm = TRUE),
      pending_deletion_n = sum(pending_deletion_n, na.rm = TRUE),
      oversampling_surplus_n = sum(oversampling_surplus_n, na.rm = TRUE),
      all_dropped_lga = all(all_dropped_lga),
      .groups = "drop"
    ) %>%
    mutate(
      pct_achieved = ifelse(target_active > 0, credited_achieved_n / target_active, NA_real_),
      credited_by_others_n = credited_achieved_n - credited_own_n,
      status = case_when(
        all_dropped_lga ~ "Dropped",
        target_active <= 0 | remaining_n <= 0 ~ "Complete",
        achieved_n > 0 ~ "In progress",
        TRUE ~ "Not started"
      ),
      shared_with = vapply(adm2_pcode, shared_with_label, character(1), org = org_id_val)
    ) %>%
    arrange(pct_achieved)
}

partner_quality_summary <- function(org_id_val) {
  df <- submissions_raw %>% filter(org_id == org_id_val)
  tibble(
    submissions = nrow(df),
    completed = sum(df$interview_outcome == "completed"),
    consent_refused = sum(df$interview_outcome == "consent_refused"),
    flagged = sum(df$any_quality_flag),
    flag_rate = ifelse(nrow(df) > 0, mean(df$any_quality_flag), NA_real_),
    avg_duration = mean(df$duration_min, na.rm = TRUE)
  )
}

compute_enumerator_stats <- function(subs) {
  subs %>%
    filter(!is_duplicate) %>%
    group_by(enum_id) %>%
    summarise(
      org_id = first(org_id),
      state = first(admin1),
      submissions = n(),
      completed = sum(interview_outcome == "completed"),
      consent_refused = sum(interview_outcome == "consent_refused"),
      avg_duration = mean(duration_min, na.rm = TRUE),
      median_duration = median(duration_min, na.rm = TRUE),
      flag_rate = mean(any_quality_flag),
      flagged = sum(any_quality_flag),
      first_date = min(submission_date, na.rm = TRUE),
      last_date = max(submission_date, na.rm = TRUE),
      days_active = n_distinct(submission_date),
      max_in_a_day = max(table(submission_date)),
      .groups = "drop"
    ) %>%
    mutate(
      avg_per_active_day = ifelse(days_active > 0, submissions / days_active, NA_real_),
      org_label = unname(ORG_LABELS[org_id])
    )
}

MAX_PLAUSIBLE_INTERVIEWS_PER_DAY <- 12

# Whipple's index on ages 23-62: 100 = no heaping on ages ending in 0 or 5, 500 = all of them
whipples_index <- function(ages) {
  ages <- ages[!is.na(ages) & ages >= 23 & ages <= 62]
  if (length(ages) == 0) return(NA_real_)
  heaped <- sum(ages %% 10 %in% c(0, 5))
  (heaped / length(ages)) * 500
}

whipples_label <- function(w) {
  case_when(
    is.na(w) ~ "n/a",
    w < 105 ~ "Highly accurate",
    w < 110 ~ "Fairly accurate",
    w < 125 ~ "Approximate",
    w < 175 ~ "Rough",
    TRUE ~ "Very rough"
  )
}

find_gps_duplicate_groups <- function(subs) {
  subs %>%
    filter(interview_outcome == "completed", !is.na(latitude_submitted)) %>%
    group_by(latitude_submitted, longitude_submitted) %>%
    filter(n() > 1) %>%
    summarise(
      n_submissions = n(),
      n_enumerators = n_distinct(enum_id),
      enumerators = paste(unique(enum_id), collapse = ", "),
      admin1 = first(admin1),
      admin2_submitted = first(admin2_submitted),
      dates = paste(sort(unique(as.character(submission_date))), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_submissions))
}

design_avg_hh_size <- function(strata_df = strata_frame) {
  strata_df %>%
    group_by(region, pop_type) %>%
    summarise(design_avg_hh_size = sum(n_pop, na.rm = TRUE) / sum(N_hh, na.rm = TRUE), .groups = "drop")
}

pct_color <- function(pct) {
  case_when(
    is.na(pct) ~ "#9AA3AF",
    pct >= 1 ~ "#1E7B4D",
    pct >= 0.75 ~ "#8FC79A",
    pct >= 0.35 ~ "#D99A2B",
    TRUE ~ "#C1443C"
  )
}

fmt_pct <- function(x) ifelse(is.na(x), "-", percent(x, accuracy = 1))

# Original and Revised targets more than 25% apart are highlighted
TARGET_DIVERGENCE_THRESHOLD <- 0.25

target_delta_pct <- function(original, revised) {
  ifelse(is.na(original) | is.na(revised) | original <= 0, NA_real_, (revised - original) / original)
}
is_significant_target_divergence <- function(original, revised) {
  d <- target_delta_pct(original, revised)
  !is.na(d) & abs(d) >= TARGET_DIVERGENCE_THRESHOLD
}

target_delta_label <- function(original, revised) {
  d <- target_delta_pct(original, revised)
  ifelse(is.na(d), "", paste0(ifelse(d >= 0, "+", ""), percent(d, accuracy = 1), " vs Original"))
}

target_basis_label <- function(target_basis) {
  if (identical(target_basis, "revised")) "Revised Target" else "Original Target"
}

# Splits Collected into what counts (Achieved) and the reasons the rest does not
collected_breakdown <- function(progress_df, subs) {
  in_roster <- progress_df %>% filter(status != "Dropped")
  collected <- sum(is_collected(subs))
  dropped_collected <- sum(progress_df$collected_n[progress_df$status == "Dropped"], na.rm = TRUE)
  in_roster_collected <- sum(in_roster$collected_n, na.rm = TRUE)
  achieved <- sum(in_roster$achieved_n, na.rm = TRUE)
  list(
    collected = collected,
    achieved = achieved,
    confirmed = sum(in_roster$confirmed_deletion_n, na.rm = TRUE),
    surplus = sum(in_roster$oversampling_surplus_n, na.rm = TRUE),
    dropped_collected = dropped_collected,
    unmatched = max(collected - in_roster_collected - dropped_collected, 0),
    gap = max(collected - achieved, 0)
  )
}

active_planned_interviews <- function(target_basis) {
  if (identical(target_basis, "revised")) TOTAL_PLANNED_INTERVIEWS_CURRENT else TOTAL_PLANNED_INTERVIEWS
}

fmt_num <- function(x, digits = 1) ifelse(is.na(x), "-", round(x, digits))

info_icon <- function(definition, color = NULL) {
  bslib::tooltip(
    icon(
      "circle-info",
      class = if (is.null(color)) "text-muted" else NULL,
      style = paste0(
        "font-size: 0.75em; margin-left: 6px; cursor: help;",
        if (!is.null(color)) paste0(" color: ", color, ";") else ""
      )
    ),
    definition,
    placement = "top"
  )
}
info_title <- function(label, definition, icon_color = NULL) {
  div(
    style = "display: flex; align-items: center; justify-content: space-between;",
    span(label), info_icon(definition, color = icon_color)
  )
}

bin_integer_counts <- function(x, from, to) {
  tibble(value = from:to) %>%
    left_join(tibble(value = round(x)) %>% filter(!is.na(value), value >= from, value <= to) %>% count(value), by = "value") %>%
    mutate(n = coalesce(n, 0L))
}

bin_continuous_counts <- function(x, bins = 40) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(tibble(center = numeric(0), n = integer(0)))
  if (min(x) == max(x)) return(tibble(center = x[1], n = length(x)))
  breaks <- seq(min(x), max(x), length.out = bins + 1)
  h <- hist(x, breaks = breaks, plot = FALSE, include.lowest = TRUE)
  tibble(center = h$mids, n = h$counts)
}

days_total <- as.numeric(FIELDING_PLANNED_END - FIELDING_START) + 1

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
