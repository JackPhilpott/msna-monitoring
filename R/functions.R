# Shared functions for the monitoring pipeline.

suppressPackageStartupMessages({
  library(here)
  library(yaml)
})

SYNTHETIC_LABEL <- "Synthetic data – not real findings"

read_config <- function(path = here::here("config.yml")) {
  yaml::read_yaml(path)
}

out_path <- function(cfg, ...) here::here(cfg$outputs$dir, ...)

# Write a CSV (gzipped if the name ends in .gz) without row names. R's gzip
# connection writes no timestamp, so the same data always gives the same file.
write_synthetic <- function(df, name, cfg) {
  path <- out_path(cfg, name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
  invisible(path)
}

read_submissions <- function(cfg) {
  files <- list.files(out_path(cfg, "raw"), pattern = "^submissions_.*\\.csv\\.gz$", full.names = TRUE)
  dplyr::bind_rows(lapply(files, readr::read_csv, col_types = readr::cols(.default = "c"),
                          show_col_types = FALSE))
}

# Great-circle distance in metres.
haversine_m <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  h <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(h)))
}

# Standard food-security composites (WFP FCS and rCSI, FANTA HHS, LCSI).
FCS_WEIGHTS <- c(cereals = 2, pulses = 3, dairy = 4, meat = 4, vegetables = 1, fruit = 1, oil = 0.5,
                 sugar = 0.5)
RCSI_WEIGHTS <- c(less_preferred = 1, borrow_food = 2, limit_portions = 1, restrict_adults = 3,
                  reduce_meals = 1)
LCSI_LEVEL <- c(spend_savings = "stress", sell_assets = "stress", reduce_health_education = "crisis",
                sell_productive_assets = "crisis", sell_house_land = "emergency", beg = "emergency")

# Weighted sums are added column by column, so one household works as well as thousands
fcs_score <- function(d) {
  Reduce(`+`, lapply(names(FCS_WEIGHTS), function(g) as.numeric(d[[paste0("fcs_", g)]]) * FCS_WEIGHTS[[g]]))
}
fcs_category <- function(score) {
  cut(score, c(-Inf, 21, 35, Inf), labels = c("poor", "borderline", "acceptable"))
}
rcsi_score <- function(d) {
  Reduce(`+`, lapply(names(RCSI_WEIGHTS), function(i) as.numeric(d[[paste0("rcsi_", i)]]) * RCSI_WEIGHTS[[i]]))
}
hhs_score <- function(d) {
  item <- function(x, f) ifelse(x == "yes", ifelse(f == "often", 2, 1), 0)
  item(d$hhs_no_food, d$hhs_no_food_freq) + item(d$hhs_sleep_hungry, d$hhs_sleep_hungry_freq) +
    item(d$hhs_whole_day, d$hhs_whole_day_freq)
}
hhs_category <- function(score) {
  cut(score, c(-Inf, 1, 3, Inf), labels = c("none_slight", "moderate", "severe"))
}
lcsi_category <- function(d) {
  used <- function(level) {
    cols <- paste0("lcsi_", names(LCSI_LEVEL)[LCSI_LEVEL == level])
    Reduce(`|`, lapply(cols, function(c) d[[c]] %in% c("yes", "no_exhausted")))
  }
  ifelse(used("emergency"), "emergency", ifelse(used("crisis"), "crisis", ifelse(used("stress"), "stress", "none")))
}
