# Step 5: data for the dashboard.
#
# The dashboard is a port of the one used on the real survey, so this step
# reshapes the pipeline's synthetic outputs into the inputs it expects, inside
# the app's own folder (shinyapps.io deploys only that folder):
#
# - submissions.rds: one row per raw submission with what cleaning decided:
#   the sampled cluster and household it matches, its distance from that
#   household, quality flags, and a deletion status (confirmed = removed by
#   cleaning, pending = flagged for follow-up and still counted)
# - strata.csv, clusters.csv, partner_lga_assignment.csv: the sampling frame
# - accessibility_wards.csv, accessibility_strata.csv: the wards partners
#   reported inaccessible, and each stratum's revised target on the
#   population still accessible
# - boundaries/: COD-AB states and LGAs, synthetic wards, PSU hexagons and IDP sites
#
# Wards are synthetic: each LGA is split into made-up wards. Real ward names
# are left out because IDP camps are often named after the ward they are in.
#
# Inputs: data/synthetic/{from_sampling,raw,clean}/, COD-AB boundaries
# Outputs: dashboard_app/data/

source(here::here("R", "functions.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
})

cfg <- read_config()
set.seed(cfg$seed)
dash <- cfg$dashboard
design <- cfg$design
dest <- here::here(cfg$outputs$dashboard_data)
unlink(dest, recursive = TRUE)
dir.create(file.path(dest, "boundaries"), recursive = TRUE)
sampling <- function(...) out_path(cfg, "from_sampling", ...)
read_chr <- function(path) read_csv(path, col_types = cols(.default = "c"))
org_id <- function(label) tolower(sub(" ", "_", label))  # Partner A -> partner_a
UTM <- 32632  # WGS 84 / UTM 32N, as in msna-sampling

states <- unlist(dash$regions, use.names = FALSE)
region_of <- setNames(rep(names(dash$regions), lengths(dash$regions)), states)
lgas <- read_csv(sampling("lgas.csv"), show_col_types = FALSE) |> filter(surveyed)
strata <- read_csv(sampling("strata.csv"), show_col_types = FALSE)
clusters <- read_csv(sampling("clusters_after_resampling.csv"), show_col_types = FALSE)
frame <- read_csv(sampling("frame_psus.csv"), show_col_types = FALSE)
sites <- read_csv(sampling("idp_sites_synthetic.csv"), show_col_types = FALSE)
households <- list.files(sampling("households"), full.names = TRUE) |>
  lapply(read_csv, show_col_types = FALSE) |> bind_rows()


# --- Boundaries -----------------------------------------------------------------

boundaries_dir <- function() {
  b <- cfg$open_data$boundaries
  if (file.exists(here::here(b$local_dir, "nga_admin2_em.shp"))) return(here::here(b$local_dir))
  dir <- here::here(b$dir)
  if (!file.exists(file.path(dir, "nga_admin2_em.shp"))) {
    dir.create(dirname(here::here(b$zip)), recursive = TRUE, showWarnings = FALSE)
    options(timeout = 3600)
    download.file(b$url, here::here(b$zip), mode = "wb", quiet = TRUE)
    unzip(here::here(b$zip), exdir = dir)
  }
  dir
}
bdir <- boundaries_dir()
read_cod <- function(file) st_read(file.path(bdir, file), quiet = TRUE) |> st_make_valid()

# Simplified for the web map: a few hundred metres of detail is invisible at LGA zoom
simplify <- function(x, tol = dash$simplify_m) {
  st_transform(x, UTM) |> st_simplify(preserveTopology = TRUE, dTolerance = tol) |> st_transform(4326)
}
write_geo <- function(x, name) {
  st_write(x, file.path(dest, "boundaries", name), quiet = TRUE,
           layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=5"))
}

lga_utm <- read_cod("nga_admin2_em.shp") |>
  filter(adm1_name %in% states) |>
  select(adm1_pcode, adm1_name, adm2_pcode, adm2_name) |>
  st_transform(UTM)
surveyed_utm <- filter(lga_utm, adm2_pcode %in% lgas$adm2_pcode)

# Random points in the LGA, one per synthetic ward, and their Voronoi cells
# clipped to the LGA. Numbered from north to south.
synthetic_wards <- function(lga) {
  g <- st_geometry(lga)
  k <- min(15, max(4, round(as.numeric(st_area(g)) / 1e6 / dash$synthetic_ward_km2)))
  cells <- st_sample(g, k) |> st_union() |> st_voronoi(envelope = g) |> st_collection_extract("POLYGON")
  cells <- suppressWarnings(st_intersection(cells, g) |> st_collection_extract("POLYGON")) |> st_cast("MULTIPOLYGON")
  cells <- cells[order(-st_coordinates(st_centroid(cells))[, 2])]
  st_sf(wardname = sprintf("%s ward %d", lga$adm2_name, seq_along(cells)), adm2_pcode = lga$adm2_pcode,
        geometry = cells)
}
wards_utm <- do.call(rbind, lapply(split(surveyed_utm, surveyed_utm$adm2_pcode), synthetic_wards)) |>
  left_join(st_drop_geometry(surveyed_utm), by = "adm2_pcode") |>
  arrange(adm2_pcode, wardname)

# Each cluster's ward: the nearest ward of its own LGA (the containing one when inside)
cluster_pts <- st_as_sf(clusters, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> st_transform(UTM)
clusters$adm3_name <- NA_character_
for (pc in unique(clusters$adm2_pcode)) {
  i <- which(clusters$adm2_pcode == pc)
  w <- wards_utm[wards_utm$adm2_pcode == pc, ]
  clusters$adm3_name[i] <- w$wardname[st_nearest_feature(cluster_pts[i, ], w)]
}

write_geo(read_cod("nga_admin0_em.shp") |> select(adm0_pcode) |> simplify(500), "admin0.geojson")
# states outside the assessment are drawn grey, so they need no name
write_geo(read_cod("nga_admin1_em.shp") |>
            transmute(adm1_pcode, adm1_name = if_else(adm1_name %in% states, adm1_name, NA_character_)) |>
            simplify(300), "admin1.geojson")
write_geo(simplify(lga_utm), "admin2.geojson")
write_geo(wards_utm |> transmute(wardname, lganame = adm2_name, statename = adm1_name, adm2_pcode) |>
            simplify(), "wards.geojson")


# --- Sampling frame -------------------------------------------------------------

partner_of_lga <- setNames(org_id(lgas$partner), lgas$adm2_pcode)
site_names <- setNames(sites$site_name, sites$site_id)
cluster_frame <- clusters |>
  transmute(cluster_id, strata_id = stratum_id, pop_type, adm1_pcode, adm1_name, adm2_pcode, adm2_name, adm3_name,
            org_id = org_id(partner), target_households = design$cluster_size, sample_status = status, draw,
            replaces, site_name = unname(site_names[psu_id]), lon, lat)

# Hexagons rebuilt around each PSU centre: pointy-topped, like sf::st_make_grid()
hexagon <- function(x, y, cellsize = design$hex_cellsize_m) {
  r <- cellsize / sqrt(3)
  a <- c(seq(30, 330, by = 60), 30) * pi / 180
  st_polygon(list(cbind(x + r * cos(a), y + r * sin(a))))
}
# The PSU layers hold only the cluster ID and its shape; the app joins the rest from clusters.csv
hex <- filter(cluster_frame, pop_type == "non_idp")
centres <- st_coordinates(st_transform(st_as_sf(hex, coords = c("lon", "lat"), crs = 4326), UTM))
write_geo(st_sf(select(hex, cluster_id), geometry = st_sfc(Map(hexagon, centres[, 1], centres[, 2]), crs = UTM)) |>
            st_transform(4326), "psu_hexagons_non_idp.geojson")
write_geo(st_as_sf(filter(cluster_frame, pop_type == "idp"), coords = c("lon", "lat"), crs = 4326) |>
            select(cluster_id), "psu_sites_idp.geojson")
write_csv(select(cluster_frame, -lon, -lat), file.path(dest, "clusters.csv"), na = "")

# Revised target: the minimum sample on the households still accessible, plus a
# 5% operational margin (the original target carries the design's 10% buffer),
# never above the original. Households are lost when partners report a PSU's
# clusters inaccessible.
min_sample <- function(N, margin) {
  z <- qnorm(1 - (1 - design$confidence) / 2)
  n0 <- z^2 * design$proportion * (1 - design$proportion) / design$margin_of_error^2
  n_deff <- n0 * (1 + (design$cluster_size - 1) * design$icc)
  pmin(N, ceiling(N * n_deff / (N + n_deff - 1) * (1 + margin)))
}
lost <- frame |>
  filter(psu_id %in% clusters$psu_id[clusters$status == "inaccessible"]) |>
  group_by(strata_id = paste(pop_type, adm2_pcode, sep = "_")) |>
  summarise(hh_lost = sum(size_hh), .groups = "drop")
active_clusters <- count(filter(clusters, status == "active"), stratum_id, name = "n_active")

strata_frame <- strata |>
  left_join(lost, by = c("stratum_id" = "strata_id")) |>
  left_join(active_clusters, by = "stratum_id") |>
  transmute(region = unname(region_of[adm1_name]), adm1_pcode, adm1_name, adm2_pcode, adm2_name, pop_type,
            strata_id = stratum_id, n_pop = round(N_hh * design$persons_per_household), N_hh,
            target_sample = households_target,
            coverage_status = if_else(coalesce(n_active, 0L) > 0, "covered", "excluded"),
            exclusion_reason = if_else(coverage_status == "covered", "none", "all_clusters_inaccessible"),
            hh_remaining = pmax(N_hh - coalesce(hh_lost, 0), 0))
write_csv(select(strata_frame, -hh_remaining), file.path(dest, "strata.csv"))
write_csv(strata_frame |>
            transmute(strata_id, pop_type,
                      pct_pop_remaining = round(100 * hh_remaining / N_hh, 1),
                      pop_accessible = round(hh_remaining * design$persons_per_household), pop_design = n_pop,
                      target_revised = if_else(hh_remaining > 0,
                                               pmin(target_sample, min_sample(hh_remaining, dash$revised_target_margin)),
                                               NA_real_)),
          file.path(dest, "accessibility_strata.csv"), na = "")
write_csv(lgas |> transmute(adm1_pcode, adm1_name, adm2_pcode, adm2_name, org_id = org_id(partner)),
          file.path(dest, "partner_lga_assignment.csv"))

# Ward accessibility, per population group: inaccessible when partners reported
# its clusters inaccessible and none is still active. Wards without a cluster
# keep the default (accessible, not reported).
reported <- clusters |>
  group_by(adm2_pcode, wardname = adm3_name, pop_type) |>
  summarise(inaccessible = all(status == "inaccessible"), partner = first(partner), .groups = "drop")
reasons <- c("Insecurity" = 0.6, "Flooding or impassable roads" = 0.3, "Access refused by local authorities" = 0.1)
access_wards <- st_drop_geometry(wards_utm) |>
  select(adm1_name, adm2_pcode, adm2_name, wardname) |>
  inner_join(distinct(strata, adm2_pcode, pop_type), by = "adm2_pcode", relationship = "many-to-many") |>
  left_join(reported, by = c("adm2_pcode", "wardname", "pop_type")) |>
  arrange(adm2_pcode, wardname, pop_type) |>
  mutate(accessible_status = if_else(coalesce(inaccessible, FALSE), "Inaccessible", "Accessible"),
         status_source = if_else(is.na(inaccessible), "default", "confirmed_by_partner_report"),
         reporting_partners = coalesce(partner, ""),
         reason_category = if_else(accessible_status == "Inaccessible",
                                   sample(names(reasons), n(), replace = TRUE, prob = reasons),
                                   "N/A - fully accessible"),
         covering_partners = lgas$partner[match(adm2_pcode, lgas$adm2_pcode)],
         pop_type = if_else(pop_type == "idp", "IDP", "Non-IDP")) |>
  select(adm1_name, adm2_pcode, adm2_name, wardname, pop_type, accessible_status, status_source,
         reporting_partners, reason_category, covering_partners)
write_csv(access_wards, file.path(dest, "accessibility_wards.csv"))


# --- Submissions ----------------------------------------------------------------

raw <- read_submissions(cfg)
log <- read_chr(out_path(cfg, "clean", "cleaning_log.csv"))
removed <- read_chr(out_path(cfg, "clean", "deletion_log.csv")) |>
  left_join(distinct(log, check_id, check), by = "check_id") |>
  arrange(`_index`, check_id) |>
  distinct(`_index`, .keep_all = TRUE)
per_row <- log |>
  group_by(`_index`) |>
  summarise(cleaning_checks = paste(sort(unique(check_id)), collapse = "; "), flagged = any(action == "flag"),
            .groups = "drop")
hh_size_fixes <- filter(log, check_id == "C10", action == "change")
checked <- function(ids) raw$`_index` %in% log$`_index`[log$check_id %in% ids]

subs <- raw |>
  # the household's cluster (IDs end _H01 for primary, _R01 for reserve households), even if the code entered is wrong
  mutate(home = sub("_[HR][0-9]+$", "", household_id)) |>
  left_join(select(clusters, cluster_id, stratum_id, pop_type, adm1_name, adm2_name, adm3_name, cl_lon = lon,
                   cl_lat = lat), by = c("home" = "cluster_id")) |>
  left_join(select(households, household_id, role, hh_lon = lon, hh_lat = lat), by = "household_id") |>
  left_join(select(removed, `_index`, removed_by = check_id, removal = check), by = "_index") |>
  left_join(per_row, by = "_index")

matched <- subs$cluster_id %in% clusters$cluster_id
start <- lubridate::ymd_hms(substr(subs$start, 1, 19))  # local (West Africa) clock time
end <- lubridate::ymd_hms(substr(subs$end, 1, 19))
uploaded <- lubridate::ymd_hms(subs$`_submission_time`) + 3600
gps_lon <- as.numeric(subs$`_gps_longitude`)
gps_lat <- as.numeric(subs$`_gps_latitude`)
# households drawn by msna-sampling have their own point; others are measured from the cluster point
dist <- haversine_m(gps_lon, gps_lat, coalesce(subs$hh_lon, subs$cl_lon), coalesce(subs$hh_lat, subs$cl_lat))
hh_size <- as.integer(subs$hh_size)
fix <- match(subs$`_index`, hh_size_fixes$`_index`)
hh_size[!is.na(fix)] <- as.integer(hh_size_fixes$new_value[fix[!is.na(fix)]])
off_hours <- lubridate::hour(start) < dash$off_hours[[1]] | lubridate::hour(start) >= dash$off_hours[[2]]

submissions <- tibble(
  submission_index = as.integer(subs$`_index`),
  submission_uuid = subs$`_uuid`,
  submission_date = as.Date(subs$today),
  start_datetime = start,
  end_datetime = end,
  uploaded_at = uploaded,
  sync_lag_min = round(as.numeric(difftime(uploaded, end, units = "mins"))),
  duration_min = round(as.numeric(difftime(end, start, units = "mins")), 1),
  interview_outcome = if_else(subs$consent == "yes", "completed", "consent_refused"),
  org_id = subs$partner,
  enum_id = subs$enumerator_id,
  admin1 = subs$adm1_name,
  admin2_submitted = subs$adm2_name,
  admin3_submitted = subs$adm3_name,
  latitude_submitted = round(gps_lat, 5),
  longitude_submitted = round(gps_lon, 5),
  setting = subs$setting,
  resp_gender = subs$resp_gender,
  resp_age = as.integer(subs$resp_age),
  resp_hoh_yn = subs$resp_is_hoh,
  hoh_gender = subs$hoh_gender,
  hoh_age = as.integer(subs$hoh_age),
  hh_size = hh_size,
  matched_survey_id = if_else(matched, subs$household_id, NA_character_),
  matched_cluster_id = if_else(matched, subs$cluster_id, NA_character_),
  matched_strata_id = if_else(matched, subs$stratum_id, NA_character_),
  matched_status = if_else(matched, subs$role, NA_character_),
  pop_type = subs$pop_type,
  dist_to_matched_point_m = if_else(matched, round(dist), NA_real_),
  match_quality = case_when(!matched ~ "unmatched_no_point_id",
                            dist > cfg$checks$max_gps_distance_m ~ "matched_gps_outlier",
                            TRUE ~ "matched"),
  is_duplicate = subs$removed_by %in% c("C02", "C03"),
  quality_exclusion_reason = subs$removal,
  deletion_status = case_when(!is.na(subs$removed_by) ~ "confirmed", coalesce(subs$flagged, FALSE) ~ "pending"),
  cleaning_checks = coalesce(subs$cleaning_checks, ""),
  flag_gps_outlier = checked("C07"),
  flag_duration_outlier = checked("C04"),
  flag_hh_size_mismatch = checked(c("C10", "C11", "C12")),
  flag_not_in_sample = checked("C05"),
  flag_off_hours = off_hours,
  flag_food_security = checked(c("C08", "C09")),
  flag_enumerator_pattern = checked("C14")
) |>
  mutate(any_quality_flag = flag_gps_outlier | flag_duration_outlier | flag_hh_size_mismatch | flag_not_in_sample |
           flag_off_hours | flag_food_security | flag_enumerator_pattern | is_duplicate) |>
  arrange(submission_index)
saveRDS(submissions, file.path(dest, "submissions.rds"), compress = "xz")

yaml::write_yaml(list(
  collection_start = as.character(cfg$collection$start_date),
  planned_end = as.character(as.Date(cfg$collection$start_date) + cfg$collection$days - 1),
  regions = dash$regions,
  off_hours = dash$off_hours,
  max_gps_distance_m = cfg$checks$max_gps_distance_m,
  min_duration_min = cfg$checks$min_duration_min
), file.path(dest, "settings.yml"))

files <- list.files(dest, recursive = TRUE, full.names = TRUE)
sizes <- file.info(files)$size
message(sprintf("Dashboard data: %d submissions, %d strata (%d dropped), %d clusters, %d synthetic wards",
                nrow(submissions), nrow(strata_frame), sum(strata_frame$coverage_status == "excluded"),
                nrow(cluster_frame), nrow(wards_utm)))
message(sprintf("%d files, %.1f MB in %s; largest %s (%.2f MB)", length(files), sum(sizes) / 2^20,
                cfg$outputs$dashboard_data, basename(files[which.max(sizes)]), max(sizes) / 2^20))
