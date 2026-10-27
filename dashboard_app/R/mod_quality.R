# Data Quality: what cleaning flagged, by type, down to the submission.

FLAG_TYPES <- c(
  flag_gps_outlier = "GPS far from the household",
  flag_duration_outlier = "Interview too short",
  flag_hh_size_mismatch = "Household size / roster inconsistent",
  flag_not_in_sample = "Cluster code not in the sample",
  flag_food_security = "Food-security answers inconsistent",
  flag_enumerator_pattern = "Enumerator pattern (fast, alike interviews)",
  is_duplicate = "Duplicate"
)

mod_quality_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Data Quality",
    icon = icon("triangle-exclamation"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Flagged submissions", value = textOutput(ns("kpi_flagged")), showcase = icon("flag"), theme = "danger"),
      value_box(title = "GPS outliers", value = textOutput(ns("kpi_gps")), showcase = icon("location-crosshairs"), theme = "warning"),
      value_box(title = "Interviews too short", value = textOutput(ns("kpi_duration")), showcase = icon("clock"), theme = "warning"),
      value_box(title = "Likely duplicates", value = textOutput(ns("kpi_dup")), showcase = icon("copy"), theme = "warning")
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          "Flags by type",
          info_icon(paste0("Each bar counts submissions with that flag; one submission can have several. GPS far = more than ",
                           SETTINGS$max_gps_distance_m, " m from the sampled household. Too short = under ",
                           SETTINGS$min_duration_min, " minutes. Duplicates, too-short interviews and cluster codes not in the sample are deleted in cleaning; the rest are followed up with the partner."))
        ),
        plotlyOutput(ns("flag_plot"), height = "340px")
      )
    ),
    card(
      card_header("Flagged submissions", span(class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;", "Cleaning checks C01-C14 are described in the repo's README")),
      DTOutput(ns("flagged_table"))
    )
  )
}

mod_quality_server <- function(id, filtered_subs) {
  moduleServer(id, function(input, output, session) {
    output$kpi_flagged <- renderText({
      df <- filtered_subs()
      paste0(comma(sum(df$any_quality_flag)), " (", fmt_pct(mean(df$any_quality_flag)), ")")
    })
    output$kpi_gps <- renderText({ comma(sum(filtered_subs()$flag_gps_outlier)) })
    output$kpi_duration <- renderText({ comma(sum(filtered_subs()$flag_duration_outlier)) })
    output$kpi_dup <- renderText({ comma(sum(filtered_subs()$is_duplicate)) })

    output$flag_plot <- renderPlotly({
      df <- filtered_subs()
      counts <- tibble(flag = unname(FLAG_TYPES), n = vapply(names(FLAG_TYPES), function(f) sum(df[[f]]), numeric(1))) %>%
        arrange(n)

      plot_ly(counts, x = ~n, y = ~factor(flag, levels = flag), type = "bar", orientation = "h",
              marker = list(color = "#D99A2B"), text = ~comma(n), textposition = "outside") %>%
        layout(xaxis = list(title = "Submissions"), yaxis = list(title = ""), margin = list(l = 260))
    })

    output$flagged_table <- renderDT({
      yn <- function(x) factor(ifelse(x, "Yes", "No"), levels = c("Yes", "No"))
      df <- filtered_subs() %>%
        filter(any_quality_flag) %>%
        transmute(
          Date = submission_date,
          State = factor(admin1),
          LGA = admin2_submitted,
          Partner = factor(unname(ORG_LABELS[org_id])),
          Enumerator = enum_id,
          `Duration (min)` = duration_min,
          `GPS dist. (m)` = dist_to_matched_point_m,
          `Match quality` = factor(match_quality),
          `GPS far` = yn(flag_gps_outlier),
          `Too short` = yn(flag_duration_outlier),
          `HH size mismatch` = yn(flag_hh_size_mismatch),
          `Not in sample` = yn(flag_not_in_sample),
          `Food security` = yn(flag_food_security),
          `Enumerator pattern` = yn(flag_enumerator_pattern),
          Duplicate = yn(is_duplicate),
          `Cleaning checks` = cleaning_checks,
          Outcome = factor(case_when(deletion_status == "confirmed" ~ "Deleted",
                                     deletion_status == "pending" ~ "Follow-up",
                                     TRUE ~ "Kept"))
        ) %>%
        arrange(desc(Date))

      datatable(df, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}
