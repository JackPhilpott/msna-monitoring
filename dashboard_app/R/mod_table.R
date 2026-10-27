# Progress by LGA: achieved against target for every LGA x population group.

mod_table_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Progress by LGA",
    icon = icon("table"),
    card(
      full_screen = TRUE,
      card_header(
        "Achieved vs. target, per LGA x population group",
        info_icon("Achieved = every interview that counts toward target here, including any collected past this stratum's own target. Still needed = what remains after that cap, so Achieved + Still needed only equals Target where nothing was over-collected; Extra interviews shows the amount above target. % achieved is capped at 100% (it counts Achieved up to target). Collected = every completed interview. Confirmed Deleted = removed in cleaning. Pending Deletion = already in Achieved, but flagged for follow-up. % from reserve = share of Achieved interviews done with a reserve household."),
        span(
          class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;",
          "Original Target = the fixed sample size set at the start. Revised Target = the current minimum needed on the population still accessible. Status and % achieved follow the sidebar's Target basis toggle. Δ vs Original flags a 25%+ difference between the two. Dropped = every cluster inaccessible; its interviews stay on this row but are left out of national and partner totals."
        )
      ),
      div(class = "text-muted", style = "font-size: 0.8em; padding: 0 12px;", AS_OF_CAPTION),
      DTOutput(ns("table"))
    )
  )
}

mod_table_server <- function(id, filtered_stratum) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      df <- filtered_stratum() %>%
        transmute(
          Region = factor(region),
          State = factor(adm1_name),
          LGA = adm2_name,
          `Pop. group` = factor(unname(POP_TYPE_LABELS[pop_type])),
          `Partner coverage` = vapply(adm2_pcode, partner_coverage_label, character(1)),
          `Original Target` = target_sample,
          `Revised Target` = target_sample_current,
          `Δ vs Original` = target_delta_pct(target_sample, target_sample_current),
          Collected = collected_n,
          `Confirmed Deleted` = confirmed_deletion_n,
          `Pending Deletion` = pending_deletion_n,
          Achieved = achieved_n,
          `Still needed` = remaining_n,
          `Extra interviews` = pmax(achieved_n - target_active, 0),
          `% achieved` = pct_achieved,
          `% from reserve` = pct_reserve_used,
          Status = factor(status, levels = names(STRATUM_STATUS_COLORS))
        ) %>%
        arrange(desc(`Still needed`))

      col <- function(name) which(names(df) == name) - 1L  # DT counts columns from 0
      datatable(
        df,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          order = list(list(col("Still needed"), "desc")),
          columnDefs = list(list(className = "dt-right", targets = col("Original Target"):col("% from reserve")))
        )
      ) %>%
        formatRound(c("Original Target", "Revised Target"), 0) %>%
        formatPercentage(c("% achieved", "% from reserve"), 1) %>%
        formatPercentage("Δ vs Original", 1) %>%
        formatStyle(
          "Δ vs Original",
          backgroundColor = styleInterval(
            c(-TARGET_DIVERGENCE_THRESHOLD, TARGET_DIVERGENCE_THRESHOLD),
            c("#FCE8CF", "#FFFFFF", "#FCE8CF")
          )
        ) %>%
        formatStyle(
          "Status",
          backgroundColor = styleEqual(names(STRATUM_STATUS_COLORS), unname(STRATUM_STATUS_COLORS)),
          color = styleEqual(names(STRATUM_STATUS_COLORS), c("white", "#1a1a1a", "white", "white"))
        ) %>%
        formatStyle(
          "Still needed",
          background = styleColorBar(c(0, max(df$`Still needed`, 1, na.rm = TRUE)), "#F8CBAD"),
          backgroundSize = "90% 70%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "left"
        ) %>%
        formatStyle(
          "Pop. group",
          color = styleEqual(unname(POP_TYPE_LABELS), unname(POP_TYPE_COLORS)),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          "% achieved",
          background = styleColorBar(c(0, 1), "#CFE0F5"),
          backgroundSize = "90% 70%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "left"
        ) %>%
        formatStyle(
          "% from reserve",
          background = styleColorBar(c(0, max(df$`% from reserve`, 0.01, na.rm = TRUE)), "#F0DCF0"),
          backgroundSize = "90% 70%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "left"
        ) %>%
        formatStyle(
          "Pending Deletion",
          background = styleColorBar(c(0, max(df$`Pending Deletion`, 1, na.rm = TRUE)), "#FCE8CF"),
          backgroundSize = "90% 70%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "left"
        )
    })
  })
}
