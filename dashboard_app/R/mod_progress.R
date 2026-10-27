# Progress Overview: headline figures, daily submissions against the pace
# needed, progress by region, and every partner's progress and pace.

mod_progress_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Progress Overview",
    icon = icon("gauge-high"),
    div(class = "text-muted", style = "font-size: 0.8em; margin-bottom: 8px;", AS_OF_CAPTION),
    layout_columns(
      col_widths = c(4, 4, 4, 4, 4, 4),
      value_box(
        title = uiOutput(ns("kpi_achieved_title")),
        value = textOutput(ns("kpi_achieved")),
        showcase = icon("clipboard-check"),
        theme = "primary"
      ),
      value_box(
        title = uiOutput(ns("kpi_still_needed_title")),
        value = textOutput(ns("kpi_still_needed")),
        showcase = icon("list-check"),
        theme = "danger"
      ),
      value_box(
        title = uiOutput(ns("kpi_pct_title")),
        value = textOutput(ns("kpi_pct")),
        showcase = icon("percent"),
        theme = "success"
      ),
      value_box(
        title = uiOutput(ns("kpi_collected_title")),
        value = textOutput(ns("kpi_collected")),
        showcase = icon("layer-group"),
        theme = "warning"
      ),
      value_box(
        title = info_title(
          "Days to planned end",
          "Calendar days left until the planned end of collection, counted from the end of the sidebar's date range. A fixed countdown, not a pace estimate."
        ),
        value = textOutput(ns("kpi_days_remaining")),
        showcase = icon("calendar-days"),
        theme = "secondary"
      ),
      value_box(
        title = info_title(
          "Est. days required",
          "Days still needed to close what is outstanding, at the pace of the last 7 days (the early ramp-up would make the average since the start look faster than teams are working now). Compare with Days to planned end to see if collection is on track.",
          icon_color = "white"
        ),
        value = textOutput(ns("kpi_days_required")),
        showcase = icon("stopwatch"),
        theme = value_box_theme(bg = "#5C7A99", fg = "white")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(
          "Daily submissions (by population group) vs. pace needed to finish by the planned end",
          info_icon("Bars = completed interviews per day. Green line = cumulative Achieved to date (all interviews, including any surplus past a stratum's own target). Red dotted line = the steady pace needed to reach the target by the planned end. Above the line = ahead of pace in total field effort, but surplus in one stratum cannot close a gap in another, so read this alongside the credited '% of target' tile above."),
          span(
            class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;",
            "\"Unmatched\" = interviews whose cluster code is not in the sample, so they could not be linked to a sampled cluster (see Data Quality)."
          )
        ),
        plotlyOutput(ns("trend_plot"), height = "460px")
      ),
      card(
        card_header("Progress by region", span(class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;", "Non-IDP / IDP / combined")),
        plotlyOutput(ns("region_plot"), height = "460px")
      )
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Progress by partner",
        info_icon("Shows every partner nationally, regardless of sidebar filters. Target includes every LGA assigned to them, even ones not started."),
        span(class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;", "Charts sorted by % of target achieved (lowest first); the table opens sorted by Still needed, largest first")
      ),
      navset_pill(
        nav_panel("By % achieved", plotlyOutput(ns("partner_bar_pct"), height = "480px")),
        nav_panel("Original vs Revised", plotlyOutput(ns("partner_bar_abs"), height = "480px"))
      ),
      DTOutput(ns("partner_table")),
      p(class = "text-muted", style = "font-size: 0.85em; margin: 8px 0 0 0;",
        paste0("Status, Required daily pace and Projected finish are measured against the planned end of collection (",
               format(FIELDING_PLANNED_END, "%d %b %Y"), "). ",
               "'On pace' means the partner's projected finish, at its own current pace, is on or before that date."))
    )
  )
}

mod_progress_server <- function(id, filtered_subs, filtered_stratum, target_basis, as_of = reactive(DATA_AS_OF_DATE)) {
  moduleServer(id, function(input, output, session) {
    completed_subs <- reactive({
      filtered_subs() %>% filter(interview_outcome == "completed", !is_duplicate)
    })

    headline <- reactive({
      s <- filtered_stratum() %>% filter(status != "Dropped")
      ach <- sum(s$achieved_n, na.rm = TRUE)
      cred <- sum(s$credited_achieved_n, na.rm = TRUE)
      list(
        target = sum(s$target_active, na.rm = TRUE),
        achieved = ach,
        credited = cred,
        surplus = ach - cred,
        remaining = sum(s$remaining_n, na.rm = TRUE),
        cb = collected_breakdown(filtered_stratum(), filtered_subs())
      )
    })

    output$kpi_achieved_title <- renderUI({
      h <- headline()
      info_title(
        "Interviews achieved",
        paste0(
          "Every completed interview that counts: excludes confirmed deletions, and includes in full any collected past a stratum's own target. ",
          "Of these, ", comma(h$credited), " count toward the target of ", comma(h$target),
          if (h$surplus > 0) paste0(", and ", comma(h$surplus), " are extra interviews in strata already at target, which is why target minus achieved is not what is still needed. ") else ". ",
          "See the Still needed tile for the real remaining figure."
        ),
        icon_color = "white"
      )
    })

    output$kpi_still_needed_title <- renderUI({
      h <- headline()
      info_title(
        "Still needed",
        paste0(
          "What is actually still required: every stratum's own remaining gap, floored at zero and then summed, so surplus in one place never cancels a shortfall in another. ",
          comma(h$credited), " counted toward target + ", comma(h$remaining), " still needed = ", comma(h$target), " target. ",
          "The partner reports use the same figure."
        )
      )
    })

    output$kpi_pct_title <- renderUI({
      h <- headline()
      info_title(
        "% of target",
        paste0(
          "Progress toward target, capped per stratum before summing: an oversampled stratum's surplus never offsets another's shortfall, so this cannot read 100% while any stratum is still short. ",
          comma(h$credited), " of ", comma(h$target), "."
        )
      )
    })

    output$kpi_collected_title <- renderUI({
      cb <- headline()$cb
      info_title(
        "Collected",
        paste0(
          "Every completed interview done: total field effort, not what counts toward the sample. ",
          comma(cb$gap), " of these do not count toward target: ", comma(cb$confirmed), " confirmed deletions, ",
          comma(cb$dropped_collected), " in strata dropped from the design, ", comma(cb$unmatched), " not matched to a sampled cluster",
          if (cb$surplus > 0) paste0(", ", comma(cb$surplus), " match-quality residual.") else "."
        )
      )
    })

    output$kpi_still_needed <- renderText(comma(headline()$remaining))

    output$kpi_achieved <- renderText(comma(headline()$achieved))

    output$kpi_pct <- renderText({
      h <- headline()
      if (h$target <= 0) return(fmt_pct(NA_real_))
      fmt_pct(h$credited / h$target)
    })

    output$kpi_collected <- renderText(comma(headline()$cb$collected))

    output$kpi_days_remaining <- renderText({
      paste0(max(0, as.numeric(FIELDING_PLANNED_END - as_of())), " / ", days_total, " days")
    })

    output$kpi_days_required <- renderText({
      subs <- filtered_subs()
      remaining <- headline()$remaining
      if (remaining <= 0) return("Target met")

      PACE_WINDOW_DAYS <- 7
      window_start <- as_of() - (PACE_WINDOW_DAYS - 1)
      recent <- sum(is_achieved(subs) & !is.na(subs$submission_date) & subs$submission_date >= window_start, na.rm = TRUE)
      rate <- recent / PACE_WINDOW_DAYS
      if (!is.finite(rate) || rate <= 0) return("N/A")
      paste(comma(ceiling(remaining / rate)), "days")
    })

    output$trend_plot <- renderPlotly({
      df <- completed_subs() %>% filter(!is.na(submission_date))
      if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers"))

      pop_cats <- c("Non-IDP", "IDP", "Unmatched")
      by_day_pop <- df %>%
        mutate(pop_cat = case_when(
          is.na(matched_survey_id) ~ "Unmatched",
          pop_type == "non_idp" ~ "Non-IDP",
          pop_type == "idp" ~ "IDP",
          TRUE ~ "Unmatched"
        )) %>%
        count(submission_date, pop_cat, name = "n") %>%
        complete(submission_date = seq(min(df$submission_date), max(df$submission_date), by = "day"), pop_cat = pop_cats, fill = list(n = 0)) %>%
        arrange(submission_date)

      achieved_scope <- filtered_stratum()$strata_id
      achieved_rows <- filtered_subs() %>%
        filter(is_achieved(.), matched_strata_id %in% achieved_scope, !is.na(matched_cluster_id))

      if (nrow(achieved_rows) == 0) {
        by_day_total <- tibble(
          submission_date = seq(min(df$submission_date), max(df$submission_date), by = "day"),
          cumulative = 0
        )
      } else {
        by_day_total <- achieved_rows %>%
          count(submission_date, name = "n") %>%
          complete(submission_date = seq(min(df$submission_date), max(df$submission_date), by = "day"), fill = list(n = 0)) %>%
          arrange(submission_date) %>%
          mutate(cumulative = cumsum(n))
      }

      target_total <- sum(filtered_stratum() %>% filter(status != "Dropped") %>% pull(target_active), na.rm = TRUE)
      pace_line <- tibble(
        submission_date = seq(FIELDING_START, FIELDING_PLANNED_END, by = "day")
      ) %>%
        mutate(needed_pace = seq(0, target_total, length.out = n()))

      cat_colors <- c("Non-IDP" = unname(POP_TYPE_COLORS[["non_idp"]]), "IDP" = unname(POP_TYPE_COLORS[["idp"]]), "Unmatched" = UNMATCHED_COLOR)

      plot_ly() %>%
        add_bars(
          data = by_day_pop %>% filter(pop_cat == "Non-IDP"), x = ~submission_date, y = ~n,
          name = "Non-IDP", marker = list(color = cat_colors[["Non-IDP"]]), yaxis = "y2"
        ) %>%
        add_bars(
          data = by_day_pop %>% filter(pop_cat == "IDP"), x = ~submission_date, y = ~n,
          name = "IDP", marker = list(color = cat_colors[["IDP"]]), yaxis = "y2"
        ) %>%
        add_bars(
          data = by_day_pop %>% filter(pop_cat == "Unmatched"), x = ~submission_date, y = ~n,
          name = "Unmatched", marker = list(color = cat_colors[["Unmatched"]]), yaxis = "y2"
        ) %>%
        add_lines(data = by_day_total, x = ~submission_date, y = ~cumulative, name = "Cumulative achieved", line = list(color = "#1E7B4D", width = 3)) %>%
        add_lines(data = pace_line, x = ~submission_date, y = ~needed_pace, name = "Pace needed by the planned end", line = list(color = "#C1443C", dash = "dot")) %>%
        layout(
          barmode = "stack",
          yaxis = list(title = "Cumulative interviews"),
          yaxis2 = list(overlaying = "y", side = "right", title = "Daily submissions", showgrid = FALSE),
          xaxis = list(title = ""),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified",
          margin = list(l = 60, r = 80, t = 20, b = 40)
        )
    })

    output$region_plot <- renderPlotly({
      region_base <- filtered_stratum() %>% filter(status != "Dropped")

      by_region_pop <- region_base %>%
        group_by(region, pop_type) %>%
        summarise(target_active = sum(target_active, na.rm = TRUE), credited_achieved_n = sum(credited_achieved_n, na.rm = TRUE), .groups = "drop") %>%
        mutate(pct = ifelse(target_active > 0, credited_achieved_n / target_active, 0), series = unname(POP_TYPE_LABELS[pop_type]))

      combined <- region_base %>%
        group_by(region) %>%
        summarise(target_active = sum(target_active, na.rm = TRUE), credited_achieved_n = sum(credited_achieved_n, na.rm = TRUE), .groups = "drop") %>%
        mutate(pct = ifelse(target_active > 0, credited_achieved_n / target_active, 0), series = "Combined")

      s <- bind_rows(
        by_region_pop %>% select(region, pct, series),
        combined %>% select(region, pct, series)
      ) %>%
        mutate(series = factor(series, levels = c("Non-IDP", "IDP", "Combined")))

      series_colors <- c("Non-IDP" = unname(POP_TYPE_COLORS[["non_idp"]]), "IDP" = unname(POP_TYPE_COLORS[["idp"]]), "Combined" = COMBINED_COLOR)

      plot_ly(
        s, y = ~region, x = ~pct, color = ~series, colors = series_colors,
        type = "bar", orientation = "h",
        text = ~percent(pct, accuracy = 1), textposition = "outside"
      ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = "% of target achieved (credited)", tickformat = ".0%", range = c(0, 1.2)),
          yaxis = list(title = ""),
          legend = list(orientation = "h", y = -0.25),
          margin = list(l = 50, r = 20, t = 20, b = 40)
        )
    })

    partner_summary_active <- reactive(build_partner_progress_summary(target_basis()))

    partner_pace_colors <- c("Behind pace" = "#C1443C", "On pace" = "#1E7B4D", "Complete" = "#1E7B4D", "Not started" = "#9AA3AF")

    partner_order <- partner_progress_summary %>% arrange(pct_achieved) %>% pull(partner_label) %>% rev()

    STAGE_COLORS <- c(
      "Below Revised Target" = "#C1443C",
      "At/above Revised, below Original" = "#8FC79A",
      "At/above Original Target" = "#1E7B4D"
    )

    partner_revised_remaining <- reactive({
      build_partner_progress_summary("revised") %>%
        select(org_id, revised_remaining_n = remaining_n, revised_credited_n = credited_achieved_n)
    })

    # Each partner's bar is its Original Target. The fill is what is credited,
    # coloured by whether it has reached the Revised and Original targets; the
    # tick marks the Revised Target.
    output$partner_bar_abs <- renderPlotly({
      df <- partner_progress_summary %>%
        left_join(partner_revised_remaining(), by = "org_id") %>%
        mutate(
          partner_label = factor(partner_label, levels = partner_order),
          fill_frac = ifelse(target_sample > 0, pmin(credited_achieved_n / target_sample, 1), 0),
          remaining_frac = 1 - fill_frac,
          revised_frac = ifelse(target_sample > 0, target_sample_current / target_sample, NA_real_),
          exceeded = !is.na(target_sample) & target_sample > 0 & remaining_n <= 0 & achieved_n > target_sample,
          overflow_frac = ifelse(exceeded, 1.08, NA_real_),
          stage = factor(case_when(
            target_sample > 0 & remaining_n <= 0 ~ "At/above Original Target",
            target_sample_current > 0 & !is.na(revised_remaining_n) & revised_remaining_n <= 0 ~ "At/above Revised, below Original",
            TRUE ~ "Below Revised Target"
          ), levels = names(STAGE_COLORS)),
          solid_frac = ifelse(
            stage == "Below Revised Target" & !is.na(revised_credited_n) & target_sample > 0,
            pmin(pmin(revised_credited_n, credited_achieved_n) / target_sample, fill_frac),
            fill_frac
          ),
          pale_frac = fill_frac - solid_frac,
          hover_text = paste0(
            "Credited toward target: ", comma(credited_achieved_n), " / Original Target: ", comma(target_sample),
            "\nStill needed: ", comma(remaining_n),
            "\nStill needed to reach Revised Target: ", comma(revised_remaining_n),
            "\nAll interviews (incl. surplus): ", comma(achieved_n),
            " (Revised Target: ", comma(round(target_sample_current)), ")",
            ifelse(exceeded, paste0("\nExceeded Original in total by ", comma(achieved_n - target_sample)), "")
          )
        )

      plot_ly(df) %>%
        add_bars(
          y = ~partner_label, x = ~solid_frac, color = ~stage, colors = STAGE_COLORS,
          orientation = "h", text = ~hover_text, hoverinfo = "text",
          textposition = "none", showlegend = TRUE
        ) %>%
        add_bars(
          y = ~partner_label, x = ~pale_frac, orientation = "h",
          marker = list(color = "rgba(193,68,60,0.30)"),
          text = ~hover_text, hoverinfo = "text", textposition = "none",
          showlegend = any(df$pale_frac > 0), name = "Credited beyond Revised (strata already met)"
        ) %>%
        add_bars(
          y = ~partner_label, x = ~remaining_frac, orientation = "h",
          marker = list(color = "rgba(0,0,0,0)", line = list(color = "#9AA3AF", width = 1)),
          hoverinfo = "skip", showlegend = FALSE, name = "Remaining to Original"
        ) %>%
        add_markers(
          y = ~partner_label, x = ~revised_frac, hoverinfo = "text",
          text = ~paste0("Revised Target: ", comma(round(target_sample_current))),
          marker = list(symbol = "line-ns", size = 13, line = list(width = 2, color = "#1B2A4A")),
          showlegend = FALSE, name = "Revised Target"
        ) %>%
        {
          if (any(df$exceeded)) {
            add_markers(
              ., data = df %>% filter(exceeded), y = ~partner_label, x = ~overflow_frac, hoverinfo = "text",
              text = ~hover_text, marker = list(symbol = "triangle-right", size = 12, color = "#D99A2B"),
              showlegend = FALSE, name = "Exceeded Original"
            )
          } else {
            .
          }
        } %>%
        layout(
          barmode = "stack",
          xaxis = list(title = "Progress toward each partner's own Original Target (the tick marks its Revised Target; always measured on Original, whatever the sidebar's Target basis)", showticklabels = FALSE, range = c(0, 1.15), zeroline = FALSE),
          yaxis = list(title = "", categoryorder = "array", categoryarray = partner_order),
          legend = list(orientation = "h", y = -0.08),
          margin = list(l = 160, r = 20, t = 10, b = 40)
        )
    })

    output$partner_bar_pct <- renderPlotly({
      df <- partner_summary_active() %>%
        mutate(partner_label = factor(partner_label, levels = partner_order))

      plot_ly(
        df, y = ~partner_label, x = ~pct_achieved, color = ~status, colors = partner_pace_colors,
        type = "bar", orientation = "h",
        text = ~paste0(comma(credited_achieved_n), " / ", comma(round(target_active)), " (", percent(pct_achieved, accuracy = 1), " of ", target_basis_label(target_basis()), "; ", comma(achieved_n), " incl. surplus)"),
        textposition = "outside", hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = paste0("% of ", target_basis_label(target_basis()), " achieved"), tickformat = ".0%", range = c(0, 1.15)),
          yaxis = list(title = "", categoryorder = "array", categoryarray = partner_order),
          legend = list(orientation = "h", y = -0.08),
          margin = list(l = 160, r = 20, t = 10, b = 40)
        )
    })

    output$partner_table <- renderDT({
      df <- partner_summary_active() %>%
        transmute(
          Partner = partner_label,
          `Original Target` = target_sample, `Revised Target` = target_sample_current,
          Collected = collected_n, `Confirmed Deleted` = confirmed_deletion_n, `Oversampling Surplus` = oversampling_surplus_n,
          `Pending Deletion` = pending_deletion_n,
          Achieved = achieved_n,
          `Credited toward target` = credited_achieved_n,
          `Still needed` = remaining_n,
          `% achieved` = pct_achieved,
          `Shared with` = shared_with,
          `Start date` = start_date,
          `Current daily pace` = round(current_daily_pace, 1),
          `Required daily pace` = round(required_daily_pace, 1),
          `Projected finish` = projected_finish_date,
          Status = factor(status, levels = names(partner_pace_colors))
        )
      if (!HAS_SHARED_COVERAGE) df <- df %>% select(-`Shared with`)

      num_cols <- c("Original Target", "Revised Target", "Collected", "Confirmed Deleted", "Oversampling Surplus",
                    "Pending Deletion", "Achieved", "Credited toward target", "Still needed", "% achieved",
                    "Current daily pace", "Required daily pace")
      datatable(
        df, rownames = FALSE, filter = "top",
        options = list(pageLength = 20, scrollX = TRUE, order = list(list(which(names(df) == "Still needed") - 1L, "desc")),
                       columnDefs = list(list(className = "dt-right", targets = which(names(df) %in% num_cols) - 1L)))
      ) %>%
        formatRound(c("Original Target", "Revised Target"), 0) %>%
        formatPercentage("% achieved", 1) %>%
        formatStyle(
          "Status",
          color = styleEqual(names(partner_pace_colors), unname(partner_pace_colors))
        ) %>%
        formatStyle(
          "% achieved",
          background = styleColorBar(c(0, 1), "#CFE0F5"),
          backgroundSize = "90% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "left"
        )
    })
  })
}
