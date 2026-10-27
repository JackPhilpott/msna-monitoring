# Enumerator Performance: who is collecting, how fast, and how often flagged.

mod_enumerator_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Enumerator Performance",
    icon = icon("users"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Active enumerators", value = textOutput(ns("kpi_n_enum")), showcase = icon("id-badge"), theme = "primary"),
      value_box(title = "Avg. submissions / enumerator", value = textOutput(ns("kpi_avg_subs")), showcase = icon("chart-simple"), theme = "secondary"),
      value_box(title = "Busiest single day (any enumerator)", value = textOutput(ns("kpi_max_day")), showcase = icon("bolt"), theme = "warning"),
      value_box(title = "Median flag rate", value = textOutput(ns("kpi_median_flag")), showcase = icon("flag"), theme = "warning")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Duration vs. flag rate", span(class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;", "Bubble size = submissions. Fast enumerators with high flag rates (top left, large, red) are worth a closer look.")),
        plotlyOutput(ns("scatter"), height = "460px")
      ),
      card(
        card_header("Top 15 by submissions"),
        plotlyOutput(ns("top_bar"), height = "460px")
      )
    ),
    card(
      card_header("Enumerator drill-down"),
      layout_columns(
        col_widths = c(4, 8),
        selectInput(ns("drill_enum"), "Enumerator", choices = NULL),
        plotlyOutput(ns("drill_trend"), height = "340px")
      )
    ),
    card(
      card_header("Leaderboard"),
      DTOutput(ns("table"))
    )
  )
}

mod_enumerator_server <- function(id, filtered_subs) {
  moduleServer(id, function(input, output, session) {
    stats <- reactive(compute_enumerator_stats(filtered_subs()))

    observe({
      s <- stats() %>% arrange(desc(submissions))
      updateSelectInput(session, "drill_enum", choices = s$enum_id, selected = if (nrow(s) > 0) s$enum_id[1] else NULL)
    })

    output$kpi_n_enum <- renderText(comma(nrow(stats())))
    output$kpi_avg_subs <- renderText(fmt_num(mean(stats()$submissions)))
    output$kpi_max_day <- renderText(ifelse(nrow(stats()) > 0, max(stats()$max_in_a_day), "-"))
    output$kpi_median_flag <- renderText(fmt_pct(median(stats()$flag_rate, na.rm = TRUE)))

    output$scatter <- renderPlotly({
      s <- stats()
      if (nrow(s) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
      # bubble area follows submissions; sizes set directly, as plotly's size mapping warns on markers
      s$bubble <- if (diff(range(s$submissions)) > 0) rescale(sqrt(s$submissions), to = c(8, 28)) else 12
      plot_ly(
        s, x = ~avg_duration, y = ~flag_rate,
        color = ~flag_rate, colors = c("#4C9A6A", "#D99A2B", "#C1443C"),
        marker = list(size = ~bubble, opacity = 0.7, line = list(color = "#FFFFFF", width = 1)),
        type = "scatter", mode = "markers",
        text = ~paste0(enum_id, "<br>", org_label, ", ", state, "<br>Submissions: ", submissions, "<br>Avg duration: ", round(avg_duration, 1), " min<br>Flag rate: ", percent(flag_rate, 1)),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "Avg. interview duration (min)"),
          yaxis = list(title = "Flag rate", tickformat = ".0%"),
          showlegend = FALSE
        )
    })

    output$top_bar <- renderPlotly({
      s <- stats() %>% arrange(desc(submissions)) %>% head(15)
      plot_ly(s, x = ~submissions, y = ~reorder(enum_id, submissions), type = "bar", orientation = "h", marker = list(color = "#1B2A4A")) %>%
        layout(xaxis = list(title = "Submissions"), yaxis = list(title = ""))
    })

    output$drill_trend <- renderPlotly({
      req(input$drill_enum)
      df <- filtered_subs() %>% filter(enum_id == input$drill_enum, !is_duplicate) %>% count(submission_date, name = "n")
      if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
      plot_ly(df, x = ~submission_date, y = ~n, type = "bar", marker = list(color = "#4C7FD9")) %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Submissions / day"))
    })

    output$table <- renderDT({
      df <- stats() %>%
        transmute(
          Enumerator = enum_id, Partner = factor(org_label), State = factor(state),
          Submissions = submissions, Completed = completed, `Consent refused` = consent_refused,
          `Avg. duration (min)` = round(avg_duration, 1),
          `Days active` = days_active, `Avg./active day` = round(avg_per_active_day, 1),
          `Busiest day` = max_in_a_day,
          `Flag rate` = flag_rate
        ) %>%
        arrange(desc(`Flag rate`))

      datatable(df, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE, order = list(list(10, "desc")))) %>%
        formatPercentage("Flag rate", 1) %>%
        formatStyle("Busiest day", backgroundColor = styleInterval(MAX_PLAUSIBLE_INTERVIEWS_PER_DAY, c("white", "#F7D6D3"))) %>%
        formatStyle("Flag rate", background = styleColorBar(c(0, max(df$`Flag rate`, 0.01)), "#F7D6D3"), backgroundSize = "90% 70%", backgroundRepeat = "no-repeat")
    })
  })
}
