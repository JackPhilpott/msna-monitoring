# Partner Report: one partner's progress across its LGAs, with an Excel or PDF
# version to send to that partner.

mod_partner_report_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Partner Report",
    icon = icon("file-export"),
    div(class = "text-muted", style = "font-size: 0.8em; margin-bottom: 8px;", AS_OF_CAPTION),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Select partner"),
        selectInput(ns("report_partner"), NULL, choices = org_id_choices, selected = unname(org_id_choices)[1]),
        p(class = "text-muted", style = "font-size: 0.85em;",
          "Follows the first partner selected in the sidebar filter. The download buttons always export the partner chosen here."),
        downloadButton(ns("download_xlsx"), "Download Excel report", class = "btn-outline-primary w-100 mb-2"),
        downloadButton(ns("download_pdf"), "Download PDF report", class = "btn-outline-primary w-100")
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3, 3, 3, 3, 3),
        height = "480px",
        value_box(title = info_title("Original Target (their LGAs)", "The fixed sample size set at the start, across this partner's assigned LGAs."), value = textOutput(ns("kpi_target_original")), showcase = icon("bullseye"), theme = "secondary"),
        value_box(title = info_title("Revised Target (their LGAs)", "The current minimum needed across this partner's assigned LGAs, recalculated on the population still accessible."), value = textOutput(ns("kpi_target")), showcase = icon("bullseye"), theme = "primary"),
        value_box(
          title = info_title("Achieved", "Interviews that count toward target. The % follows the sidebar's Target basis toggle."),
          value = textOutput(ns("kpi_achieved")), showcase = icon("clipboard-check"), theme = "success"
        ),
        value_box(
          title = info_title("Collected", "Every completed interview, including any later deleted: total field effort, not what counts toward target."),
          value = textOutput(ns("kpi_collected")), showcase = icon("layer-group"), theme = "warning"
        ),
        value_box(
          title = info_title("Confirmed Deleted", "Interviews in this partner's LGAs removed in cleaning."),
          value = textOutput(ns("kpi_confirmed_deletion")), showcase = icon("trash"), theme = "danger"
        ),
        value_box(
          title = info_title("Pending Deletion", "How much of Achieved still has an open follow-up flag that could become a deletion."),
          value = textOutput(ns("kpi_pending_deletion")), showcase = icon("hourglass-half"), theme = "warning"
        ),
        value_box(title = "Flagged for review", value = textOutput(ns("kpi_flagged")), showcase = icon("flag"), theme = "warning"),
        value_box(
          title = info_title("Oversampled clusters", "Clusters where this partner's interviews pushed achieved past the cluster's target. They count toward Achieved in full, but are worth reviewing for representativity."),
          value = textOutput(ns("kpi_oversampled")), showcase = icon("triangle-exclamation"), theme = "warning"
        )
      )
    ),
    card(
      card_header(
        "Progress by LGA (their assigned coverage area)",
        info_icon("Achieved = interviews that count toward target. Collected = every completed interview, including any later deleted."),
        if (HAS_SHARED_COVERAGE) {
          span(
            class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;",
            "Some LGAs are covered by more than one partner (see \"Shared with\"); Achieved there combines everyone's interviews."
          )
        }
      ),
      DTOutput(ns("lga_table"))
    )
  )
}

mod_partner_report_server <- function(id, selected_partners, target_basis) {
  moduleServer(id, function(input, output, session) {
    observeEvent(selected_partners(), {
      sp <- selected_partners()
      if (length(sp) >= 1) updateSelectInput(session, "report_partner", selected = sp[1])
    }, ignoreInit = TRUE)

    lga_df <- reactive({
      req(input$report_partner)
      partner_progress_by_lga(input$report_partner, target_basis())
    })

    qual <- reactive({
      req(input$report_partner)
      partner_quality_summary(input$report_partner)
    })

    output$kpi_target_original <- renderText(comma(sum(lga_df()$target_sample)))
    output$kpi_target <- renderText({
      orig <- sum(lga_df()$target_sample)
      rev <- sum(lga_df()$target_sample_current)
      d <- target_delta_label(orig, rev)
      if (d == "") comma(rev) else paste0(comma(rev), " (", d, ")")
    })
    output$kpi_achieved <- renderText({
      denom <- sum(lga_df()$target_active)
      pct <- if (denom > 0) sum(lga_df()$credited_achieved_n) / denom else NA_real_
      paste0(comma(sum(lga_df()$achieved_n)), " (", fmt_pct(pct), " credited; ", comma(sum(lga_df()$remaining_n)), " still needed)")
    })
    output$kpi_collected <- renderText(comma(sum(lga_df()$collected_n)))
    output$kpi_confirmed_deletion <- renderText(comma(sum(lga_df()$confirmed_deletion_n)))
    output$kpi_pending_deletion <- renderText(comma(sum(lga_df()$pending_deletion_n)))
    output$kpi_flagged <- renderText({
      q <- qual()
      paste0(comma(q$flagged), " (", fmt_pct(q$flag_rate), ")")
    })

    partner_oversampled <- reactive({
      req(input$report_partner)
      oversampled_clusters[
        vapply(strsplit(oversampled_clusters$submitting_org_ids, ", "), function(x) input$report_partner %in% x, logical(1)),
      ]
    })
    output$kpi_oversampled <- renderText({
      o <- partner_oversampled()
      paste0(comma(nrow(o)), " (", comma(sum(o$surplus)), " surplus)")
    })

    output$lga_table <- renderDT({
      df <- partner_lga_table(lga_df()) %>%
        mutate(Region = factor(Region), State = factor(State), Status = factor(Status, levels = names(STRATUM_STATUS_COLORS)))
      datatable(df, rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE)) %>%
        formatRound(c("Original Target", "Revised Target"), 0) %>%
        formatPercentage("% achieved", 1) %>%
        formatPercentage("Δ vs Original", 1) %>%
        formatStyle(
          "Δ vs Original",
          backgroundColor = styleInterval(
            c(-TARGET_DIVERGENCE_THRESHOLD, TARGET_DIVERGENCE_THRESHOLD),
            c("#FCE8CF", "#FFFFFF", "#FCE8CF")
          )
        ) %>%
        formatStyle("Status",
                    backgroundColor = styleEqual(names(STRATUM_STATUS_COLORS), unname(STRATUM_STATUS_COLORS)),
                    color = styleEqual(names(STRATUM_STATUS_COLORS), c("white", "#1a1a1a", "white", "white")))
    })

    output$download_xlsx <- downloadHandler(
      filename = function() paste0("MSNA_2026_synthetic_partner_report_", input$report_partner, ".xlsx"),
      content = function(file) build_partner_excel(input$report_partner, file, target_basis())
    )

    output$download_pdf <- downloadHandler(
      filename = function() paste0("MSNA_2026_synthetic_partner_report_", input$report_partner, ".pdf"),
      content = function(file) build_partner_pdf(input$report_partner, file, target_basis())
    )
  })
}

# The LGA table shared by the page and the Excel report
partner_lga_table <- function(lga_df) {
  df <- lga_df %>%
    transmute(
      Region = region, State = adm1_name, LGA = adm2_name,
      `Original Target` = target_sample, `Revised Target` = target_sample_current,
      `Δ vs Original` = target_delta_pct(target_sample, target_sample_current),
      Collected = collected_n, `Confirmed Deleted` = confirmed_deletion_n, `Pending Deletion` = pending_deletion_n,
      Achieved = achieved_n,
      `Credited toward target` = credited_achieved_n,
      `Still needed` = remaining_n,
      `% achieved` = pct_achieved,
      Status = status,
      `Shared with` = shared_with
    )
  if (!HAS_SHARED_COVERAGE) df <- df %>% select(-`Shared with`)
  df
}

partner_totals <- function(lga_df) {
  list(
    target = sum(lga_df$target_sample), target_current = sum(lga_df$target_sample_current),
    active = sum(lga_df$target_active), achieved = sum(lga_df$achieved_n),
    credited = sum(lga_df$credited_achieved_n), remaining = sum(lga_df$remaining_n),
    collected = sum(lga_df$collected_n), confirmed = sum(lga_df$confirmed_deletion_n),
    pending = sum(lga_df$pending_deletion_n), surplus = sum(lga_df$oversampling_surplus_n)
  )
}

PARTNER_REPORT_NOTES <- c(
  "Achieved = completed interviews matched to a sampled cluster and not deleted in cleaning, measured against the target basis chosen on the dashboard when this report was made. A flag for follow-up does not remove an interview from Achieved; only a confirmed deletion does. Interviews past a stratum's own target count in full.",
  "Collected = every completed interview done, including any later deleted.",
  "Confirmed Deleted = removed in cleaning. Pending Deletion (for information, already inside Achieved) = interviews with an open follow-up flag that could still become a deletion. Collected = Achieved + Confirmed Deleted + Oversampling Surplus.",
  "Original Target = the fixed design total. Revised Target = the current minimum needed on the population still accessible; it can rise or fall as access changes.",
  "Credited toward target / Still needed: Achieved is capped at each stratum's (LGA x population group) own target before LGA and partner totals are summed, so a surplus in one stratum never offsets a shortfall in another, and the credited % cannot read 100% while any stratum is still short."
)

build_partner_excel <- function(org_id_val, file, target_basis = "original") {
  lga_df <- partner_progress_by_lga(org_id_val, target_basis)
  qual <- partner_quality_summary(org_id_val)
  t <- partner_totals(lga_df)
  pct_achieved_summary <- if (t$active > 0) t$credited / t$active else NA_real_

  wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  writeData(
    wb, "Summary",
    data.frame(
      Field = c("Partner", "Data", "Report generated", "Original Target interviews (their LGAs)", "Revised Target interviews (their LGAs)",
                "Achieved interviews (all, incl. surplus)", "Credited toward target (capped per stratum)", "Still needed (sum of each stratum's own gap)",
                "Collected interviews", "Confirmed Deleted", "Oversampling Surplus", "Pending Deletion (for information, included in Achieved)",
                "% achieved (credited)", "Submissions logged", "Flagged for review", "Flag rate", "Consent refusals"),
      Value = c(
        ORG_LABELS[[org_id_val]], SYNTHETIC_LABEL, format(Sys.time(), "%d %b %Y %H:%M"), comma(t$target), comma(t$target_current),
        comma(t$achieved), comma(t$credited), comma(t$remaining),
        comma(t$collected), comma(t$confirmed), comma(t$surplus), comma(t$pending),
        fmt_pct(pct_achieved_summary), comma(qual$submissions), comma(qual$flagged),
        fmt_pct(qual$flag_rate), comma(qual$consent_refused)
      )
    ),
    colNames = FALSE
  )
  setColWidths(wb, "Summary", cols = 1:2, widths = c(48, 40))
  addStyle(wb, "Summary", createStyle(textDecoration = "bold"), rows = 1:17, cols = 1)
  addStyle(wb, "Summary", createStyle(textDecoration = "bold", fontColour = "#C1443C"), rows = 2, cols = 2)
  writeData(wb, "Summary", paste0("Measured against the ", target_basis_label(target_basis), ". ", paste(PARTNER_REPORT_NOTES, collapse = " ")),
            startRow = 19)
  addStyle(wb, "Summary", createStyle(fontSize = 9, textDecoration = "italic", fontColour = "#666666", wrapText = TRUE), rows = 19, cols = 1)

  sheet2 <- "Progress by LGA"
  addWorksheet(wb, sheet2)
  export_df <- partner_lga_table(lga_df) %>%
    mutate(`Original Target` = round(`Original Target`), `Revised Target` = round(`Revised Target`))
  writeDataTable(wb, sheet2, export_df, tableStyle = "TableStyleLight9")
  pct_cols <- which(names(export_df) %in% c("% achieved", "Δ vs Original"))
  status_col <- which(names(export_df) == "Status")
  addStyle(wb, sheet2, createStyle(numFmt = "0%"), rows = 2:(nrow(export_df) + 1), cols = pct_cols, gridExpand = TRUE, stack = TRUE)
  for (s in names(STRATUM_STATUS_COLORS)) {
    rows <- which(export_df$Status == s) + 1
    if (length(rows) > 0) {
      addStyle(wb, sheet2, createStyle(fgFill = STRATUM_STATUS_COLORS[[s]]), rows = rows, cols = status_col, gridExpand = TRUE, stack = TRUE)
    }
  }
  setColWidths(wb, sheet2, cols = 1:ncol(export_df), widths = "auto")
  freezePane(wb, sheet2, firstRow = TRUE)

  saveWorkbook(wb, file, overwrite = TRUE)
}

build_partner_pdf <- function(org_id_val, file, target_basis = "original") {
  lga_df <- partner_progress_by_lga(org_id_val, target_basis)
  qual <- partner_quality_summary(org_id_val)
  t <- partner_totals(lga_df)
  pct <- if (t$active > 0) t$credited / t$active else NA_real_
  pct_raw <- if (t$active > 0) t$achieved / t$active else NA_real_

  header_text <- paste0(
    ORG_LABELS[[org_id_val]], "\nMSNA 2026 — Partner progress report\n", SYNTHETIC_LABEL,
    "\nGenerated: ", format(Sys.time(), "%d %b %Y %H:%M"),
    "\n\nOriginal Target: ", comma(t$target), "     Revised Target: ", comma(t$target_current),
    "\nCredited toward target: ", comma(t$credited), " (", fmt_pct(pct), " of ", target_basis_label(target_basis), ")     Still needed: ", comma(t$remaining),
    "\nAchieved (all interviews incl. surplus): ", comma(t$achieved), " (", fmt_pct(pct_raw), " raw)", "     Collected: ", comma(t$collected),
    "\nConfirmed Deleted: ", comma(t$confirmed), "     Oversampling Surplus: ", comma(t$surplus),
    "\nPending Deletion (for information, included in Achieved above): ", comma(t$pending),
    "\nSubmissions logged: ", comma(qual$submissions), "     Flagged for review: ", comma(qual$flagged), " (", fmt_pct(qual$flag_rate), ")"
  )
  header_plot <- ggplot() + theme_void() + xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0, y = 1, label = header_text, hjust = 0, vjust = 1, size = 4)

  bar_plot <- ggplot(lga_df, aes(x = reorder(adm2_name, pct_achieved), y = pmin(pct_achieved, 1.5), fill = status)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = percent, limits = c(0, max(1, max(lga_df$pct_achieved, na.rm = TRUE), na.rm = TRUE))) +
    scale_fill_manual(values = STRATUM_STATUS_COLORS, drop = FALSE) +
    labs(x = NULL, y = "% of target achieved (credited)", fill = "Status", title = "Progress by LGA",
         caption = SYNTHETIC_LABEL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")

  focus <- lga_df %>% filter(!status %in% c("Complete", "Dropped")) %>% arrange(pct_achieved) %>% head(10)
  focus_text <- if (nrow(focus) == 0) {
    "Focus areas: none, every assigned LGA is complete."
  } else {
    paste0(
      "Focus areas (lowest % achieved):\n",
      paste0("  - ", focus$adm2_name, " (", fmt_pct(focus$pct_achieved), " credited, ", focus$credited_achieved_n, "/", comma(round(focus$target_active)), ", ", focus$remaining_n, " still needed)", collapse = "\n")
    )
  }
  shared <- lga_df %>% filter(shared_with != "")
  if (nrow(shared) > 0) {
    focus_text <- paste0(
      focus_text, "\n\nCovered jointly with another partner (Achieved combines both):\n",
      paste0("  - ", shared$adm2_name, " (with ", shared$shared_with, ")", collapse = "\n")
    )
  }
  focus_plot <- ggplot() + theme_void() + xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0, y = 1, label = focus_text, hjust = 0, vjust = 1, size = 3.6, family = "mono")

  combined <- cowplot::plot_grid(header_plot, bar_plot, focus_plot, ncol = 1, rel_heights = c(0.22, 0.50, 0.28))
  ggsave(file, combined, width = 8.27, height = 11.69, units = "in", device = "pdf")
}
