# Data Export: download whatever is currently in view, without respondent details.

mod_export_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Data Export",
    icon = icon("download"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Filtered submissions"),
        p(class = "text-muted", "Every submission-level column currently in scope (follows all sidebar filters): every raw submission, including consent refusals and duplicate copies. This is a larger number than both \"Collected\" (completed interviews only) and \"Achieved\" (completed, matched and not deleted in cleaning) elsewhere in the dashboard; see the Home page for those definitions. Respondent fields (age, gender, household size, exact GPS) are left out; Sample Representativeness shows the same information in aggregate."),
        uiOutput(ns("subs_count")),
        downloadButton(ns("dl_subs_csv"), "Download CSV", class = "btn-outline-primary w-100 mb-2"),
        downloadButton(ns("dl_subs_xlsx"), "Download Excel", class = "btn-outline-primary w-100")
      ),
      card(
        card_header("Filtered LGA x population-group progress"),
        p(class = "text-muted", "Target, achieved and status per stratum, the same scope as the Progress by LGA table."),
        uiOutput(ns("stratum_count")),
        downloadButton(ns("dl_stratum_csv"), "Download CSV", class = "btn-outline-primary w-100 mb-2"),
        downloadButton(ns("dl_stratum_xlsx"), "Download Excel", class = "btn-outline-primary w-100")
      )
    ),
    card(
      card_header("Preview (first 200 filtered submissions)"),
      DTOutput(ns("preview"))
    )
  )
}

mod_export_server <- function(id, filtered_subs, filtered_stratum) {
  moduleServer(id, function(input, output, session) {

    exportable_subs <- reactive(strip_row_level_pii(filtered_subs()))

    output$subs_count <- renderUI(p(strong(comma(nrow(filtered_subs()))), " submissions in scope"))
    output$stratum_count <- renderUI(p(strong(comma(nrow(filtered_stratum()))), " stratum rows in scope"))

    output$preview <- renderDT({
      datatable(exportable_subs() %>% head(200), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$dl_subs_csv <- downloadHandler(
      filename = function() paste0("MSNA_2026_synthetic_submissions_filtered_", Sys.Date(), ".csv"),
      content = function(file) write_csv(exportable_subs(), file)
    )
    output$dl_subs_xlsx <- downloadHandler(
      filename = function() paste0("MSNA_2026_synthetic_submissions_filtered_", Sys.Date(), ".xlsx"),
      content = function(file) writeData_wrapper(exportable_subs(), file, "Submissions")
    )
    output$dl_stratum_csv <- downloadHandler(
      filename = function() paste0("MSNA_2026_synthetic_lga_progress_filtered_", Sys.Date(), ".csv"),
      content = function(file) write_csv(filtered_stratum(), file)
    )
    output$dl_stratum_xlsx <- downloadHandler(
      filename = function() paste0("MSNA_2026_synthetic_lga_progress_filtered_", Sys.Date(), ".xlsx"),
      content = function(file) writeData_wrapper(filtered_stratum(), file, "LGA progress")
    )
  })
}

# One sheet with a filterable table, plus the synthetic-data label above it
writeData_wrapper <- function(df, file, sheet_name) {
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, SYNTHETIC_LABEL, startRow = 1)
  addStyle(wb, sheet_name, createStyle(textDecoration = "bold", fontColour = "#C1443C"), rows = 1, cols = 1)
  writeDataTable(wb, sheet_name, df, startRow = 2, tableStyle = "TableStyleLight9")
  setColWidths(wb, sheet_name, cols = seq_along(df), widths = "auto")
  freezePane(wb, sheet_name, firstActiveRow = 3)
  saveWorkbook(wb, file, overwrite = TRUE)
}
