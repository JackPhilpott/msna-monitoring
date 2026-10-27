# MSNA data collection monitoring dashboard (synthetic demo).
#
# Run from the repo root with shiny::runApp("dashboard_app"). Open a page
# directly with ?page=<title>, for example ?page=Coverage%20Map.

source("global.R", local = FALSE)

GLOBAL_CSS <- tags$style(HTML("
  .bslib-value-box { min-height: 132px; }
  .bslib-value-box .value-box-value { white-space: normal; word-break: break-word; line-height: 1.25; font-size: 1.3rem; }
  .bslib-value-box .value-box-title { white-space: normal; }
  /* DT sizes each column-filter dropdown to its column, which wraps short options
     over several lines; let the dropdown grow past a narrow column instead. */
  table.dataTable thead tr.filters th { overflow: visible; }
  table.dataTable thead tr.filters select {
    width: auto !important;
    min-width: 110px;
  }
"))

FINAL_BANNER <- div(
  class = "alert alert-primary",
  style = "margin-bottom: 8px; padding: 6px 12px; font-size: 0.95em;",
  span(class = "badge text-bg-warning", style = "font-size: 0.9em; margin-right: 6px;", icon("flask"), " ", SYNTHETIC_LABEL),
  icon("flag-checkered"), " ",
  strong(paste0("Data collection closed on ", fmt_long_date(COLLECTION_CLOSED_DATE), ".")),
  " This is the final overview of the collected data. Every figure comes from the synthetic pipeline in this repo."
)

picker_opts <- function(...) pickerOptions(
  actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3",
  countSelectedText = "{0} of {1} selected", noneSelectedText = "Nothing selected",
  size = 10, container = "body", ...
)

filter_sidebar <- sidebar(
  title = "Filters",
  width = 300,

  div(
    style = "background: rgba(255,255,255,0.08); border-radius: 6px; padding: 8px 10px;",
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      strong("Target basis"),
      info_icon(
        "Switches which target every % and status on the dashboard is measured against. Original = the fixed target set at the start of collection. Revised = the current minimum needed sample, recalculated on the population still accessible. Both are always shown for reference wherever they appear; this only changes which one drives the headline figures.",
        color = THEME_SIDEBAR_FG
      )
    ),
    radioButtons(
      "target_basis", NULL,
      choices = c("Original Target" = "original", "Revised Target" = "revised"),
      selected = "original", inline = TRUE
    )
  ),
  actionButton("reset_filters", "Reset all filters", icon = icon("rotate-left"), class = "btn-secondary btn-sm w-100 mb-2"),
  pickerInput(
    "f_region", "Region",
    choices = region_choices, selected = unname(region_choices),
    multiple = TRUE, options = picker_opts()
  ),
  pickerInput(
    "f_state", "State",
    choices = state_choices, selected = state_choices,
    multiple = TRUE, options = picker_opts()
  ),
  pickerInput(
    "f_lga", "LGA",
    choices = compute_filter_choices("lga", list()), selected = compute_filter_choices("lga", list()),
    multiple = TRUE, options = picker_opts()
  ),
  pickerInput(
    "f_ward", "Ward",
    choices = get_ward_choices(compute_filter_choices("lga", list())),
    selected = get_ward_choices(compute_filter_choices("lga", list())),
    multiple = TRUE, options = picker_opts(virtualScroll = 50)
  ),
  checkboxGroupInput(
    "f_poptype", "Population group",
    choices = pop_type_choices, selected = unname(pop_type_choices)
  ),
  pickerInput(
    "f_partner", "Partner",
    choices = org_id_choices, selected = unname(org_id_choices),
    multiple = TRUE, options = picker_opts()
  ),
  dateRangeInput(
    "f_daterange", "Submission date range",
    start = FIELDING_START,
    end = max(submissions_raw$submission_date, na.rm = TRUE),
    min = FIELDING_START,
    max = max(submissions_raw$submission_date, na.rm = TRUE)
  ),
  p(style = "font-size: 0.8em; opacity: 0.8; margin-top: -4px;",
    "Move the end date back to see collection as it stood on that day."),
  hr(),
  bg = THEME_SIDEBAR_BG, fg = THEME_SIDEBAR_FG
)

ui <- tagList(
  GLOBAL_CSS,
  page_navbar(
    id = "main_nav",
    title = "MSNA 2026 — Data collection overview",
    window_title = "MSNA 2026 monitoring (synthetic demo)",
    theme = bs_theme(version = 5, primary = "#1B2A4A", success = "#1E7B4D", warning = "#D99A2B", danger = "#C1443C"),
    navbar_options = navbar_options(bg = THEME_NAVBAR_BG, theme = "dark"),
    sidebar = filter_sidebar,
    header = FINAL_BANNER,
    fillable = c(
      "Coverage Map", "Progress by LGA", "Data Quality", "Partner Report", "Data Export",
      "Enumerator Performance", "Data Integrity Checks", "Sample Representativeness"
    ),
    mod_home_ui("home"),
    mod_progress_ui("progress"),
    mod_map_ui("map"),
    mod_table_ui("table"),
    mod_quality_ui("quality"),
    mod_partner_report_ui("partner_report"),
    mod_export_ui("export"),
    nav_menu(
      "Analysis",
      icon = icon("magnifying-glass-chart"),
      mod_enumerator_ui("enumerator"),
      mod_integrity_ui("integrity"),
      mod_representativeness_ui("representativeness")
    ),
    footer = div(
      style = "font-size: 0.8em; color: #888; padding: 6px 16px;",
      "MSNA 2026 monitoring dashboard · ", SYNTHETIC_LABEL, " · ",
      tags$a(href = "https://github.com/JackPhilpott/msna-monitoring", target = "_blank", "Source code")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(session$clientData$url_search, {
    page <- parseQueryString(session$clientData$url_search)$page
    if (!is.null(page)) nav_select("main_nav", page)
  }, once = TRUE)

  # What each filter currently offers and has selected. Filters narrow each
  # other (choosing a state limits the LGAs, and so on); one the user has
  # touched keeps its selection where still valid, an untouched one follows
  # the others and stays "all".
  filter_ui_state <- reactiveValues(
    region = list(choices = unname(region_choices), selected = unname(region_choices)),
    state = list(choices = state_choices, selected = state_choices),
    lga = list(choices = compute_filter_choices("lga", list()), selected = compute_filter_choices("lga", list())),
    poptype = list(choices = unname(pop_type_choices), selected = unname(pop_type_choices)),
    partner = list(choices = unname(org_id_choices), selected = unname(org_id_choices)),
    ward = list(choices = get_ward_choices(compute_filter_choices("lga", list())), selected = get_ward_choices(compute_filter_choices("lga", list())))
  )

  # Only push an update to the browser when something changed, to avoid update loops
  sync_filter_input <- function(key, input_id, new_choices, new_selected, updater = updatePickerInput) {
    prev <- isolate(filter_ui_state[[key]])
    if (!setequal(unname(new_choices), unname(prev$choices)) || !setequal(new_selected, prev$selected)) {
      updater(session, input_id, choices = new_choices, selected = new_selected)
      filter_ui_state[[key]] <- list(choices = new_choices, selected = new_selected)
    }
  }

  filter_touched <- reactiveValues(region = FALSE, state = FALSE, lga = FALSE, ward = FALSE, poptype = FALSE, partner = FALSE)

  watch_for_manual_touch <- function(key, input_id) {
    observeEvent(input[[input_id]], {
      live <- input[[input_id]]
      prev <- isolate(filter_ui_state[[key]])
      if (is.null(prev$selected) || !setequal(live, prev$selected)) {
        filter_touched[[key]] <- TRUE
        filter_ui_state[[key]] <- list(choices = prev$choices, selected = live)
      }
    }, ignoreInit = TRUE)
  }
  watch_for_manual_touch("region", "f_region")
  watch_for_manual_touch("state", "f_state")
  watch_for_manual_touch("lga", "f_lga")
  watch_for_manual_touch("ward", "f_ward")
  watch_for_manual_touch("poptype", "f_poptype")
  watch_for_manual_touch("partner", "f_partner")

  resolve_selection <- function(key, new_choices) {
    new_values <- unname(new_choices)
    current <- isolate(filter_ui_state[[key]]$selected)
    if (isolate(filter_touched[[key]])) smart_selection(current, new_values) else new_values
  }

  observeEvent(input$reset_filters, {
    for (k in names(filter_touched)) filter_touched[[k]] <- FALSE
    sync_filter_input("region", "f_region", region_choices, unname(region_choices))
    sync_filter_input("state", "f_state", state_choices, state_choices)
    all_lgas <- compute_filter_choices("lga", list())
    sync_filter_input("lga", "f_lga", all_lgas, all_lgas)
    all_wards <- get_ward_choices(all_lgas)
    sync_filter_input("ward", "f_ward", all_wards, all_wards)
    sync_filter_input("poptype", "f_poptype", pop_type_choices, unname(pop_type_choices), updater = updateCheckboxGroupInput)
    sync_filter_input("partner", "f_partner", org_id_choices, unname(org_id_choices))
    updateDateRangeInput(
      session, "f_daterange",
      start = FIELDING_START, end = max(submissions_raw$submission_date, na.rm = TRUE)
    )
  })

  observe({
    touched <- list(
      region = filter_touched$region, state = filter_touched$state, lga = filter_touched$lga,
      poptype = filter_touched$poptype, partner = filter_touched$partner
    )
    sel <- list(
      region = filter_ui_state$region$selected, state = filter_ui_state$state$selected,
      lga = filter_ui_state$lga$selected, poptype = filter_ui_state$poptype$selected,
      partner = filter_ui_state$partner$selected
    )
    only_if_touched <- function(key) if (touched[[key]]) sel[[key]] else NULL

    choices <- list(
      region  = compute_filter_choices("region",  list(poptype = only_if_touched("poptype"), partner = only_if_touched("partner"))),
      state   = compute_filter_choices("state",   list(region = only_if_touched("region"), poptype = only_if_touched("poptype"), partner = only_if_touched("partner"))),
      lga     = compute_filter_choices("lga",     list(region = only_if_touched("region"), state = only_if_touched("state"), poptype = only_if_touched("poptype"), partner = only_if_touched("partner"))),
      poptype = compute_filter_choices("poptype", list(region = only_if_touched("region"), state = only_if_touched("state"), lga = only_if_touched("lga"), partner = only_if_touched("partner"))),
      partner = compute_filter_choices("partner", list(region = only_if_touched("region"), state = only_if_touched("state"), lga = only_if_touched("lga"), poptype = only_if_touched("poptype")))
    )
    new_sel <- list(
      region  = if (touched$region)  smart_selection(sel$region,  unname(choices$region))  else unname(choices$region),
      state   = if (touched$state)   smart_selection(sel$state,   unname(choices$state))   else unname(choices$state),
      lga     = if (touched$lga)     smart_selection(sel$lga,     unname(choices$lga))     else unname(choices$lga),
      poptype = if (touched$poptype) smart_selection(sel$poptype, unname(choices$poptype)) else unname(choices$poptype),
      partner = if (touched$partner) smart_selection(sel$partner, unname(choices$partner)) else unname(choices$partner)
    )

    sync_filter_input("region", "f_region", choices$region, new_sel$region)
    sync_filter_input("state", "f_state", choices$state, new_sel$state)
    sync_filter_input("lga", "f_lga", choices$lga, new_sel$lga)
    sync_filter_input("poptype", "f_poptype", choices$poptype, new_sel$poptype, updater = updateCheckboxGroupInput)
    sync_filter_input("partner", "f_partner", choices$partner, new_sel$partner)
  })

  observe({
    new_wards <- get_ward_choices(filter_ui_state$lga$selected)
    selected <- resolve_selection("ward", new_wards)
    sync_filter_input("ward", "f_ward", new_wards, selected)
  })

  # LGAs still in scope once the ward filter is applied
  effective_lgas <- reactive({
    intersect(filter_ui_state$lga$selected, ward_to_lga$adm2_name[ward_to_lga$adm3_name %in% filter_ui_state$ward$selected])
  })

  selected_partner_adm2 <- reactive(unique(unlist(partner_adm2[filter_ui_state$partner$selected])))

  filtered_subs <- reactive({
    poptype_sel <- filter_ui_state$poptype$selected
    req(poptype_sel, input$f_daterange)
    submissions_raw %>%
      filter(
        # submissions with a place name outside the frame stay in view rather than vanish
        is.na(admin1) | !(admin1 %in% KNOWN_STATE_NAMES) | admin1 %in% filter_ui_state$state$selected,
        is.na(admin2_submitted) | !(admin2_submitted %in% KNOWN_LGA_NAMES) | admin2_submitted %in% effective_lgas(),
        is.na(admin3_submitted) | !(admin3_submitted %in% KNOWN_WARD_NAMES) | admin3_submitted %in% filter_ui_state$ward$selected,
        is.na(pop_type) | pop_type %in% poptype_sel,
        is.na(submission_date) | submission_date >= input$f_daterange[1],
        is.na(submission_date) | submission_date <= input$f_daterange[2],
        org_id %in% filter_ui_state$partner$selected
      )
  })

  target_basis <- reactive(input$target_basis)
  as_of <- reactive(min(input$f_daterange[2], DATA_AS_OF_DATE))

  filtered_stratum <- reactive({
    poptype_sel <- filter_ui_state$poptype$selected
    req(poptype_sel)
    compute_progress_by_stratum(filtered_subs(), target_basis()) %>%
      filter(
        adm1_name %in% filter_ui_state$state$selected,
        adm2_name %in% effective_lgas(),
        pop_type %in% poptype_sel,
        adm2_pcode %in% selected_partner_adm2()
      )
  })

  map_tab_active <- reactive(identical(input$main_nav, "Coverage Map"))

  mod_home_server("home", target_basis)
  mod_progress_server("progress", filtered_subs, filtered_stratum, target_basis, as_of)
  mod_map_server("map", filtered_stratum, filtered_subs, map_tab_active, target_basis)
  mod_table_server("table", filtered_stratum)
  mod_quality_server("quality", filtered_subs)
  mod_partner_report_server("partner_report", reactive(filter_ui_state$partner$selected), target_basis)
  mod_export_server("export", filtered_subs, filtered_stratum)
  mod_enumerator_server("enumerator", filtered_subs)
  mod_integrity_server("integrity", filtered_subs)
  mod_representativeness_server("representativeness", filtered_subs, filtered_stratum)
}

shinyApp(ui, server)
