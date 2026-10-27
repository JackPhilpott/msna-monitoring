# Coverage Map: progress by LGA or by cluster, with optional layers for
# boundaries, ward accessibility and the PSU frame.
#
# The base map draws once; each layer is added to it through leafletProxy
# when its data changes, and the heavier reference layers only after the
# user first switches them on.

mod_map_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Coverage Map",
    icon = icon("map-location-dot"),
    card(
      full_screen = TRUE,
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
          span(
            "Coverage map",
            info_icon("Colour and % reflect Achieved: completed interviews that count toward target. Coverage by LGA follows the sidebar's Target basis toggle; Coverage by cluster always uses each cluster's own target. Hover over any area for its Collected, Confirmed Deleted and Pending Deletion breakdown."),
            span(class = "text-muted", style = "font-size: 0.75em; font-weight: normal; margin-left: 10px;", AS_OF_CAPTION)
          ),
          div(
            style = "display: flex; align-items: center; flex-wrap: wrap; gap: 14px;",
            radioButtons(
              ns("map_view"), NULL,
              choices = c("Coverage by LGA" = "lga", "Coverage by cluster" = "cluster"),
              selected = "lga", inline = TRUE
            ),
            checkboxGroupInput(
              ns("status_filter"), NULL,
              choices = names(CLUSTER_STATUS_COLORS), selected = names(CLUSTER_STATUS_COLORS), inline = TRUE
            )
          )
        ),
        span(
          class = "text-muted", style = "font-size: 0.8em; font-weight: normal;",
          "Toggle state, LGA and ward boundaries, accessibility, the PSU reference layers and a satellite basemap with the layers control (top right of the map). ",
          "Grey states are outside this assessment. White areas are within it but were not surveyed (too little of the LGA accessible at design time)."
        ),
        br(),
        span(
          class = "text-muted", style = "font-size: 0.8em; font-weight: normal;",
          "Accessibility (via the layers control): green = accessible, red = reported inaccessible. ",
          strong(paste0(N_ACCESSIBILITY_PARTNERS_REPORTED, " of ", TOTAL_ACCESSIBILITY_PARTNERS, " partners")),
          " have reported on their areas; wards without a sampled cluster keep the default (accessible) until a partner reports otherwise.",
          info_icon("Based on partners' own accessibility reports for their assigned areas (synthetic in this demo).")
        )
      ),
      leafletOutput(ns("map"), height = "780px")
    )
  )
}

mod_map_server <- function(id, filtered_stratum, filtered_subs, map_tab_active = reactive(TRUE), target_basis = reactive("original")) {
  moduleServer(id, function(input, output, session) {

    filtered_lga <- reactive({
      filtered_stratum() %>%
        # dropped strata stay on the map but add nothing to the LGA's totals
        mutate(across(
          c(target_sample_current, achieved_n, collected_n, confirmed_deletion_n,
            pending_deletion_n, oversampling_surplus_n, credited_achieved_n, remaining_n),
          ~ ifelse(status == "Dropped", 0, .)
        )) %>%
        mutate(target_active = if (identical(target_basis(), "revised")) target_sample_current else target_sample) %>%
        group_by(region, adm1_pcode, adm1_name, adm2_pcode, adm2_name) %>%
        summarise(
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
          n_strata = n(),
          n_dropped_strata = sum(status == "Dropped"),
          .groups = "drop"
        ) %>%
        mutate(
          pct_achieved = ifelse(target_active > 0, credited_achieved_n / target_active, NA_real_),
          all_dropped = n_strata > 0 & n_dropped_strata == n_strata
        )
    })

    scope_admin2_sf <- reactive({
      lga <- filtered_lga() %>%
        select(adm2_pcode, target_sample, target_sample_current, target_active, achieved_n, credited_achieved_n, remaining_n,
               collected_n, confirmed_deletion_n, pending_deletion_n, oversampling_surplus_n, pct_achieved, all_dropped,
               n_dropped_strata)
      admin2_sf %>% inner_join(lga, by = "adm2_pcode")
    })

    scope_state_pcodes <- reactive(unique(filtered_stratum()$adm1_pcode))
    scope_lga_pcodes <- reactive(unique(filtered_stratum()$adm2_pcode))

    remaining_by_pop_type <- reactive({
      filtered_stratum() %>%
        filter(status != "Dropped") %>%
        group_by(adm2_pcode, pop_type) %>%
        summarise(remaining = sum(remaining_n, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = pop_type, values_from = remaining, names_prefix = "remaining_", values_fill = 0) %>%
        {
          if (!"remaining_non_idp" %in% names(.)) mutate(., remaining_non_idp = 0) else .
        } %>%
        {
          if (!"remaining_idp" %in% names(.)) mutate(., remaining_idp = 0) else .
        }
    })

    lga_map_data <- reactive({
      scope_admin2_sf() %>%
        mutate(
          pct_achieved = coalesce(pct_achieved, 0),
          fill_color = ifelse(all_dropped, "#9AA3AF", pct_color(pct_achieved)),
          label_pct = fmt_pct(pct_achieved),
          status = case_when(
            all_dropped ~ "Inaccessible",
            target_active <= 0 | remaining_n <= 0 ~ "Complete",
            achieved_n > 0 ~ "In progress",
            TRUE ~ "Not started"
          ),
          partner_coverage = vapply(adm2_pcode, partner_coverage_label, character(1)),
          target_delta = target_delta_label(target_sample, target_sample_current),
          target_diverges = is_significant_target_divergence(target_sample, target_sample_current)
        ) %>%
        left_join(accessibility_lga_summary, by = "adm2_pcode") %>%
        left_join(remaining_by_pop_type(), by = "adm2_pcode") %>%
        filter(status %in% input$status_filter)
    })

    accessibility_map_data <- reactive({
      s <- filtered_stratum()
      pop_labels <- unname(POP_TYPE_LABELS[unique(s$pop_type)])
      accessibility_sf %>%
        filter(adm2_pcode %in% unique(s$adm2_pcode), pop_type %in% pop_labels) %>%
        # one shape per ward: inaccessible for any group in scope wins
        arrange(adm2_pcode, wardname, desc(accessible_status == "Inaccessible")) %>%
        distinct(adm2_pcode, wardname, .keep_all = TRUE)
    })

    psu_hex_in_scope <- reactive({
      s <- filtered_stratum()
      psu_hexagons_sf %>% filter(adm2_pcode %in% unique(s$adm2_pcode), pop_type %in% unique(s$pop_type))
    })

    psu_sites_in_scope <- reactive({
      s <- filtered_stratum()
      psu_sites_sf %>% filter(adm2_pcode %in% unique(s$adm2_pcode), pop_type %in% unique(s$pop_type))
    })

    cluster_achieved <- reactive({
      compute_cluster_progress(filtered_subs())
    })

    cluster_status <- function(psu_sf) {
      psu_sf %>%
        left_join(cluster_achieved(), by = "cluster_id") %>%
        mutate(
          achieved_n = coalesce(achieved_n, 0L),
          collected_n = coalesce(collected_n, 0L),
          confirmed_deletion_n = coalesce(confirmed_deletion_n, 0L),
          pending_deletion_n = coalesce(pending_deletion_n, 0L),
          target_households = as.numeric(target_households),
          is_inaccessible = !is.na(accessible_status) & accessible_status == "Inaccessible",
          status = case_when(
            achieved_n >= target_households ~ "Complete",
            achieved_n > 0 ~ "In progress",
            is_inaccessible ~ "Inaccessible",
            TRUE ~ "Not started"
          ),
          fill_color = unname(CLUSTER_STATUS_COLORS[status]),
          oversampled = target_households > 0 & achieved_n > target_households,
          stranded_inaccessible = achieved_n > 0 & is_inaccessible,
          label_pct = fmt_pct(ifelse(target_households > 0, achieved_n / target_households, NA_real_)),
          partner_coverage = vapply(adm2_pcode, partner_coverage_label, character(1)),
          tip_name = ifelse(is.na(site_name), cluster_id, paste0(site_name, " (", cluster_id, ")")),
          draw_note = ifelse(is.na(replaces), "",
                             paste0("Replacement for ", replaces, " (", sub("replacement_", "", draw), ")<br>"))
        ) %>%
        filter(status %in% input$status_filter)
    }

    cluster_hex_data <- reactive(cluster_status(psu_hex_in_scope()))
    cluster_site_data <- reactive(cluster_status(psu_sites_in_scope()))

    scope_bbox <- reactive({
      sf_data <- scope_admin2_sf()
      if (nrow(sf_data) == 0) return(NULL)
      bb <- sf::st_bbox(sf_data)
      if (any(!is.finite(bb))) return(NULL)
      bb
    })

    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 5)) %>%
        addProviderTiles("Esri.WorldGrayCanvas", group = "Light (default)") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        setView(lng = 8.0, lat = 10.5, zoom = 6) %>%
        # panes fix the stacking order whatever order layers are added in
        addMapPane("lgaProgressPane", zIndex = 401) %>%
        addMapPane("accessibilityPane", zIndex = 402) %>%
        addMapPane("clusterStatusPane", zIndex = 403) %>%
        addMapPane("psuReferencePane", zIndex = 410) %>%
        addMapPane("wardBoundariesPane", zIndex = 420) %>%
        addMapPane("lgaBoundariesPane", zIndex = 421) %>%
        addMapPane("stateBoundariesPane", zIndex = 422) %>%
        addMapPane("idpSitesPane", zIndex = 450) %>%
        addPolygons(
          data = admin1_sf %>% filter(!in_assessment),
          fillColor = "#9AA3AF", fillOpacity = 0.35, color = "#9AA3AF", weight = 0.5,
          label = "Not part of this assessment"
        ) %>%
        addPolygons(
          data = admin0_sf, fill = FALSE, color = "#0D1526", weight = 2, opacity = 0.8,
          options = pathOptions(interactive = FALSE)
        ) %>%
        addLayersControl(
          baseGroups = c("Light (default)", "Satellite"),
          overlayGroups = c("State boundaries", "LGA boundaries", "Ward boundaries", "Accessibility", "PSU hexagons (reference)", "PSU sites (reference)"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("LGA boundaries", "Ward boundaries", "Accessibility", "PSU hexagons (reference)", "PSU sites (reference)", "Cluster status", "Cluster status sites")) %>%
        htmlwidgets::onRender(sprintf(
          "function(el, x) {
            this.on('overlayadd', function(e) {
              Shiny.setInputValue('%s', e.name, {priority: 'event'});
            });
          }",
          session$ns("layer_toggled")
        ))
    })

    outputOptions(output, "map", suspendWhenHidden = FALSE)

    layer_shown <- reactiveValues(ward = FALSE, lga_boundaries = FALSE, accessibility = FALSE, psu_hex = FALSE, psu_sites = FALSE)
    observeEvent(input$layer_toggled, {
      nm <- input$layer_toggled
      if (identical(nm, "Ward boundaries")) layer_shown$ward <- TRUE
      if (identical(nm, "LGA boundaries")) layer_shown$lga_boundaries <- TRUE
      if (identical(nm, "Accessibility")) layer_shown$accessibility <- TRUE
      if (identical(nm, "PSU hexagons (reference)")) layer_shown$psu_hex <- TRUE
      if (identical(nm, "PSU sites (reference)")) layer_shown$psu_sites <- TRUE
    })

    observe({
      req(map_tab_active())
      req(input$map_view == "lga")
      md <- lga_map_data()

      active_style <- paste0("color:", TARGET_BASIS_ACTIVE_COLOR, ";font-weight:bold;")
      inactive_style <- paste0("color:", TARGET_BASIS_INACTIVE_COLOR, ";")
      original_style <- if (identical(target_basis(), "revised")) inactive_style else active_style
      revised_style <- if (identical(target_basis(), "revised")) active_style else inactive_style

      leafletProxy("map", data = md) %>%
        clearGroup("LGA progress") %>%
        addPolygons(
          fillColor = ~fill_color,
          fillOpacity = 0.75,
          color = "#FFFFFF",
          weight = 0.5,
          group = "LGA progress",
          options = pathOptions(pane = "lgaProgressPane"),
          label = ~lapply(
            paste0(
              "<b>", adm2_name, "</b>, ", adm1_name, "<br>",
              "Partner(s): ", partner_coverage, "<br>",
              "Credited toward target: <span style='color:", fill_color, ";font-weight:bold;'>", coalesce(credited_achieved_n, 0), " (", label_pct, " of ", target_basis_label(target_basis()), ")</span>",
              " | Still needed: ", coalesce(remaining_n, 0), "<br>",
              "Achieved (all interviews incl. surplus): ", coalesce(achieved_n, 0), "<br>",
              ifelse(all_dropped, "<b style='color:#9AA3AF;'>&#9888; Every stratum here is inaccessible (dropped from the sampling frame)</b><br>",
                     ifelse(n_dropped_strata > 0, "<b style='color:#9AA3AF;'>&#9888; One population group here was dropped (every cluster inaccessible)</b><br>", "")),
              "<span style='", original_style, "'>Original Target: ", coalesce(target_sample, 0), "</span><br>",
              ifelse(target_diverges, "<b style='color:#C1443C;'>", paste0("<span style='", revised_style, "'>")),
              "Revised Target: ", coalesce(target_sample_current, 0), " (", target_delta, ")",
              ifelse(target_diverges, "</b>", "</span>"), "<br>",
              "Collected: ", coalesce(collected_n, 0), "<br>",
              "Confirmed Deleted: ", coalesce(confirmed_deletion_n, 0),
              " | Oversampling Surplus: ", coalesce(oversampling_surplus_n, 0), "<br>",
              "Pending Deletion (still counted in Achieved): ", coalesce(pending_deletion_n, 0), "<br>",
              "Still needed by group: ", coalesce(remaining_idp, 0), " IDP | ", coalesce(remaining_non_idp, 0), " Non-IDP<br>",
              "Inaccessible wards: ", coalesce(n_ward_portions_inaccessible, 0), " of ", coalesce(n_ward_portions, 0), "<br>",
              "Population remaining accessible:<br>", gsub(" | ", "<br>", coalesce(pop_remaining_label, "n/a"), fixed = TRUE)
            ),
            htmltools::HTML
          ),
          highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
        )
    })

    cluster_label <- ~lapply(
      paste0(
        "<b>", tip_name, "</b><br>", adm3_name, ", ", adm2_name, ", ", adm1_name, "<br>",
        "Achieved: ", achieved_n, " / ", target_households, " (", label_pct, ")<br>",
        "Collected: ", collected_n, "<br>",
        "Confirmed Deleted: ", confirmed_deletion_n, " | Pending Deletion: ", pending_deletion_n, "<br>",
        "Status: ", status, "<br>",
        draw_note,
        ifelse(oversampled, paste0("<b style='color:", OVERSAMPLED_BORDER, ";'>&#9888; Oversampled by ", achieved_n - target_households, "</b><br>"), ""),
        ifelse(stranded_inaccessible, paste0("<b style='color:", INACCESSIBLE_BORDER, ";'>&#9888; Achieved, but now reported inaccessible</b><br>"), ""),
        "Partner(s): ", partner_coverage
      ),
      htmltools::HTML
    )

    observe({
      req(map_tab_active())
      req(input$map_view == "cluster")
      hexes <- cluster_hex_data()
      leafletProxy("map", data = hexes) %>%
        clearGroup("Cluster status") %>%
        addPolygons(
          fillColor = ~fill_color,
          fillOpacity = 0.85,
          color = ~ifelse(oversampled, OVERSAMPLED_BORDER, ifelse(stranded_inaccessible, INACCESSIBLE_BORDER, "#FFFFFF")),
          weight = ~ifelse(oversampled | stranded_inaccessible, 4, 0.5),
          group = "Cluster status",
          options = pathOptions(pane = "clusterStatusPane"),
          label = cluster_label,
          highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
        )
    })

    observe({
      req(map_tab_active())
      req(input$map_view == "cluster")
      sites <- cluster_site_data()
      leafletProxy("map", data = sites) %>%
        clearGroup("Cluster status sites") %>%
        addCircleMarkers(
          radius = 6,
          fillColor = ~fill_color,
          fillOpacity = 0.9,
          color = ~ifelse(oversampled, OVERSAMPLED_BORDER, ifelse(stranded_inaccessible, INACCESSIBLE_BORDER, "#333333")),
          weight = ~ifelse(oversampled | stranded_inaccessible, 4, 1),
          stroke = TRUE,
          group = "Cluster status sites",
          options = pathOptions(pane = "idpSitesPane"),
          label = cluster_label
        )
    })

    observe({
      req(map_tab_active())
      req(layer_shown$accessibility)
      leafletProxy("map", data = accessibility_map_data()) %>%
        clearGroup("Accessibility") %>%
        addPolygons(
          group = "Accessibility",
          fillColor = ~ifelse(accessible_status == "Inaccessible", "#C1443C", "#4C9A6A"),
          fillOpacity = 1,
          color = "#FFFFFF",
          weight = 0.5,
          options = pathOptions(pane = "accessibilityPane"),
          label = ~lapply(
            paste0(
              "<b>", wardname, "</b>, ", adm2_name, ", ", adm1_name, "<br>",
              "Status: ", accessible_status,
              ifelse(status_source == "confirmed_by_partner_report", "", " (default: not reported)"), "<br>",
              ifelse(nzchar(coalesce(reporting_partners, "")), paste0("Reported by: ", reporting_partners, "<br>"),
                     paste0("Covered by: ", covering_partners, "<br>")),
              ifelse(reason_category != "N/A - fully accessible", paste0("Reason: ", reason_category, "<br>"), "")
            ),
            htmltools::HTML
          ),
          highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
        )
    })

    observe({
      req(map_tab_active())
      req(layer_shown$psu_hex)
      leafletProxy("map", data = psu_hex_in_scope()) %>%
        clearGroup("PSU hexagons (reference)") %>%
        addPolygons(
          group = "PSU hexagons (reference)", fill = FALSE, color = "#2A4D8F", weight = 1, opacity = 0.8,
          options = pathOptions(pane = "psuReferencePane"),
          label = ~lapply(paste0("<b>", cluster_id, "</b><br>", adm2_name, ", ", adm1_name, "<br>Target HH: ", target_households), htmltools::HTML)
        )
    })

    observe({
      req(map_tab_active())
      req(layer_shown$psu_sites)
      leafletProxy("map", data = psu_sites_in_scope()) %>%
        clearGroup("PSU sites (reference)") %>%
        addCircleMarkers(
          group = "PSU sites (reference)", radius = 5, color = "#8B4A4A", fillOpacity = 0.8, stroke = TRUE, weight = 1,
          options = pathOptions(pane = "psuReferencePane"),
          label = ~lapply(paste0("<b>", site_name, "</b> (", cluster_id, ")<br>", adm2_name, ", ", adm1_name, "<br>Target HH: ", target_households), htmltools::HTML)
        )
    })

    observe({
      req(map_tab_active())
      in_scope <- scope_state_pcodes()
      assessment_states <- admin1_sf %>%
        filter(in_assessment) %>%
        mutate(is_selected = adm1_pcode %in% in_scope)

      leafletProxy("map", data = assessment_states) %>%
        clearGroup("State boundaries") %>%
        addPolygons(
          fill = FALSE,
          color = ~ifelse(is_selected, "#1B2A4A", "#8FA0BF"),
          weight = ~ifelse(is_selected, 2.2, 1),
          opacity = ~ifelse(is_selected, 0.9, 0.5),
          group = "State boundaries",
          options = pathOptions(interactive = FALSE, pane = "stateBoundariesPane")
        )
    })

    observe({
      req(map_tab_active())
      req(layer_shown$lga_boundaries)
      leafletProxy("map", data = scope_admin2_sf()) %>%
        clearGroup("LGA boundaries") %>%
        addPolygons(
          fill = FALSE, color = "#3E5A8C", weight = 1, opacity = 0.8, group = "LGA boundaries",
          options = pathOptions(pane = "lgaBoundariesPane"),
          label = ~lapply(paste0("<b>", adm2_name, "</b>, ", adm1_name), htmltools::HTML)
        )
    })

    observe({
      req(map_tab_active())
      req(layer_shown$ward)
      w <- wards_sf %>% filter(adm2_pcode %in% scope_lga_pcodes())
      leafletProxy("map", data = w) %>%
        clearGroup("Ward boundaries") %>%
        addPolygons(
          fill = FALSE, color = "#6B5B95", weight = 0.7, opacity = 0.7, group = "Ward boundaries",
          options = pathOptions(pane = "wardBoundariesPane"),
          label = ~lapply(paste0("<b>", wardname, "</b> (synthetic)<br>", lganame, ", ", statename), htmltools::HTML)
        )
    })

    observe({
      req(map_tab_active())
      bb <- scope_bbox()
      if (is.null(bb)) return()
      leafletProxy("map") %>% fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
    })

    observe({
      req(input$map_view)
      proxy <- leafletProxy("map") %>% removeControl("map_legend")
      if (input$map_view == "lga") {
        proxy %>%
          hideGroup(c("Cluster status", "Cluster status sites")) %>%
          showGroup("LGA progress") %>%
          addLegend(
            layerId = "map_legend", position = "bottomright",
            colors = c("#C1443C", "#D99A2B", "#8FC79A", "#1E7B4D", "#9AA3AF"),
            labels = c("<35%", "35-75%", "75-100%", "100%+", "Inaccessible"),
            title = paste0("% of ", target_basis_label(target_basis()), " achieved (LGA)"), opacity = 0.9
          )
      } else {
        proxy %>%
          hideGroup("LGA progress") %>%
          showGroup(c("Cluster status", "Cluster status sites")) %>%
          addLegend(
            layerId = "map_legend", position = "bottomright",
            colors = c(unname(CLUSTER_STATUS_COLORS), OVERSAMPLED_BORDER, INACCESSIBLE_BORDER),
            labels = c(names(CLUSTER_STATUS_COLORS), "Oversampled (purple outline)", "Achieved, now inaccessible (grey outline)"),
            title = "Cluster status", opacity = 0.9
          )
      }
    })
  })
}
