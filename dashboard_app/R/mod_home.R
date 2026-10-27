# Home: what the survey and the dashboard are, the key definitions, and the
# national picture at a glance. Always national and unfiltered.

mod_home_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Home",
    icon = icon("house"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Northern Nigeria MSNA 2026 — Data collection overview"),
        div(
          style = "font-size: 1.02em; line-height: 1.6;",
          p(
            "This dashboard tracks the daily progress of household data collection for the 2026 ",
            "Multi-Sector Needs Assessment (MSNA) in north-east, north-west and north-central Nigeria. ",
            "The real survey covers about 32,000 households across 11 states and 176 local government ",
            "areas (LGAs), collected by more than 20 partner organisations at up to 1,000 interviews a day. ",
            "At that pace, problems have to be caught within days, not after collection ends. The ",
            "dashboard compares what has been collected with the sampling frame's targets, shows where ",
            "collection is falling behind, and surfaces the data-quality issues that need follow-up while ",
            "teams are still in the field."
          ),
          div(
            class = "alert alert-warning", style = "padding: 8px 12px;",
            icon("flask"), strong(" This public version runs on synthetic data. "),
            "It follows ", strong(comma(TOTAL_PLANNED_INTERVIEWS)), " planned interviews across ",
            strong(TOTAL_COVERED_LGAS), " LGAs (IDP and non-IDP population groups): the sample drawn in ",
            tags$a(href = "https://github.com/JackPhilpott/msna-sampling", target = "_blank", "msna-sampling"),
            ", six weeks of simulated Kobo-style submissions with planted errors, and the cleaning ",
            "decisions from this repo's pipeline. Partners are Partner A to Partner T, enumerator IDs are ",
            "made up, and accessibility is a synthetic layer, so nothing here describes real conditions."
          ),
          p(strong("The dashboard serves two audiences:")),
          tags$ul(
            tags$li(
              strong("The monitoring and coordination team"), ", overseeing the whole collection: ",
              em("Progress Overview"), ", ", em("Coverage Map"), " and ", em("Progress by LGA"),
              " show where effort and support are most needed; ", em("Data Quality"), " catches issues early ",
              "(GPS points far from the household, interviews that are too short, duplicates, cluster codes ",
              "that are not in the sample, inconsistent food-security answers, enumerator patterns). Under ",
              strong("Analysis"), ", ", em("Enumerator Performance"), ", ", em("Data Integrity Checks"), " and ",
              em("Sample Representativeness"), " look deeper at who is collecting and whether the achieved ",
              "sample looks like what the design expected."
            ),
            tags$li(
              strong("Field partners"), " collecting in specific LGAs: filter to your organisation in the ",
              "sidebar, and use ", em("Partner Report"), " to see, and download, your own LGAs' progress and ",
              "focus areas. ", em("Data Export"), " downloads whatever is currently in view."
            )
          ),
          p(
            strong("Collected vs. Achieved: "),
            "wherever the dashboard reports progress against target, it draws a hard line between ",
            strong("Collected"), " (every completed interview done in the field) and ", strong("Achieved"),
            " (completed interviews matched to a sampled cluster and not deleted in cleaning: what counts ",
            "toward the sample). An interview flagged for follow-up still counts as Achieved; only a ",
            "confirmed deletion removes it. Interviews collected past a stratum's own target count in full in ",
            "Achieved, but progress percentages are ", em("credited"), " per stratum, capped at each ",
            "stratum's target, so a surplus in one place never hides a shortfall in another. Look for the ",
            icon("circle-info", class = "text-muted"), " icon next to a figure for its exact definition."
          ),
          p(
            strong("A live, evolving sampling frame: "),
            "the sampling frame, and every target the dashboard shows, reflects the current state of an ",
            "ongoing process, not a design fixed at the start. As partners report areas inaccessible ",
            "(insecurity, flooding, denied access), clusters there are dropped, and replacements are drawn ",
            "from the accessible part of the same stratum where there is one. The ", em("Revised Target"),
            " is recalculated on the population still accessible: the minimum sample for the survey's ",
            "precision (90% confidence, a 10% margin of error, allowing for clustering), plus 5% for ",
            "operations. The ", em("Original Target"), " is the fixed design figure, with its 10% buffer. ",
            "A stratum where every cluster became inaccessible is ", em("Dropped"), ": its interviews stay ",
            "visible but are left out of national and partner totals."
          ),
          p(
            strong("Administrative boundaries: "),
            "state and LGA boundaries are the official COD-AB layers (OCHA, via HDX), and LGA is always ",
            "taken from COD-AB wherever an interview's location is shown or compared. Wards in this demo ",
            "are synthetic: each LGA is split into made-up wards, since IDP camps are often named after ",
            "their ward. The real dashboard used COD-AB wards where they exist and GRID3's operational ",
            "wards elsewhere, treating COD-AB as correct wherever the two disagree near an LGA border."
          )
        )
      ),
      card(
        card_header("At a glance"),
        uiOutput(ns("glance"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Final snapshot", span(class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;", "National, unfiltered")),
        uiOutput(ns("snapshot"))
      ),
      card(
        card_header("Priorities", span(class = "text-muted", style = "font-size: 0.8em; font-weight: normal; margin-left: 8px;", "Auto-generated, national, unfiltered: a starting point, not a full picture")),
        uiOutput(ns("priorities"))
      )
    )
  )
}

mod_home_server <- function(id, target_basis) {
  moduleServer(id, function(input, output, session) {

    progress_active <- reactive(compute_progress_by_stratum(submissions_raw, target_basis()))

    output$glance <- renderUI({
      national_agg <- progress_active() %>% filter(status != "Dropped")
      total_achieved <- sum(national_agg$achieved_n)
      total_credited <- sum(national_agg$credited_achieved_n)
      total_remaining <- sum(national_agg$remaining_n)

      cb <- collected_breakdown(progress_active(), submissions_raw)
      total_confirmed_deletion <- sum(national_agg$confirmed_deletion_n)
      total_pending_deletion <- sum(national_agg$pending_deletion_n)
      total_oversampling_surplus <- sum(national_agg$oversampling_surplus_n)

      target_by_pop <- national_agg %>% group_by(pop_type) %>% summarise(target = sum(target_active, na.rm = TRUE), .groups = "drop")
      target_non_idp <- target_by_pop$target[target_by_pop$pop_type == "non_idp"]
      target_idp <- target_by_pop$target[target_by_pop$pop_type == "idp"]
      n_states <- length(unique(strata_frame$adm1_name))
      n_regions <- length(unique(strata_frame$region))

      tags$table(
        class = "table table-sm",
        tags$tr(
          tags$td("Collection window", info_icon("The planned period of data collection, from the first day of interviews to the planned end.")),
          tags$td(strong(paste(format(FIELDING_START, "%d %b"), "-", format(FIELDING_PLANNED_END, "%d %b %Y"))))
        ),
        tags$tr(
          tags$td("Original Target", info_icon("The total planned interviews set at the start of collection, unchanged since. Always the national total, regardless of sidebar filters.")),
          tags$td(strong(comma(TOTAL_PLANNED_INTERVIEWS)))
        ),
        tags$tr(
          tags$td("Revised Target", info_icon("The current minimum sample actually needed, recalculated on the population still accessible. It can rise or fall.")),
          tags$td(
            strong(comma(TOTAL_PLANNED_INTERVIEWS_CURRENT)),
            if (nzchar(target_delta_label(TOTAL_PLANNED_INTERVIEWS, TOTAL_PLANNED_INTERVIEWS_CURRENT))) {
              tagList(
                " (",
                span(
                  style = if (is_significant_target_divergence(TOTAL_PLANNED_INTERVIEWS, TOTAL_PLANNED_INTERVIEWS_CURRENT)) "color:#C1443C; font-weight:bold;" else NULL,
                  target_delta_label(TOTAL_PLANNED_INTERVIEWS, TOTAL_PLANNED_INTERVIEWS_CURRENT)
                ),
                ")"
              )
            }
          )
        ),
        tags$tr(
          tags$td("Achieved", info_icon(paste0(
            "Every completed interview that counts: excludes confirmed deletions, and includes in full any collected past a stratum's own target. ",
            "Of these, ", comma(total_credited), " count toward the target of ", comma(active_planned_interviews(target_basis())),
            " and ", comma(total_achieved - total_credited), " are extra interviews in strata already at target, which is why target minus achieved is not what is still needed: see the next row."))),
          tags$td(strong(comma(total_achieved)))
        ),
        tags$tr(
          tags$td("Credited toward target / Still needed", info_icon("Achieved capped at each stratum's own target before summing nationally, so an oversampled stratum's surplus never offsets another stratum's shortfall and this % cannot read 100% while any stratum is still short. Still needed = the sum of every stratum's own remaining gap.")),
          tags$td(strong(comma(total_credited), " (", fmt_pct(total_credited / active_planned_interviews(target_basis())), " of ", target_basis_label(target_basis()), ") / ", comma(total_remaining), " still needed"))
        ),
        tags$tr(
          tags$td("Collected", info_icon("Every completed interview, including any later deleted: total field effort, not what counts toward target.")),
          tags$td(strong(comma(cb$collected)))
        ),
        tags$tr(
          tags$td("Confirmed Deleted", info_icon(paste0(
            "Interviews removed in cleaning (too short, duplicates, cluster codes not in the sample, clusters reported inaccessible). ",
            "Within the active strata, Collected = Achieved + Confirmed Deleted + Oversampling Surplus (currently ",
            comma(total_oversampling_surplus), "). ",
            "The rest of Collected sits outside those strata: ", comma(cb$dropped_collected),
            " in strata dropped from the design and ", comma(cb$unmatched), " not matched to a sampled cluster."))),
          tags$td(strong(comma(total_confirmed_deletion)))
        ),
        tags$tr(
          tags$td("Pending Deletion", info_icon("Achieved interviews with an open follow-up flag (GPS far from the household, inconsistent answers, enumerator patterns) that could still become a deletion. Still counted in Achieved.")),
          tags$td(strong(comma(total_pending_deletion)))
        ),
        tags$tr(
          tags$td("Clusters collected past their own target", info_icon("Clusters with more achieved interviews than the cluster's own target. They count toward Achieved in full, but are worth reviewing for representativity.")),
          tags$td(strong(comma(nrow(oversampled_clusters)), " (", comma(sum(oversampled_clusters$surplus)), " surplus interviews)"))
        ),
        tags$tr(tags$td("Target by population group"), tags$td(strong("Non-IDP: ", comma(target_non_idp), " / IDP: ", comma(target_idp)))),
        tags$tr(tags$td("Covered LGAs"), tags$td(strong(TOTAL_COVERED_LGAS))),
        tags$tr(tags$td("States / regions"), tags$td(strong(n_states, " states across ", n_regions, " regions (", paste(sort(unique(strata_frame$adm1_name)), collapse = ", "), ")"))),
        tags$tr(tags$td("Field partners"), tags$td(strong(length(PARTNERS_ASSIGNED)))),
        tags$tr(
          tags$td("Partners with zero submissions", info_icon("Partners assigned an LGA who have not submitted any interviews.")),
          tags$td(strong(if (length(PARTNERS_NOT_STARTED) == 0) "None" else paste(unname(ORG_LABELS[PARTNERS_NOT_STARTED]), collapse = ", ")))
        )
      )
    })

    output$snapshot <- renderUI({
      today_n <- max(submissions_raw$submission_date, na.rm = TRUE)
      achieved_mask <- is_achieved(submissions_raw)
      yesterday_n <- sum(submissions_raw$submission_date == (today_n - 1) & achieved_mask, na.rm = TRUE)
      today_count <- sum(submissions_raw$submission_date == today_n & achieved_mask, na.rm = TRUE)

      total_achieved <- sum(progress_active()$achieved_n[progress_active()$status != "Dropped"])
      total_credited <- sum(progress_active()$credited_achieved_n[progress_active()$status != "Dropped"])
      total_collected <- sum(is_collected(submissions_raw))
      total_target <- active_planned_interviews(target_basis())

      up_all <- submissions_raw$uploaded_at
      last_upload <- if (any(!is.na(up_all))) max(up_all, na.rm = TRUE) else NA

      tags$table(
        class = "table table-sm",
        tags$tr(tags$td("Latest submission date in the data"), tags$td(strong(format(today_n, "%d %b %Y")))),
        tags$tr(tags$td("Achieved interviews that day"), tags$td(strong(comma(today_count)))),
        tags$tr(tags$td("Achieved interviews the day before"), tags$td(strong(comma(yesterday_n)))),
        tags$tr(
          tags$td("Total achieved", info_icon("Same figures as 'Achieved' and 'Credited toward target' above. The % is credited (capped per stratum), not raw.")),
          tags$td(strong(comma(total_achieved), " / ", comma(total_target), " (", fmt_pct(total_credited / total_target), " credited toward target)"))
        ),
        tags$tr(
          tags$td("Total collected", info_icon("Every completed interview, including duplicates and interviews later deleted in cleaning.")),
          tags$td(strong(comma(total_collected)))
        ),
        tags$tr(
          tags$td("Last upload in the final data"),
          tags$td(strong(if (is.na(last_upload)) "not available" else format(last_upload, "%d %b %Y %H:%M")))
        )
      )
    })

    output$priorities <- renderUI({
      worst_lgas <- progress_active() %>%
        filter(status != "Complete", status != "Dropped", target_active > 0) %>%
        arrange(pct_achieved) %>%
        head(5)

      enum_stats <- compute_enumerator_stats(submissions_raw)
      worst_enum <- enum_stats %>% filter(submissions >= 10) %>% arrange(desc(flag_rate)) %>% head(3)

      tagList(
        p(strong("LGAs furthest behind target:")),
        tags$ul(
          if (nrow(worst_lgas) == 0) tags$li("None currently behind.") else
            lapply(seq_len(nrow(worst_lgas)), function(i) {
              r <- worst_lgas[i, ]
              tags$li(paste0(r$adm2_name, ", ", r$adm1_name, " (", unname(POP_TYPE_LABELS[r$pop_type]), ") — ", fmt_pct(r$pct_achieved), " (", r$achieved_n, "/", r$target_active, ")"))
            })
        ),
        p(strong("Enumerators with the highest flag rates (10+ submissions):")),
        tags$ul(
          if (nrow(worst_enum) == 0) tags$li("Not enough data yet.") else
            lapply(seq_len(nrow(worst_enum)), function(i) {
              r <- worst_enum[i, ]
              tags$li(paste0(r$enum_id, " (", r$org_label, ") — ", fmt_pct(r$flag_rate), " flag rate across ", r$submissions, " submissions"))
            })
        )
      )
    })
  })
}
