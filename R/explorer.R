#' Launch the herminR interactive explorer
#'
#' A guided Shiny application that takes a scientist from data or
#' coefficients to MPI, uncertainty, robustness and follow-up analysis, using
#' the package's own functions throughout (it performs no calculation of its
#' own). It opens on a *Start here* page; a progress bar and a guide box on
#' each stage explain the question being answered and summarise what was
#' found in plain language. Stages the current data cannot support are greyed
#' out with an explanation.
#'
#' 1. **Data & model** - built-in studies, an uploaded data set (responses,
#'    factors, optional block, model order, coding to \eqn{[-1, 1]}) or an
#'    uploaded coefficient table; fitted coefficients are shown.
#' 2. **MPI explorer** - factor x response heatmaps with norm, signed or
#'    absolute values, standardisation and layout; table and downloads;
#'    optional advanced settings. Hovering a cell shows its numbers; clicking
#'    it highlights the factor on every stage. A snapshot of the results can
#'    be kept and compared with other settings (rank changes are marked).
#' 3. **Uncertainty** - SE, delta-method or ellipsoid intervals
#'    ([mpi_interval_plot()]) next to bootstrap rank stability
#'    ([bootstrap_ranks()], run on request).
#' 4. **Robustness** - model-selection paths ([mpi_selection_paths()], run on
#'    request) and norm comparison ([mpi_compare_norms()]).
#' 5. **Follow-up** - range adjustment and factor subsets
#'    ([mpi_adjust_range()], [mpi_subset()]) and feasible-region MPI
#'    ([mpi_feasible()]): exact for boxes, sampled on request for constraints.
#' 6. **Reproduce** - the R script that recreates the results on screen, a
#'    self-contained HTML report, and a link that restores every setting
#'    (URL bookmarking; uploaded files must be uploaded again).
#'
#' Requires the \pkg{shiny} package.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return Called for its side effect (runs the app).
#' @examples
#' if (interactive()) run_explorer()
#' @export
run_explorer <- function(...) {
  .need_shiny()
  shiny::runApp(explorer_app(), ...)
}

#' @rdname run_explorer
#' @return `explorer_app()` returns the Shiny app object (with URL
#'   bookmarking enabled).
#' @export
explorer_app <- function() {
  .need_shiny()
  shiny::shinyApp(function(request) explorer_ui(), explorer_server, enableBookmarking = "url")
}

.need_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("The explorer needs the {.pkg shiny} package: {.code install.packages(\"shiny\")}.")
  }
}

#' @rdname run_explorer
#' @return `explorer_ui()` returns the UI definition.
#' @export
explorer_ui <- function() {
  .need_shiny()
  shiny::navbarPage(
    title = "herminR explorer",
    collapsible = TRUE,   # a menu button instead of a column of tabs on phones
    id = "tabs",
    selected = "start",
    header = shiny::tagList(explorer_head(), shiny::uiOutput("guide_stepper"), shiny::uiOutput("focus_bar")),
    guide_start_ui(),
    stage_data_ui(),
    stage_mpi_ui(),
    stage_uncertainty_ui(),
    stage_robustness_ui(),
    stage_followup_ui(),
    stage_reproduce_ui(),
    # secondary: tools for specialists, kept out of the six-stage path
    shiny::navbarMenu("More", adv_view_ui(), menuName = "more")
  )
}

#' @rdname run_explorer
#' @param input,output,session Shiny server arguments.
#' @export
explorer_server <- function(input, output, session) {
  # Each stage lives in its own file (R/explorer_stage_*.R) and returns its
  # reactives; they are gathered here so later stages, the guide, the report
  # and the tests can use them.
  st <- stage_data_server(input, output, session)
  # shared across stages: the highlighted factor and the comparison snapshot
  st$focus <- shiny::reactiveVal(NULL)
  st$geom <- new.env(parent = emptyenv())   # panel positions of the interactive plots
  st$snapshot <- shiny::reactiveVal(NULL)
  # optional views the user has opened; the script, report and guide mention
  # them only then, so the default outputs stay as in 0.3.0
  st$used <- shiny::reactiveVal(character())
  shiny::observe({
    u <- c(if (isTRUE(input$mpi_view %in% c("terms", "network", "responses"))) "flows",
           if (identical(input$mpi_view, "contour")) "contour",
           if (identical(input$rank_view, "flow")) "rank_flow")
    old <- shiny::isolate(st$used())
    if (length(setdiff(u, old))) st$used(union(old, u))
  })
  analysis <- st$analysis
  responses <- st$responses
  mpi_input <- st$mpi_input
  term_maps <- st$term_maps

  st <- c(st, stage_mpi_server(input, output, session, st))
  base_mpi <- st$base_mpi
  shown_mpi <- st$shown_mpi
  heatmap_plot <- st$heatmap_plot

  st <- c(st, stage_uncertainty_server(input, output, session, st))
  unc_mpi <- st$unc_mpi
  boot_res <- st$boot_res

  # comparison with a snapshot (needs the MPI and uncertainty stages)
  st <- c(st, explorer_compare_server(input, output, session, st))
  compare <- st$compare
  compare_text <- st$compare_text
  focus <- st$focus
  snapshot <- st$snapshot

  st <- c(st, stage_robustness_server(input, output, session, st))
  sel_res <- st$sel_res

  st <- c(st, stage_followup_server(input, output, session, st))
  followup_mpi <- st$followup_mpi
  feas_res <- st$feas_res

  # Advanced settings & Advanced View (R/advanced_settings.R); the standard
  # outputs never read the advanced inputs.
  adv <- adv_server(input, output, session, analysis = analysis, inputs_of = mpi_input,
                    responses = responses)
  st$adv <- adv

  guide <- guide_server(input, output, session, st)
  st$guide <- guide

  explorer_focus_server(input, output, session, st)

  rp <- stage_reproduce_server(input, output, session, st)
  code_text <- rp$code_text
  report_html <- rp$report_html

  # Bookmarking: navigation buttons and the file itself are not part of the link
  shiny::setBookmarkExclude(c("file", "guide_goto", "guide_example", "guide_begin", "constraint_example",
                              "goto_data_3", "goto_data_4", "bookmark", "snap_take", "snap_clear", "focus_clear",
                              "heatmap_hover", "heatmap_click", "interval_plot_hover", "interval_plot_click",
                              "rank_plot_hover", "rank_plot_click", "network_click", "snap_take", "snap_clear",
                              paste0("guide_next_", 1:6), paste0("guide_back_", 1:6)))
  # a bookmark button with a custom id must trigger bookmarking itself
  shiny::observeEvent(input$bookmark, session$doBookmark())
  shiny::onRestored(function(state) {
    if (isTRUE(state$input$source %in% c("upload_data", "upload_coef"))) {
      shiny::showNotification("This link used an uploaded file, which links cannot store: please upload it again in step 1.",
                              type = "warning", duration = NULL)
    }
  })
  invisible(NULL)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
