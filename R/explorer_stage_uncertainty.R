# Stage 3: uncertainty (intervals and bootstrap rank stability) ----------------------

#' UI of the uncertainty stage
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
stage_uncertainty_ui <- function() {
  shiny::tabPanel(
    "3 Uncertainty",
    guide_box_ui(3),
    shiny::conditionalPanel("!output.cap_intervals", explorer_unavailable_ui(
      paste("Intervals need fitted models (raw data) or a coefficient table that includes a column 'se'",
            "with standard errors. The paper's Table 1 and simulation only provide coefficients."),
      "goto_data_3")),
    shiny::conditionalPanel("output.cap_intervals",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("unc_response", "Response", NULL),
          shiny::radioButtons("bounds", "Interval", c("Ellipsoid (simultaneous within factor)" = "ellipsoid",
                                                      "Delta method (t)" = "delta",
                                                      "SE soft-threshold" = "se")),
          shiny::sliderInput("level", "Level", min = 0.8, max = 0.99, value = 0.95, step = 0.01, ticks = FALSE),
          shiny::hr(),
          shiny::h5("Bootstrap rank stability"),
          shiny::conditionalPanel("output.cap_bootstrap",
            shiny::numericInput("boot_n", "Replicates", 200, min = 20, max = 5000, step = 50),
            shiny::radioButtons("boot_type", "Resampling", c("Residual (fixed design)" = "residual",
                                                             "Case" = "case"), inline = TRUE),
            shiny::numericInput("boot_seed", "Seed", 1),
            shiny::actionButton("run_boot", "Run bootstrap", class = "herminR-run"),
            shiny::uiOutput("boot_last")),
          shiny::conditionalPanel("!output.cap_bootstrap",
            shiny::p(class = "note", "The bootstrap re-fits the model to replayed data, so it needs raw data (step 1)."))
        ),
        shiny::mainPanel(
          shiny::uiOutput("unc_info"),
          shiny::h4("How large, and how precisely estimated?"),
          shiny::uiOutput("bounds_note"),
          shiny::p(class = "plot-hint", "Hover a factor for its numbers; click to highlight it on every stage."),
          explorer_interactive_plot("interval_plot", vh = 48, min = 300, max = 520),
          explorer_details("Interval table", shiny::tableOutput("interval_table")),
          shiny::conditionalPanel("output.cap_bootstrap",
            shiny::h4("How stable is the ranking?"),
            shiny::radioButtons("rank_view", NULL, inline = TRUE,
                                c("Rank intervals" = "interval", "Rank flow (Sankey)" = "flow")),
            shiny::uiOutput("rank_note"),
            explorer_interactive_plot("rank_plot", vh = 45, min = 280, max = 480),
            explorer_details("Rank table", shiny::tableOutput("rank_table")))
        )
      )
    )
  )
}

#' Server of the uncertainty stage
#' @param input,output,session Shiny objects.
#' @param st Shared state.
#' @return A list: `unc_mpi`, `boot_state`, `boot_res`, `boot_params`.
#' @keywords internal
#' @noRd
stage_uncertainty_server <- function(input, output, session, st) {
  unc_mpi <- shiny::reactive({
    shiny::validate(shiny::need(st$capabilities()$intervals,
                                "Intervals need fitted models or a coefficient table with standard errors."))
    a <- st$analysis()
    sel <- if (identical(input$unc_response, ".all")) NULL else shiny::req(input$unc_response)
    suppressWarnings(mpi_multi(st$mpi_input(a), norm = input$norm, bounds = input$bounds,
                               level = input$level, abs = TRUE, factors = a$factors))
  })
  output$unc_info <- shiny::renderUI(shiny::p(class = "note",
    "Intervals describe the precision of each factor's influence (per factor, not simultaneous across factors; ",
    "'lower bound > 0' is not a significance test). The bootstrap describes how stable the ranking is. ",
    "They answer different questions: overlapping intervals can coexist with a stable ranking."))
  output$interval_plot <- shiny::renderPlot(
    explorer_draw_interactive(explorer_fit_width(explorer_flag_intervals(
                                explorer_highlight_points(mpi_interval_plot(unc_mpi()), st$focus(), "value"), int_flags()) +
                                                   explorer_theme(), explorer_width(session, "interval_plot")),
                              st$geom, "interval_plot"), res = 96,
    alt = "MPI of each factor with its uncertainty interval; the interval table gives the same numbers")
  int_flags <- shiny::reactive(explorer_interval_flags(unc_mpi(), input$bounds, input$level))
  output$bounds_note <- shiny::renderUI({
    fl <- tryCatch(int_flags(), error = function(e) NULL)
    bad <- if (is.null(fl)) fl else fl[!fl$reliable, , drop = FALSE]
    shiny::tagList(
      if (!is.null(bad) && nrow(bad)) shiny::div(class = "unreliable", role = "alert",
        shiny::strong(if (identical(input$bounds, "se")) "These intervals are not confidence intervals. " else
          sprintf("%d of %d intervals may be too narrow. ", nrow(bad), nrow(fl))),
        if (identical(input$bounds, "delta")) shiny::tagList(
          shiny::tags$ul(lapply(utils::head(sprintf("%s (%s): %s", bad$factor, bad$response, bad$note), 8), shiny::tags$li)),
          "Switch to 'Ellipsoid' for intervals that keep their coverage.") else
          "Use 'Ellipsoid' or 'Delta method' to report precision."),
      shiny::div(class = "note", explorer_bounds_note(input$bounds, input$level)))
  })
  output$rank_note <- shiny::renderUI(shiny::p(class = "note",
    "This is not a confidence interval: each replay re-fits the model to data simulated from the fitted one, and the plot shows ",
    "which rank each factor gets. A factor ranked first in the original fit but not in most replays should not be presented as the clear leader."))
  output$interval_table <- shiny::renderTable({
    m <- tibble::as_tibble(unc_mpi())
    out <- as.data.frame(m[, c("response", "factor", "estimate", "lower", "upper")])
    fl <- int_flags()
    if (any(!fl$reliable)) out$reliability <- ifelse(fl$reliable, "ok", fl$note)
    out
  }, digits = 4)

  boot_params <- shiny::reactive({
    r <- if (identical(input$unc_response, ".all") || is.null(input$unc_response)) st$responses()[1] else input$unc_response
    list(response = r, n = input$boot_n, type = input$boot_type, seed = input$boot_seed,
         norm = input$norm, level = input$level)
  })
  boot_state <- explorer_run_on_click(input, session, "run_boot", boot_params, function(p) {
    a <- st$analysis()
    if (!identical(a$kind, "model")) stop("The bootstrap needs fitted models (choose raw data in step 1).")
    if (!is.numeric(p$n) || is.na(p$n) || p$n < 20 || p$n > 5000) stop("Replicates must be between 20 and 5000.")
    shiny::withProgress(message = sprintf("Bootstrap (%s, %d replays)", p$response, as.integer(p$n)), value = 0.3, {
      bootstrap_ranks(a$models[[p$response]], n = as.integer(p$n), type = p$type,
                      norm = p$norm, seed = p$seed, level = p$level)
    })
  })
  boot_res <- shiny::reactive(explorer_run_value(boot_state(), "Press 'Run bootstrap'."))
  output$boot_last <- shiny::renderUI(explorer_last_run_ui(
    boot_state(), tryCatch(boot_params(), error = function(e) NULL),
    function(p) sprintf("response %s, %s replicates, %s resampling, seed %s", p$response, p$n, p$type, p$seed),
    "Run bootstrap"))
  output$rank_plot <- shiny::renderPlot(
    explorer_draw_interactive(explorer_fit_width(
      if (identical(input$rank_view, "flow")) {
        plot(boot_res(), type = "flow", highlight = st$focus()$factor,
             compact = isTRUE(explorer_width(session, "rank_plot") < 600)) + explorer_theme_diagram()
      } else {
        explorer_highlight_points(plot(boot_res()), st$focus(), "rank_mean") + explorer_theme()
      }, explorer_width(session, "rank_plot")), st$geom, "rank_plot"), res = 96,
    alt = "Bootstrap rank of each factor; the rank table gives the same numbers")
  output$rank_table <- shiny::renderTable(tibble::as_tibble(boot_res()), digits = 3)

  list(unc_mpi = unc_mpi, int_flags = int_flags, boot_state = boot_state, boot_res = boot_res, boot_params = boot_params)
}

#' What an interval method's coverage means, in plain words
#' @param bounds `"ellipsoid"`, `"delta"` or `"se"`.
#' @param level Confidence level.
#' @return A Shiny tag list.
#' @keywords internal
#' @noRd
explorer_bounds_note <- function(bounds, level) {
  lv <- paste0(round(100 * as.numeric(level %||% 0.95)), "%")
  txt <- switch(bounds %||% "ellipsoid",
    ellipsoid = c(
      paste0("Ellipsoid: the range of the factor's MPI over the ", lv, " joint confidence ellipsoid of all the coefficients it uses, with their covariance. ",
             "It covers the true MPI at least ", lv, " of the time if the model is right and the errors are roughly normal (conservative: usually more)."),
      "It is per factor: the chance that every factor's interval is right at once is lower."),
    delta = c(
      paste0("Delta method: estimate \u00b1 t \u00d7 SE, with the SE from the coefficients' covariance. Approximately ", lv,
             " coverage when the estimate is clearly away from zero and the coefficients' signs are stable."),
      "Close to zero, or when a coefficient may change sign, the approximation is poor and coverage can fall below the nominal level."),
    se = c(
      "SE soft-threshold: each coefficient is shrunk towards zero by its own SE before adding up. A quick screening aid.",
      "It has no exact joint coverage: do not read it as a confidence interval."))
  shiny::tagList(shiny::strong("What this interval means. "), txt[1], " ", txt[2],
                 " The intervals describe precision of the estimate; how stable the ranking is comes from the bootstrap below.")
}

#' Which intervals may not deliver their nominal coverage
#'
#' The delta-method interval is a linear approximation; when one of the terms
#' that make up a factor's MPI is not clearly different from zero (|t| below
#' the critical value), its sign is uncertain and the interval can be too
#' narrow (about 86% instead of 95% in the package's validation simulation).
#' The SE soft-threshold interval has no coverage guarantee at all. The
#' ellipsoid interval is conservative and is never flagged.
#'
#' @param u MPI object with intervals (`mpi_multi(..., bounds = )`).
#' @param bounds Interval method.
#' @param level Confidence level.
#' @return A data frame: `response`, `factor`, `reliable`, `note`.
#' @keywords internal
#' @noRd
explorer_interval_flags <- function(u, bounds, level = 0.95) {
  d <- tibble::as_tibble(u)
  tms <- attr(u, "term_maps")
  out <- data.frame(response = d$response, factor = d$factor, reliable = TRUE, note = "", stringsAsFactors = FALSE)
  if (identical(bounds, "se")) {
    out$reliable <- FALSE
    out$note <- "SE soft-threshold: no coverage guarantee (screening only)"
    return(out)
  }
  if (!identical(bounds, "delta") || is.null(tms)) return(out)
  for (i in seq_len(nrow(out))) {
    tm <- tms[[out$response[i]]]
    if (is.null(tm) || is.null(tm$vcov)) next
    f <- out$factor[i]
    terms <- names(tm$powers)[vapply(tm$powers, function(p) f %in% names(p), logical(1))]
    terms <- intersect(terms, rownames(tm$vcov))
    if (!length(terms)) next
    se <- sqrt(diag(tm$vcov)[terms])
    tval <- abs(tm$coef[terms]) / se
    crit <- stats::qt(1 - (1 - level) / 2, df = if (is.null(tm$df) || !is.finite(tm$df)) Inf else tm$df)
    weak <- terms[is.finite(tval) & tval < crit]
    if (length(weak)) {
      out$reliable[i] <- FALSE
      out$note[i] <- paste0("may be too narrow: ", paste(vapply(tm$powers[weak], .term_label, ""), collapse = ", "),
                            if (length(weak) > 1) " are" else " is", " not clearly non-zero; prefer the ellipsoid interval")
    }
  }
  out
}

#' Mark intervals that may not keep their coverage on the interval plot
#' @param p Output of [mpi_interval_plot()].
#' @param flags Output of `explorer_interval_flags()`.
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_flag_intervals <- function(p, flags) {
  if (is.null(flags) || all(flags$reliable)) return(p)
  d <- p$data
  k <- match(paste(d$response, d$factor), paste(flags$response, flags$factor))
  d <- d[!is.na(k) & !flags$reliable[k], , drop = FALSE]
  if (!nrow(d)) return(p)
  d$xlab <- ifelse(is.na(d$upper), d$value, d$upper)
  p + ggplot2::geom_text(data = d, ggplot2::aes(x = .data$xlab, y = .data$key), label = "  may be too narrow",
                         hjust = 0, size = 3.2, colour = "#B35900", fontface = "bold") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.3))) +
    ggplot2::labs(caption = paste(c(p$labels$caption,
      "Orange: this interval may not keep its nominal coverage (see the note above the plot)."), collapse = "\n"))
}
