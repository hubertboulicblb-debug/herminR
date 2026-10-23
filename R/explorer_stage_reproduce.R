# Stage 6: reproduce (script, report, link) ---------------------------------------------

#' UI of the reproduce stage
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
stage_reproduce_ui <- function() {
  shiny::tabPanel(
    "6 Reproduce",
    guide_box_ui(6),
    shiny::fluidRow(
      shiny::column(4,
        shiny::h4("Report"),
        shiny::p(class = "note", "One self-contained HTML file with the findings of every stage, the plots and the R script; open it in any browser, attach it to a lab notebook or print it to PDF."),
        shiny::downloadButton("dl_report", "Download report (HTML)", class = "btn-primary")),
      shiny::column(4,
        shiny::h4("Link to this analysis"),
        shiny::p(class = "note", "Saves every setting in a link, so you can come back to the same view after closing the app. Uploaded files are not stored in the link: re-upload them."),
        shiny::bookmarkButton(label = "Copy a link to this analysis", id = "bookmark")),
      shiny::column(4,
        shiny::h4("R script"),
        shiny::p(class = "note", "Recreates the results on screen with the same settings."),
        shiny::downloadButton("dl_code", "Download script"))
    ),
    shiny::hr(),
    shiny::verbatimTextOutput("code"),
    shiny::h4("Session"),
    shiny::verbatimTextOutput("session")
  )
}

#' Server of the reproduce stage
#' @param input,output,session Shiny objects.
#' @param st Shared state (all earlier stages).
#' @return A list: `code_text`, `report_html`.
#' @keywords internal
#' @noRd
stage_reproduce_server <- function(input, output, session, st) {
  ok_params <- function(state) if (!is.null(state) && is.null(state$error)) state$params else NULL

  contour_code <- function() {
    ca <- tryCatch(st$contour_args(), error = function(e) NULL)
    if (is.null(ca)) return("")
    a <- st$analysis()
    paste0("\n# 2c. What MPI means (contour)\n",
           sprintf('mpi_contour(mpi_multi(input, factors = %s), factor = "%s"%s, response = "%s")\n',
                   deparse1(a$factors), ca$factor, if (!is.null(ca$partner)) sprintf(', partner = "%s"', ca$partner) else "",
                   ca$response))
  }

  code_text <- shiny::reactive({
    a <- st$analysis()
    vr <- st$view_resp()
    used <- st$used()
    std <- switch(input$std, none = "", colmax_response = '\nm <- standardize_mpi(m, "colmax", by = "response")',
                  colmax_factor = '\nm <- standardize_mpi(m, "colmax", by = "factor")',
                  residual_sd = '\nm <- standardize_mpi(m, "residual_sd")', rank = '\nm <- standardize_mpi(m, "rank")')
    rr <- tryCatch(st$read_ranges("rng_"), error = function(e) list())
    fu <- c(
      if (length(rr)) sprintf("f <- mpi_adjust_range(mpi_multi(input, abs = TRUE), %s)", deparse1(rr)) else
        "f <- mpi_multi(input, abs = TRUE)",
      if (length(input$keep) && length(input$keep) < length(a$factors))
        sprintf('f <- mpi_subset(f, keep = %s, others = "%s")', deparse1(input$keep), input$others),
      'plot(standardize_mpi(f, "colmax"), transpose = TRUE, reorder = FALSE, signed = FALSE)')
    # the parameters of the results actually displayed
    bp <- ok_params(st$boot_state())
    sp <- ok_params(st$sel_state())
    fp <- tryCatch({
      p <- st$feas_params()
      if (p$method == "exact") p else ok_params(st$feas_state())
    }, error = function(e) NULL)
    feas_code <- if (!is.null(fp) && (length(fp$region) || nzchar(fp$constraint))) {
      paste0("\n# 5b. Feasible region (", fp$response, ")\n",
             sprintf('fe <- mpi_feasible(input[["%s"]], region = %s,%s method = "%s"%s, factors = %s)\n',
                     fp$response, deparse1(fp$region),
                     if (nzchar(fp$constraint)) sprintf(" constraint = function(d) with(d, %s),", fp$constraint) else "",
                     fp$method,
                     if (fp$method != "exact") sprintf(", size = %s, seed = %s", fp$size, fp$seed) else "",
                     deparse1(a$factors)),
             "fe\n")
    } else ""
    adv_code <- tryCatch({
      if (st$adv$runs() > 0) {
        ap <- st$adv$result()$params
        ba <- adv_bounds_args(ap$interval_method)
        extra <- paste(vapply(setdiff(names(ba), "bounds"), function(n) sprintf(', %s = %s', n, deparse1(ba[[n]])), ""),
                       collapse = "")
        paste0("\n# Advanced run\n",
               if (ba$bounds != "none") sprintf('adv_u <- mpi_multi(input, norm = "%s", bounds = "%s", level = %s, abs = TRUE, factors = %s, gap_tol = %s%s)\n',
                                                ap$norm, ba$bounds, ap$level, deparse1(a$factors), ap$tolerance, extra) else "",
               if (a$kind == "model") sprintf('adv_b <- bootstrap_ranks(input[["%s"]], n = %s, type = "%s", level = %s, norm = "%s", seed = %s)\n',
                                              ap$bootstrap_response, ap$iterations, ap$bootstrap_type, ap$level, ap$norm, ap$seed) else "")
      } else ""
    }, error = function(e) "")
    paste0(
      "library(herminR)\n\n# 1. Data & model (", a$label, ")\n", a$code, "\n\n",
      "# 2. MPI heatmap\n",
      sprintf('m <- mpi_multi(input, norm = "%s", abs = %s, factors = %s)', input$norm,
              input$signed == "abs", deparse1(a$factors)), std, "\n",
      sprintf('plot(m, transpose = %s, annotate = %s, reorder = %s, palette = "%s")',
              isTRUE(input$transpose), isTRUE(input$annotate), isTRUE(input$reorder), input$palette), "\n",
      if ("flows" %in% used) paste0(
        "\n# 2b. Where the influence comes from\n",
        sprintf('m0 <- mpi_multi(input, norm = "%s", abs = %s, factors = %s)\n', input$norm,
                input$signed == "abs", deparse1(a$factors)),
        sprintf('mpi_contributions(m0, response = "%s")\nmpi_sankey(m0, response = "%s")\nmpi_network(m0, response = "%s")\n',
                vr, vr, vr),
        sprintf('mpi_sankey(m0, view = "responses", standardize = "%s", min_value = %s)\n',
                if (isTRUE(input$flow_scale %||% TRUE)) "colmax" else "none", input$flow_min %||% 0)) else "",
      if ("contour" %in% used) contour_code() else "",
      if (a$kind != "coef") paste0("\n# 3. Uncertainty\n",
        sprintf('u <- mpi_multi(input, norm = "%s", bounds = "%s", level = %s, abs = TRUE)\nmpi_interval_plot(u)\n',
                input$norm, input$bounds, input$level),
        if (a$kind == "model" && !is.null(bp)) {
          if ("rank_flow" %in% used) {
            sprintf('br <- bootstrap_ranks(input[["%s"]], n = %s, type = "%s", norm = "%s", level = %s, seed = %s)\nplot(br)\nplot(br, type = "flow")\n',
                    bp$response, bp$n, bp$type, bp$norm, bp$level, bp$seed)
          } else {
            sprintf('plot(bootstrap_ranks(input[["%s"]], n = %s, type = "%s", norm = "%s", level = %s, seed = %s))\n',
                    bp$response, bp$n, bp$type, bp$norm, bp$level, bp$seed)
          }
        }) else "",
      if (!is.null(sp)) paste0("\n# 4. Selection paths\n",
        sprintf('plot(mpi_selection_paths(input[["%s"]], alphas = c(%s), top = %s, norm = "%s"))\n',
                sp$response, paste(strsplit(sp$alphas, "[,; ]+")[[1]], collapse = ", "), sp$top, sp$norm)) else "",
      "\n# 5. Follow-up (ranges & subset)\n", paste(fu, collapse = "\n"), "\n",
      feas_code, adv_code)
  })
  output$code <- shiny::renderText(code_text())
  output$dl_code <- shiny::downloadHandler(filename = function() "herminR_analysis.R",
                                           content = function(file) writeLines(code_text(), file))

  report_html <- function() {
    a <- st$analysis()
    used <- st$used()
    stages <- guide_stages()
    plot_or_null <- function(expr) tryCatch(expr, error = function(e) NULL)
    plots <- list(
      list(),
      list(plot_or_null(st$heatmap_plot()),
           if ("flows" %in% used) plot_or_null(mpi_sankey(st$base_mpi(), response = st$view_resp())),
           if ("flows" %in% used) plot_or_null(mpi_network(st$base_mpi(), response = st$view_resp())),
           if ("contour" %in% used) plot_or_null(st$contour_plot()),
           if (!is.null(st$compare())) plot_or_null(explorer_compare_plot(st$compare()))),
      list(plot_or_null(mpi_interval_plot(st$unc_mpi())),
           if (!is.null(ok_params(st$boot_state()))) plot_or_null(plot(st$boot_res())),
           if ("rank_flow" %in% used && !is.null(ok_params(st$boot_state()))) plot_or_null(plot(st$boot_res(), type = "flow"))),
      list(if (!is.null(ok_params(st$sel_state()))) plot_or_null(plot(st$sel_res())),
           plot_or_null(plot(suppressWarnings(mpi_compare_norms(st$mpi_input(a, input$rob_response %||% st$responses()[1])[[1]], abs = TRUE))))),
      list(plot_or_null(plot(st$followup_mpi(), transpose = TRUE, reorder = FALSE, signed = FALSE)),
           plot_or_null(explorer_feasible_plot(st$feas_res())))
    )
    sections <- lapply(1:5, function(k) {
      list(title = sprintf("%d. %s", k, stages[[k]]$title), question = stages[[k]]$question,
           findings = st$guide$findings(k), plots = Filter(Negate(is.null), plots[[k]]))
    })
    # assumptions and limitations of what is on screen, so a saved report can be read correctly later
    sections[[6]] <- list(title = "Methods and limitations", question = "What do these numbers assume, and what can they not show?",
                          findings = explorer_limitations(st, input), plots = list())
    info <- data.frame(
      item = c("Date", "herminR", "R", "Data", "Responses", "Factors", "Aggregation", "Values",
               "Standardisation", "Interval method / level"),
      value = c(format(Sys.time(), "%Y-%m-%d %H:%M"), as.character(utils::packageVersion("herminR")),
                R.version.string, a$label, paste(st$responses(), collapse = ", "),
                paste(a$factors, collapse = ", "), input$norm, input$signed, input$std,
                paste(input$bounds, input$level)),
      stringsAsFactors = FALSE)
    explorer_report_html(info, sections, code_text())
  }
  output$dl_report <- shiny::downloadHandler(
    filename = function() sprintf("herminR_report_%s.html", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      html <- shiny::withProgress(message = "Building report", value = 0.5, report_html())
      writeLines(html, file, useBytes = TRUE)
    })

  output$session <- shiny::renderText(paste(
    sprintf("herminR %s | R %s | shiny %s", utils::packageVersion("herminR"),
            paste(R.version$major, R.version$minor, sep = "."), utils::packageVersion("shiny")),
    collapse = "\n"))

  list(code_text = code_text, report_html = report_html)
}

#' Assumptions and limitations of the current analysis, for the report
#' @param st Shared state.
#' @param input Shiny input.
#' @return A named list (heading -> sentences).
#' @keywords internal
#' @noRd
explorer_limitations <- function(st, input) {
  a <- tryCatch(st$analysis(), error = function(e) NULL)
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  model <- c(
    "MPI is a property of the fitted model: it is only as good as the model (check residuals and lack of fit).",
    "Values hold inside the coded range [-1, 1]; outside it they extrapolate the fitted surface.",
    "MPI measures how sensitive the fitted surface is to each factor; it supports a causal reading only for factors set independently in a designed experiment.")
  if (!is.null(a) && identical(a$kind, "coef")) model <- c(model, "Only coefficients were supplied: no uncertainty or robustness can be assessed.")
  out <- list(`Model` = model)
  caps <- safe(st$capabilities())
  if (isTRUE(caps$intervals)) {
    note <- paste(vapply(explorer_bounds_note(input$bounds, input$level), function(x) if (is.character(x)) x else "", ""), collapse = "")
    fl <- safe(st$int_flags())
    out[["Intervals"]] <- c(sub("^What this interval means\\. ", "", note),
      if (!is.null(fl) && any(!fl$reliable)) paste0("Flagged in this analysis: ",
        paste(sprintf("%s (%s)", fl$factor[!fl$reliable], fl$response[!fl$reliable]), collapse = ", "), "."))
  }
  bs <- safe(st$boot_state())
  if (!is.null(bs) && is.null(bs$error)) {
    out[["Rank stability"]] <- sprintf("Bootstrap: %s resampling, %s replicates, seed %s. It shows how stable the ranking is, not a confidence interval.",
                                       bs$params$type, bs$params$n, bs$params$seed)
  }
  cs <- safe(st$conv_status())
  if (isTRUE(cs$applies)) {
    out[["Feasible region"]] <- c("Sampled feasible-region values are lower estimates of the true maximum.",
      if (!isTRUE(cs$checked)) "Convergence was not checked for these settings." else
        if (length(cs$unstable)) paste0("Not yet stable for ", .join_words(cs$unstable), ".") else
          "Stable under this sampling (numerical stability, not proof that every feasible setting was searched).")
  }
  out
}
