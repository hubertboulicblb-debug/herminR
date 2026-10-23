# Advanced settings and advanced results for the explorer ----------------------
#
# Everything here is opt-in. The standard explorer outputs never read the
# advanced inputs, so they are unchanged whatever is set in the panel; the
# advanced analysis runs only when "Run advanced analysis" is clicked.

#' Default advanced settings
#'
#' Each default equals the behaviour the explorer had before the advanced
#' panel existed: 200 bootstrap replicates and seed 1 (the Uncertainty tab's
#' defaults), the ellipsoid solver's duality-gap tolerance of \eqn{10^{-6}}
#' (hard-coded in [mpi_ellipsoid()]), ellipsoid intervals with an F critical
#' value (the default of [mpi_ellipsoid()] and of the Uncertainty tab), no
#' verbose logging and no caching.
#'
#' @return A named list with elements `iterations`, `tolerance`, `seed`,
#'   `method`, `verbose`, `cache`.
#' @keywords internal
#' @noRd
adv_defaults <- function() {
  list(iterations = 200L, tolerance = 1e-6, seed = 1L, method = "ellipsoid_F",
       verbose = FALSE, cache = FALSE)
}

#' Allowed ranges for the numeric advanced settings
#' @return Named list of `c(min, max)` vectors.
#' @keywords internal
#' @noRd
adv_limits <- function() {
  list(iterations = c(20, 5000), tolerance = c(1e-12, 1e-2), seed = c(0, .Machine$integer.max))
}

#' Interval methods offered in the advanced panel
#'
#' Each maps to arguments of [mpi()] / [mpi_multi()] (and through them to
#' [mpi_ellipsoid()], [mpi_delta()], [mpi_se()]).
#' @return Named character vector (labels = names, values = codes).
#' @keywords internal
#' @noRd
adv_methods <- function() {
  c("Ellipsoid, F critical value (default)" = "ellipsoid_F",
    "Ellipsoid, chi-squared critical value" = "ellipsoid_chisq",
    "Delta method (t)" = "delta",
    "SE soft-threshold" = "se",
    "SE soft-threshold, Bonferroni" = "se_bonferroni",
    "No intervals" = "none")
}

#' Translate an interval-method code into mpi() arguments
#' @param method One of the values of `adv_methods()`.
#' @return A list of arguments (`bounds`, and `crit` or `k_adj` when relevant).
#' @keywords internal
#' @noRd
adv_bounds_args <- function(method) {
  switch(method,
    ellipsoid_F = list(bounds = "ellipsoid", crit = "F"),
    ellipsoid_chisq = list(bounds = "ellipsoid", crit = "chisq"),
    delta = list(bounds = "delta"),
    se = list(bounds = "se", k_adj = FALSE),
    se_bonferroni = list(bounds = "se", k_adj = TRUE),
    none = list(bounds = "none"),
    cli::cli_abort("Unknown interval method {.val {method}}."))
}

#' Validate advanced settings
#'
#' Checks each value and each combination with the current analysis, so that
#' nothing invalid reaches the calculation functions.
#'
#' @param settings A list like `adv_defaults()`.
#' @param context A list with `kind` (`"model"`, `"table"` or `"coef"`),
#'   `norm` (`"l1"`, `"l2"`, `"linf"`), `std` (the standardisation code of the
#'   MPI tab) and `max_group` (largest number of terms in any factor group).
#' @return A character vector of user-facing error messages; empty when the
#'   settings are valid.
#' @keywords internal
#' @noRd
adv_validate <- function(settings, context = list()) {
  lim <- adv_limits()
  msg <- character(0)
  num_ok <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  whole <- function(x) num_ok(x) && abs(x - round(x)) < 1e-8
  fmt <- function(x) format(x, scientific = x < 1e-3 || x > 1e6, big.mark = "")

  if (!whole(settings$iterations) || settings$iterations < lim$iterations[1] ||
      settings$iterations > lim$iterations[2]) {
    msg <- c(msg, sprintf("Bootstrap replicates must be a whole number from %s to %s.",
                          fmt(lim$iterations[1]), fmt(lim$iterations[2])))
  }
  if (!num_ok(settings$tolerance) || settings$tolerance < lim$tolerance[1] ||
      settings$tolerance > lim$tolerance[2]) {
    msg <- c(msg, sprintf("Solver tolerance must be a number from %s to %s.",
                          fmt(lim$tolerance[1]), fmt(lim$tolerance[2])))
  }
  if (!whole(settings$seed) || settings$seed < lim$seed[1] || settings$seed > lim$seed[2]) {
    msg <- c(msg, sprintf("Random seed must be a whole number from 0 to %s.", fmt(lim$seed[2])))
  }
  if (!is.character(settings$method) || length(settings$method) != 1L ||
      !settings$method %in% adv_methods()) {
    msg <- c(msg, "Choose an interval method from the list.")
  }
  for (nm in c("verbose", "cache")) {
    if (!is.logical(settings[[nm]]) || length(settings[[nm]]) != 1L || is.na(settings[[nm]])) {
      msg <- c(msg, sprintf("'%s' must be TRUE or FALSE.", nm))
    }
  }
  # combinations with the current analysis
  kind <- context$kind
  if (!is.null(kind) && kind == "coef" && isTRUE(settings$method %in% setdiff(adv_methods(), "none"))) {
    msg <- c(msg, paste("Intervals need fitted models or a coefficient table with standard errors;",
                        "choose 'No intervals' or another data source."))
  }
  if (identical(context$std, "residual_sd") && !identical(kind, "model")) {
    msg <- c(msg, "Residual-SD standardisation needs fitted models; change the standardisation in the MPI tab.")
  }
  if (isTRUE(settings$method %in% c("ellipsoid_F", "ellipsoid_chisq")) && identical(context$norm, "l1") &&
      !is.null(context$max_group) && context$max_group > 16) {
    msg <- c(msg, sprintf(paste("The L1 ellipsoid maximum enumerates 2^g sign vectors and is limited to",
                                "16 terms per factor; this analysis has a factor with %d terms.",
                                "Choose the delta or SE method."), context$max_group))
  }
  msg
}

#' Largest number of terms in any factor group across responses
#' @param inputs Named list of models / coefficient vectors / coefficient tables.
#' @return Integer.
#' @keywords internal
#' @noRd
adv_max_group <- function(inputs) {
  g <- vapply(inputs, function(x) {
    tm <- suppressWarnings(.term_map(x))
    if (!length(tm$factors)) return(0L)
    max(vapply(tm$factors, function(f) sum(vapply(tm$powers, function(p) f %in% names(p), logical(1))),
               integer(1)))
  }, integer(1))
  if (length(g)) max(g) else 0L
}

# Cache ------------------------------------------------------------------------

#' Create an intermediate-results cache
#'
#' A per-session environment store keyed by a hash of each step's inputs.
#' Because every key includes all parameters (and a fingerprint of the data)
#' that the step depends on, changing any of them produces a new key: stale
#' results are never returned.
#' @return An environment with `store` (environment), `hits`, `misses`.
#' @keywords internal
#' @noRd
adv_cache_new <- function() {
  e <- new.env(parent = emptyenv())
  e$store <- new.env(parent = emptyenv())
  e$hits <- 0L
  e$misses <- 0L
  e
}

#' Fetch a cached value or compute and store it
#' @param cache A cache from `adv_cache_new()`, or `NULL` to disable caching.
#' @param key A list of everything the result depends on; it is hashed.
#' @param compute A function of no arguments that computes the value.
#' @return A list with `value` and `hit` (logical).
#' @keywords internal
#' @noRd
adv_cached <- function(cache, key, compute) {
  if (is.null(cache)) return(list(value = compute(), hit = FALSE))
  h <- rlang::hash(key)
  if (exists(h, envir = cache$store, inherits = FALSE)) {
    cache$hits <- cache$hits + 1L
    return(list(value = get(h, envir = cache$store, inherits = FALSE), hit = TRUE))
  }
  v <- compute()
  assign(h, v, envir = cache$store)
  cache$misses <- cache$misses + 1L
  list(value = v, hit = FALSE)
}

#' Number of entries in a cache
#' @param cache A cache from `adv_cache_new()`.
#' @return Integer.
#' @keywords internal
#' @noRd
adv_cache_size <- function(cache) length(ls(cache$store, all.names = TRUE))

#' Fingerprint of the analysis inputs (data and models)
#'
#' Hashes what determines the results: for fitted models the formula, the
#' coefficients and the model frame; for coefficient inputs the values.
#' @param inputs Named list of models / coefficient vectors / tables.
#' @return A hash string.
#' @keywords internal
#' @noRd
adv_fingerprint <- function(inputs) {
  rlang::hash(lapply(inputs, function(x) {
    if (inherits(x, c("lm", "rsm"))) {
      list(formula = paste(deparse(stats::formula(x)), collapse = ""), coef = stats::coef(x),
           frame = as.list(stats::model.frame(x)))
    } else {
      x
    }
  }))
}

# The timed, cached pipeline ------------------------------------------------------

#' Run the advanced analysis
#'
#' Runs the explorer's calculation steps one by one, timing each with
#' [system.time()], optionally caching each step, and collecting messages and
#' warnings into a log.
#'
#' @param inputs Named list of models / coefficient vectors / coefficient
#'   tables (one per response), as used by the explorer.
#' @param kind `"model"`, `"table"` or `"coef"`.
#' @param factors Character vector of factors.
#' @param settings Validated advanced settings (see `adv_defaults()`).
#' @param standard Standard-view settings: `norm`, `abs` (logical), `std`
#'   (standardisation code), `level`, `boot_type`, `response` (the response
#'   used for the bootstrap).
#' @param cache A cache from `adv_cache_new()`, or `NULL`.
#' @return A list of class `herminR_advanced` with `params`, `timings`,
#'   `terms`, `mpi`, `standardized`, `bounds`, `bootstrap`, `log`, `info`.
#' @keywords internal
#' @noRd
adv_run <- function(inputs, kind, factors, settings, standard, cache = NULL) {
  errs <- adv_validate(settings, list(kind = kind, norm = standard$norm, std = standard$std,
                                      max_group = adv_max_group(inputs)))
  if (length(errs)) cli::cli_abort(errs)
  if (!isTRUE(settings$cache)) cache <- NULL

  log <- character(0)
  add_log <- function(...) {
    line <- paste0(format(Sys.time(), "%H:%M:%OS3"), "  ", paste0(...))
    log <<- c(log, line)
    if (isTRUE(settings$verbose)) message("[herminR] ", paste0(...))
  }
  timings <- data.frame(step = character(0), status = character(0), seconds = numeric(0),
                        cache = character(0), stringsAsFactors = FALSE)
  fp <- adv_fingerprint(inputs)
  step <- function(name, key, compute, skip = NULL) {
    if (!is.null(skip)) {
      add_log(name, ": skipped (", skip, ")")
      timings[nrow(timings) + 1, ] <<- list(name, paste("skipped:", skip), NA_real_, "-")
      return(NULL)
    }
    add_log(name, ": start")
    out <- NULL
    tm <- system.time(out <- withCallingHandlers(
      adv_cached(cache, c(list(step = name, data = fp), key), compute),
      warning = function(w) { add_log(name, ": warning: ", conditionMessage(w)); invokeRestart("muffleWarning") },
      message = function(m) { add_log(name, ": ", trimws(conditionMessage(m))); invokeRestart("muffleMessage") }
    ))
    cache_lab <- if (is.null(cache)) "off" else if (out$hit) "hit" else "miss"
    add_log(name, sprintf(": done in %.3f s (cache %s)", tm[["elapsed"]], cache_lab))
    timings[nrow(timings) + 1, ] <<- list(name, "ok", unname(tm[["elapsed"]]), cache_lab)
    out$value
  }

  terms <- step("1 Term mapping", list(), function() lapply(inputs, .term_map))
  mpi_key <- list(norm = standard$norm, abs = standard$abs, factors = factors)
  base <- step("2 MPI", mpi_key, function() {
    mpi_multi(inputs, norm = standard$norm, abs = standard$abs, factors = factors)
  })
  std <- step("3 Standardisation", c(mpi_key, std = standard$std), function() {
    switch(standard$std,
      none = base,
      colmax_response = standardize_mpi(base, "colmax", by = "response"),
      colmax_factor = standardize_mpi(base, "colmax", by = "factor"),
      residual_sd = standardize_mpi(base, "residual_sd"),
      rank = standardize_mpi(base, "rank"))
  })
  ba <- adv_bounds_args(settings$method)
  bounds <- step("4 Uncertainty intervals",
    list(norm = standard$norm, method = settings$method, level = standard$level,
         tolerance = settings$tolerance, factors = factors),
    function() {
      do.call(mpi_multi, c(list(inputs, norm = standard$norm, level = standard$level, abs = TRUE,
                                factors = factors, gap_tol = settings$tolerance), ba))
    },
    skip = if (ba$bounds == "none") "interval method 'No intervals'")
  boot <- step("5 Bootstrap ranks",
    list(norm = standard$norm, response = standard$response, n = settings$iterations,
         type = standard$boot_type, level = standard$level, seed = settings$seed),
    function() {
      bootstrap_ranks(inputs[[standard$response]], n = as.integer(settings$iterations),
                      type = standard$boot_type, level = standard$level, norm = standard$norm,
                      seed = as.integer(settings$seed))
    },
    skip = if (kind != "model") "needs fitted models")

  params <- list(
    data_source_kind = kind, responses = names(inputs), factors = factors,
    norm = standard$norm, values = if (isTRUE(standard$abs)) "absolute" else "signed",
    standardisation = standard$std, level = standard$level,
    bootstrap_response = standard$response, bootstrap_type = standard$boot_type,
    iterations = as.integer(settings$iterations), tolerance = settings$tolerance,
    seed = as.integer(settings$seed), interval_method = settings$method,
    verbose = settings$verbose, cache = settings$cache)
  structure(list(
    params = params, timings = timings, terms = terms, mpi = base, standardized = std,
    bounds = bounds, bootstrap = boot, log = log,
    info = list(time = Sys.time(), herminR = as.character(utils::packageVersion("herminR")),
                R = R.version.string,
                cache = if (is.null(cache)) NULL else list(hits = cache$hits, misses = cache$misses,
                                                           entries = adv_cache_size(cache)))
  ), class = "herminR_advanced")
}

#' Run parameters as a two-column table
#' @param result Output of `adv_run()`.
#' @return A data frame with `parameter` and `value`.
#' @keywords internal
#' @noRd
adv_params_table <- function(result) {
  p <- result$params
  data.frame(parameter = names(p),
             value = vapply(p, function(v) paste(format(v), collapse = ", "), character(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Diagnostics table of intermediate values
#'
#' One row per response and factor: raw estimate, standardised value, SE,
#' interval bounds, direction, number of terms, and (for the bootstrap
#' response) the rank summaries.
#' @param result Output of `adv_run()`.
#' @return A data frame.
#' @keywords internal
#' @noRd
adv_diagnostics_table <- function(result) {
  m <- tibble::as_tibble(result$mpi)
  s <- tibble::as_tibble(result$standardized)
  key <- paste(m$response, m$factor)
  out <- data.frame(response = m$response, factor = m$factor, estimate = m$estimate,
                    standardized = s$estimate[match(key, paste(s$response, s$factor))],
                    se = m$se, direction = m$direction, terms_contributed = m$terms_contributed,
                    stringsAsFactors = FALSE)
  if (!is.null(result$bounds)) {
    b <- tibble::as_tibble(result$bounds)
    i <- match(key, paste(b$response, b$factor))
    out$interval_lower <- b$lower[i]
    out$interval_upper <- b$upper[i]
  }
  if (!is.null(result$bootstrap)) {
    br <- tibble::as_tibble(result$bootstrap)
    i <- match(out$factor, br$factor)
    i[out$response != result$params$bootstrap_response] <- NA
    out$rank_mean <- br$rank_mean[i]
    out$rank_lower <- br$rank_lower[i]
    out$rank_upper <- br$rank_upper[i]
    out$top1_frac <- br$top1_frac[i]
  }
  out
}

# UI -------------------------------------------------------------------------------

#' Collapsible advanced-settings section for the MPI sidebar
#'
#' A native HTML `<details>` element (collapsed by default), so no extra UI
#' framework is needed.
#' @return A Shiny tag.
#' @keywords internal
#' @noRd
adv_settings_ui <- function() {
  d <- adv_defaults()
  lim <- adv_limits()
  shiny::tags$details(
    id = "adv_panel", class = "adv-settings",
    shiny::tags$summary("Advanced settings"),
    shiny::p(class = "note", "Defaults reproduce the standard results exactly. Changes take effect only when you press 'Run advanced analysis'."),
    shiny::numericInput("adv_iter", "Bootstrap replicates (iterations)", d$iterations,
                        min = lim$iterations[1], max = lim$iterations[2], step = 10),
    shiny::numericInput("adv_tol", "Solver tolerance (ellipsoid duality gap)", d$tolerance,
                        min = lim$tolerance[1], max = lim$tolerance[2], step = 1e-7),
    shiny::numericInput("adv_seed", "Random seed", d$seed, min = lim$seed[1], max = lim$seed[2], step = 1),
    shiny::selectInput("adv_method", "Interval method", adv_methods(), selected = d$method),
    shiny::checkboxInput("adv_verbose", "Enable verbose logging", d$verbose),
    shiny::checkboxInput("adv_cache", "Cache intermediate results", d$cache),
    shiny::checkboxInput("adv_show", "Show Advanced View tab", FALSE),
    shiny::uiOutput("adv_check"),
    shiny::actionButton("adv_run", "Run advanced analysis"),
    shiny::uiOutput("adv_status")
  )
}

#' Advanced View tab (hidden until enabled)
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
adv_view_ui <- function() {
  shiny::tabPanel(
    "Advanced View", value = "advanced",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h5("Heatmap clustering"),
        shiny::checkboxInput("adv_cluster", "Cluster rows and columns", TRUE),
        shiny::selectInput("adv_linkage", "Linkage", c("complete", "average", "single", "ward.D2", "mcquitty")),
        shiny::selectInput("adv_dist", "Distance", c("euclidean", "manhattan", "maximum", "canberra")),
        shiny::hr(),
        shiny::downloadButton("adv_dl_csv", "Diagnostics (CSV)"),
        shiny::downloadButton("adv_dl_rds", "Full output (.rds)")
      ),
      shiny::mainPanel(
        shiny::uiOutput("adv_view_info"),
        shiny::h4("Run parameters"),
        shiny::tableOutput("adv_params"),
        shiny::h4("Timing of each step"),
        shiny::tableOutput("adv_timings"),
        shiny::h4("Heatmap"),
        explorer_plot_output("adv_heatmap", vh = 60, min = 340, max = 600),
        shiny::conditionalPanel("input.adv_verbose", shiny::h4("Log"), shiny::verbatimTextOutput("adv_log"))
      )
    )
  )
}

# Server -----------------------------------------------------------------------------

#' Server logic for the advanced settings and Advanced View
#'
#' @param input,output,session Shiny server arguments.
#' @param analysis The explorer's `analysis()` reactive.
#' @param inputs_of Function returning the list of models / coefficients for
#'   an analysis (the explorer's `mpi_input()`).
#' @param responses The explorer's `responses()` reactive.
#' @return A list of reactives (`result`, `runs`) and the session cache, for
#'   testing.
#' @keywords internal
#' @noRd
adv_server <- function(input, output, session, analysis, inputs_of, responses) {
  cache <- adv_cache_new()
  runs <- shiny::reactiveVal(0L)

  set_tab <- function(show) {
    if (is.function(session$sendChangeTabVisibility) || !inherits(session, "MockShinySession")) {
      # the "More" menu holds only the Advanced View: show and hide them together
      if (show) {
        shiny::showTab("tabs", "more", session = session)
        shiny::showTab("tabs", "advanced", session = session)
      } else {
        shiny::hideTab("tabs", "advanced", session = session)
        shiny::hideTab("tabs", "more", session = session)
      }
    }
  }
  shiny::observeEvent(input$adv_show, set_tab(isTRUE(input$adv_show)), ignoreNULL = FALSE)

  current_settings <- function() {
    list(iterations = input$adv_iter, tolerance = input$adv_tol, seed = input$adv_seed,
         method = input$adv_method, verbose = isTRUE(input$adv_verbose), cache = isTRUE(input$adv_cache))
  }
  context <- function(a) {
    list(kind = a$kind, norm = input$norm, std = input$std, max_group = adv_max_group(inputs_of(a)))
  }

  # live feedback on the settings (cheap: no calculation)
  output$adv_check <- shiny::renderUI({
    errs <- adv_validate(current_settings(), context(analysis()))
    if (length(errs)) {
      shiny::div(class = "note warn", shiny::tags$ul(lapply(errs, shiny::tags$li)))
    }
  })

  result <- shiny::eventReactive(input$adv_run, {
    a <- analysis()
    settings <- current_settings()
    errs <- adv_validate(settings, context(a))
    shiny::validate(shiny::need(length(errs) == 0, paste(errs, collapse = "\n")))
    resp <- if (!is.null(input$unc_response) && input$unc_response %in% responses()) input$unc_response else responses()[1]
    standard <- list(norm = input$norm, abs = identical(input$signed, "abs"), std = input$std,
                     level = if (is.null(input$level)) 0.95 else input$level,
                     boot_type = if (is.null(input$boot_type)) "residual" else input$boot_type,
                     response = resp)
    res <- shiny::withProgress(message = "Advanced analysis", {
      adv_run(inputs_of(a), a$kind, a$factors, settings, standard, cache = cache)
    })
    runs(runs() + 1L)
    res
  })

  output$adv_status <- shiny::renderUI({
    shiny::req(input$adv_run > 0)
    r <- result()
    tot <- sum(r$timings$seconds, na.rm = TRUE)
    hits <- sum(r$timings$cache == "hit")
    shiny::p(class = "note", sprintf("Run %d done in %.2f s%s.", runs(), tot,
                                     if (isTRUE(r$params$cache)) sprintf(" (%d of %d steps from cache)", hits,
                                                                         sum(r$timings$status == "ok")) else ""),
             if (!isTRUE(input$adv_show)) " Tick 'Show Advanced View tab' to see the results.")
  })

  output$adv_view_info <- shiny::renderUI({
    if (input$adv_run == 0) {
      return(shiny::p(class = "note", "Press 'Run advanced analysis' in the MPI explorer's Advanced settings."))
    }
    r <- result()
    shiny::p(class = "note", sprintf("Results of run %d (%s), herminR %s.", runs(),
                                     format(r$info$time, "%H:%M:%S"), r$info$herminR))
  })
  output$adv_params <- shiny::renderTable({
    shiny::req(input$adv_run > 0)
    adv_params_table(result())
  })
  output$adv_timings <- shiny::renderTable({
    shiny::req(input$adv_run > 0)
    t <- result()$timings
    t$seconds <- ifelse(is.na(t$seconds), "", sprintf("%.3f", t$seconds))
    t
  })
  output$adv_heatmap <- shiny::renderPlot({
    shiny::req(input$adv_run > 0)
    r <- result()
    p <- plot(r$standardized, transpose = isTRUE(input$transpose), palette = input$palette,
              annotate = isTRUE(input$annotate), reorder = isTRUE(input$adv_cluster),
              cluster_method = input$adv_linkage, dist_method = input$adv_dist,
              signed = if (identical(input$signed, "abs") || identical(input$std, "rank")) FALSE else NULL,
              text_size = if (nrow(r$standardized) > 60) 3 else 4.5) +
      ggplot2::theme(text = ggplot2::element_text(size = 14))
    w <- explorer_width(session, "adv_heatmap")
    explorer_fit_width(explorer_fit_heatmap(p, w), w)
  }, res = 96, alt = "Heatmap of the advanced analysis")
  output$adv_log <- shiny::renderText({
    shiny::req(input$adv_run > 0)
    paste(result()$log, collapse = "\n")
  })
  output$adv_dl_csv <- shiny::downloadHandler(
    filename = function() sprintf("herminR_diagnostics_run%d.csv", runs()),
    content = function(file) utils::write.csv(adv_diagnostics_table(result()), file, row.names = FALSE))
  output$adv_dl_rds <- shiny::downloadHandler(
    filename = function() sprintf("herminR_advanced_run%d.rds", runs()),
    content = function(file) saveRDS(result(), file))

  list(result = result, runs = runs, cache = cache)
}
