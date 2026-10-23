# Stage 5: follow-up (ranges, subsets, feasible region) --------------------------------

.allowed_calls <- c("+", "-", "*", "/", "^", "<", "<=", ">", ">=", "==", "!=", "&", "|", "!",
                    "(", "abs", "sqrt", "exp", "log", "pmin", "pmax")

#' Build a constraint function from a text expression, safely
#'
#' Only arithmetic, comparisons, logical operators and `abs`, `sqrt`, `exp`,
#' `log`, `pmin`, `pmax` are allowed; every name must be a factor. Anything
#' else is rejected before evaluation.
#' @param text e.g. `"x1 + x2 <= 0"`.
#' @param factors Allowed variable names.
#' @return A function of a data frame returning a logical vector.
#' @keywords internal
#' @noRd
.safe_constraint <- function(text, factors) {
  expr <- tryCatch(str2lang(text), error = function(e) {
    cli::cli_abort("Constraint is not a valid expression: {conditionMessage(e)}")
  })
  check <- function(e) {
    if (is.call(e)) {
      fn <- deparse(e[[1]])
      if (!fn %in% .allowed_calls) cli::cli_abort("Function {.code {fn}} is not allowed in a constraint.")
      for (a in as.list(e)[-1]) check(a)
    } else if (is.symbol(e)) {
      if (!as.character(e) %in% factors) {
        cli::cli_abort("Unknown name {.code {as.character(e)}} in constraint; use factor names: {.val {factors}}.")
      }
    } else if (!(is.numeric(e) || is.logical(e)) || length(e) != 1L) {
      cli::cli_abort("Only numbers, factor names and operators are allowed in a constraint.")
    }
  }
  check(expr)
  function(d) {
    v <- eval(expr, envir = as.list(d), enclos = baseenv())
    if (length(v) == 1L) v <- rep(v, nrow(d))
    as.logical(v)
  }
}

#' Example constraints written with the analysis' own factor names
#' @param factors Character vector of factors.
#' @return Named character vector (label = description, value = expression).
#' @keywords internal
#' @noRd
explorer_constraint_examples <- function(factors) {
  if (length(factors) < 2) return(c("(choose an example)" = ""))
  a <- factors[1]; b <- factors[2]
  ex <- c(sprintf("%s + %s <= 0", a, b), sprintf("abs(%s) + abs(%s) <= 1", a, b),
          sprintf("%s^2 + %s^2 <= 1", a, b), sprintf("%s >= %s", a, b), sprintf("%s <= 0.5", a))
  stats::setNames(c("", ex), c("(choose an example)",
    paste0(ex, c(" - sum limited", " - diamond", " - disc", " - ordered settings", " - one-sided cap"))))
}

#' Number of sampled steps per factor for a sampling method
#' @param method `"lhs"` or `"grid"`.
#' @param size Requested size.
#' @param p Number of factors.
#' @return Integer.
#' @keywords internal
#' @noRd
explorer_n_sampled <- function(method, size, p) {
  if (method == "grid") max(2L, floor(size^(1 / p)))^p else as.integer(size)
}

#' UI of the follow-up stage
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
stage_followup_ui <- function() {
  shiny::tabPanel(
    "5 Follow-up",
    guide_box_ui(5),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Ranges & subset (paper 3.3)",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::p(class = "note", "New coded range per factor; interactions with a restricted factor shrink in the other factors' MPI."),
            shiny::uiOutput("range_inputs"),
            shiny::checkboxGroupInput("keep", "Factors kept", NULL),
            shiny::radioButtons("others", "Dropped factors", c("At centre (paper)" = "centre",
                                                               "Most favourable" = "favourable"))
          ),
          shiny::mainPanel(explorer_plot_output("followup_plot", vh = 58, min = 340, max = 560),
                           explorer_details("Table", shiny::tableOutput("followup_table")))
        )
      ),
      shiny::tabPanel(
        "Feasible region",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::selectInput("feas_response", "Response", NULL),
            shiny::uiOutput("feas_inputs"),
            shiny::uiOutput("constraint_examples"),
            shiny::textInput("constraint", "Extra constraint (optional)", "",
                             placeholder = "e.g. x1 + x2 <= 0"),
            shiny::radioButtons("feas_method", "Method", c("Exact (box only, instant)" = "exact",
                                                           "LHS sampling (any constraint)" = "lhs",
                                                           "Grid sampling (up to 4 factors)" = "grid")),
            shiny::conditionalPanel("input.feas_method != 'exact'",
              shiny::numericInput("feas_size", "Samples per factor", 5000, min = 500, max = 200000, step = 1000),
              shiny::numericInput("feas_seed", "Seed", 1),
              shiny::actionButton("run_feas", "Run sampling", class = "herminR-run"),
              shiny::uiOutput("feas_last"),
              shiny::hr(),
              shiny::h5("Is the sampled value reliable?"),
              shiny::p(class = "note", "Repeats the sampling at 1/8, 1/4, 1/2 and all of the samples, with three seeds, to see whether the maximum has levelled off."),
              shiny::actionButton("run_conv", "Check convergence", class = "herminR-run btn-sm"),
              shiny::uiOutput("conv_last"))
          ),
          shiny::mainPanel(shiny::uiOutput("feas_info"),
                           explorer_plot_output("feas_plot", vh = 42, min = 280, max = 440),
                           explorer_details("Table", shiny::tableOutput("feas_table")),
                           shiny::conditionalPanel("input.feas_method != 'exact'",
                             shiny::uiOutput("conv_info"),
                             explorer_plot_output("conv_plot", vh = 40, min = 260, max = 420)))
        )
      )
    )
  )
}

#' Server of the follow-up stage
#' @param input,output,session Shiny objects.
#' @param st Shared state.
#' @return A list: `followup_mpi`, `feas_res`, `feas_params`, `feas_state`, `read_ranges`.
#' @keywords internal
#' @noRd
stage_followup_server <- function(input, output, session, st) {
  range_ui <- function(prefix) {
    shiny::renderUI({
      fs <- st$analysis()$factors
      lapply(fs, function(f) shiny::fluidRow(
        shiny::column(4, shiny::tags$label(f, style = "margin-top:28px")),
        shiny::column(4, shiny::numericInput(paste0(prefix, f, "_lo"), "low", -1, step = 0.1)),
        shiny::column(4, shiny::numericInput(paste0(prefix, f, "_hi"), "high", 1, step = 0.1))))
    })
  }
  output$range_inputs <- range_ui("rng_")
  output$feas_inputs <- range_ui("fea_")
  read_ranges <- function(prefix) {
    fs <- st$analysis()$factors
    out <- list()
    for (f in fs) {
      lo <- input[[paste0(prefix, f, "_lo")]]; hi <- input[[paste0(prefix, f, "_hi")]]
      if (is.null(lo) || is.null(hi) || is.na(lo) || is.na(hi)) next
      shiny::validate(shiny::need(lo < hi, sprintf("Range for %s: low must be below high.", f)))
      if (lo != -1 || hi != 1) out[[f]] <- c(lo, hi)
    }
    out
  }

  followup_mpi <- shiny::reactive({
    a <- st$analysis()
    m <- suppressWarnings(mpi_multi(st$mpi_input(a), abs = TRUE, factors = a$factors))
    rr <- read_ranges("rng_")
    if (length(rr)) m <- suppressWarnings(mpi_adjust_range(m, rr))
    keep <- input$keep
    shiny::validate(shiny::need(length(keep) > 0, "Keep at least one factor."))
    if (length(keep) < length(a$factors)) m <- mpi_subset(m, keep = keep, others = input$others)
    standardize_mpi(m, "colmax")
  })
  output$followup_plot <- shiny::renderPlot(
    {
      w <- explorer_width(session, "followup_plot")
      p <- plot(followup_mpi(), transpose = TRUE, reorder = FALSE, signed = FALSE, annotate = TRUE,
                text_size = if (nrow(followup_mpi()) > 60) 3 else 4.5) + explorer_theme()
      explorer_highlight_heatmap(explorer_fit_width(explorer_fit_heatmap(p, w), w), st$focus())
    },
    res = 96, alt = "Heatmap of the MPI after the range and subset changes; the table gives the same numbers")
  output$followup_table <- shiny::renderTable(
    tibble::as_tibble(followup_mpi())[, c("response", "factor", "estimate", "estimate_raw")], digits = 4)

  # feasible region ------------------------------------------------------------
  output$constraint_examples <- shiny::renderUI({
    fs <- tryCatch(st$analysis()$factors, error = function(e) character(0))
    shiny::selectInput("constraint_example", "Constraint examples", explorer_constraint_examples(fs))
  })
  shiny::observeEvent(input$constraint_example, {
    if (nzchar(input$constraint_example)) {
      shiny::updateTextInput(session, "constraint", value = input$constraint_example)
      if (identical(input$feas_method, "exact")) shiny::updateRadioButtons(session, "feas_method", selected = "lhs")
    }
  }, ignoreInit = TRUE)

  feas_params <- shiny::reactive({
    list(response = shiny::req(input$feas_response), region = read_ranges("fea_"),
         constraint = trimws(input$constraint %||% ""), method = input$feas_method,
         size = input$feas_size, seed = input$feas_seed)
  })
  feas_compute <- function(p) {
    a <- st$analysis()
    con <- NULL
    if (nzchar(p$constraint)) {
      con <- .safe_constraint(p$constraint, a$factors)
      if (p$method == "exact") stop("A constraint needs a sampling method: choose LHS or grid and press 'Run sampling'.")
    }
    if (p$method == "grid" && length(a$factors) > 4) stop("Grid sampling supports at most 4 factors; use LHS.")
    if (p$method != "exact" && (!is.numeric(p$size) || is.na(p$size) || p$size < 500 || p$size > 200000)) {
      stop("Samples per factor must be between 500 and 200000.")
    }
    res <- mpi_feasible(st$mpi_input(a, p$response)[[1]], region = p$region, constraint = con,
                        method = p$method, size = p$size, seed = p$seed, factors = a$factors)
    if (p$method != "exact") attr(res, "n_sampled") <- explorer_n_sampled(p$method, p$size, length(a$factors))
    res
  }
  feas_state <- explorer_run_on_click(input, session, "run_feas", feas_params, function(p) {
    shiny::withProgress(message = "Sampling the feasible region", value = 0.3, feas_compute(p))
  })
  feas_res <- shiny::reactive({
    p <- feas_params()
    if (identical(p$method, "exact")) {
      tryCatch(feas_compute(p), error = function(e) shiny::validate(conditionMessage(e)))
    } else {
      explorer_run_value(feas_state(), "Sampling is not run automatically: press 'Run sampling'.")
    }
  })
  output$feas_last <- shiny::renderUI(explorer_last_run_ui(
    feas_state(), tryCatch(feas_params(), error = function(e) NULL),
    function(p) sprintf("%s, %s samples, seed %s%s", toupper(p$method), p$size, p$seed,
                        if (nzchar(p$constraint)) paste0(", constraint ", p$constraint) else ""),
    "Run sampling"))
  output$feas_info <- shiny::renderUI({
    r <- feas_res()
    if (attr(r, "method") == "exact") {
      shiny::p(class = "note", shiny::span(class = "tag exact", "Exact"),
               " Box region, second-order model: exact maximal attainable change (L1).")
    } else {
      share <- max(r$n_feasible, na.rm = TRUE) / attr(r, "n_sampled")
      cs <- conv_status()
      verdict <- if (!isTRUE(cs$checked)) {
        shiny::tagList(shiny::span(class = "verdict na", "Convergence not checked"),
                       " Press 'Check convergence' before reporting these values.")
      } else if (length(cs$unstable)) {
        shiny::tagList(shiny::span(class = "verdict no", "Not yet stable"),
                       sprintf(" for %s: increase the samples per factor; report as lower estimates only.", .join_words(cs$unstable)))
      } else {
        shiny::tagList(shiny::span(class = "verdict ok", "Stable under this sampling"),
                       " Numerically stable lower estimates; not a proof that every feasible setting was searched.")
      }
      shiny::div(
        shiny::p(class = "note warn", shiny::span(class = "tag approx", "Approximate"),
                 sprintf(" %s sampling: the sampled maximum is a lower estimate of the true attainable change; more samples tighten it. About %.0f%% of the sampled one-unit steps stay inside the allowed region.",
                         toupper(attr(r, "method")), 100 * min(1, share))),
        shiny::p(class = "note", verdict))
    }
  })
  output$feas_plot <- shiny::renderPlot({
    f <- feas_res()
    p <- explorer_feasible_plot(f)
    cs <- conv_status()
    if (isTRUE(cs$applies)) {
      p <- p + ggplot2::labs(subtitle = if (!isTRUE(cs$checked)) "Lower estimates; convergence not checked" else
        if (length(cs$unstable)) paste0("Lower estimates; not yet stable for ", .join_words(cs$unstable)) else
          "Lower estimates; stable under this sampling")
    }
    explorer_fit_width(explorer_highlight_bars(p, st$focus()) + explorer_theme(),
                       explorer_width(session, "feas_plot"))
  }, res = 96, alt = "Unrestricted and feasible-region MPI of each factor; the table gives the same numbers")
  # convergence of the sampled maximum
  conv_params <- shiny::reactive({
    p <- feas_params()
    shiny::validate(shiny::need(p$method != "exact", "Convergence applies to sampling methods."))
    p
  })
  conv_state <- explorer_run_on_click(input, session, "run_conv", conv_params, function(p) {
    a <- st$analysis()
    con <- if (nzchar(p$constraint)) .safe_constraint(p$constraint, a$factors) else NULL
    if (!is.numeric(p$size) || is.na(p$size) || p$size < 500 || p$size > 200000) stop("Samples per factor must be between 500 and 200000.")
    sizes <- unique(round(p$size * c(0.125, 0.25, 0.5, 1)))
    shiny::withProgress(message = "Checking convergence (12 sampling runs)", value = 0.3,
      mpi_feasible_convergence(st$mpi_input(a, p$response)[[1]], region = p$region, constraint = con,
                               method = p$method, sizes = sizes, seeds = p$seed + 0:2, factors = a$factors))
  })
  conv_res <- shiny::reactive(explorer_run_value(conv_state(), "Press 'Check convergence' to repeat the sampling at several sizes and seeds."))
  # verdict for the result on screen: only a check run with the current settings counts
  conv_status <- shiny::reactive({
    p <- tryCatch(feas_params(), error = function(e) NULL)
    if (is.null(p) || identical(p$method, "exact")) return(list(applies = FALSE))
    cs <- conv_state()
    if (is.null(cs) || !is.null(cs$error) || !identical(cs$params, p)) return(list(applies = TRUE, checked = FALSE))
    s <- attr(cs$value, "summary")
    list(applies = TRUE, checked = TRUE, unstable = s$factor[!s$stable])
  })
  output$conv_last <- shiny::renderUI(explorer_last_run_ui(
    conv_state(), tryCatch(conv_params(), error = function(e) NULL),
    function(p) sprintf("%s up to %s samples, seeds %s-%s", toupper(p$method), p$size, p$seed, p$seed + 2),
    "Check convergence"))
  output$conv_info <- shiny::renderUI({
    s <- attr(conv_res(), "summary")
    shiny::div(class = "note", shiny::h5("Convergence of the sampled maximum"),
               shiny::tags$ul(lapply(explorer_convergence_text(s), shiny::tags$li)))
  })
  output$conv_plot <- shiny::renderPlot(explorer_fit_width(plot(conv_res()) + explorer_theme(), explorer_width(session, "conv_plot")),
                                        res = 96,
                                        alt = "Sampled feasible-region MPI against the number of samples, one panel per factor")

  output$feas_table <- shiny::renderTable({
    r <- tibble::as_tibble(feas_res())
    r[, intersect(c("factor", "mpi_unrestricted", "estimate", "feas_ratio", "direction", "n_feasible"), names(r))]
  }, digits = 4)

  list(followup_mpi = followup_mpi, feas_res = feas_res, feas_params = feas_params,
       feas_state = feas_state, read_ranges = read_ranges, conv_state = conv_state, conv_res = conv_res,
       conv_status = conv_status)
}

#' Bar chart of unrestricted vs feasible MPI
#' @param f Output of [mpi_feasible()].
#' @return A ggplot object.
#' @keywords internal
#' @noRd
explorer_feasible_plot <- function(f) {
  r <- tibble::as_tibble(f)
  df <- rbind(data.frame(factor = r$factor, value = r$mpi_unrestricted, what = "unrestricted"),
              data.frame(factor = r$factor, value = r$estimate, what = "feasible"))
  df$factor <- factor(df$factor, levels = rev(r$factor))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$factor, fill = .data$what)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_manual(values = c(unrestricted = "grey75", feasible = "grey15")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "MPI (L1, attainable change)", y = NULL, fill = NULL,
                  title = if (identical(attr(f, "method"), "exact")) "Unrestricted vs feasible-region MPI (exact)" else
                    "Unrestricted vs feasible-region MPI (approximate, sampled)")
}

#' Plain-language summary of a convergence check
#' @param s The `summary` attribute of [mpi_feasible_convergence()].
#' @return Character vector.
#' @keywords internal
#' @noRd
explorer_convergence_text <- function(s) {
  pct <- function(v) ifelse(is.finite(v), sprintf("%.1f%%", 100 * v), "n/a")
  lines <- vapply(seq_len(nrow(s)), function(i) {
    r <- s[i, ]
    paste0(r$factor, ": ", formatC(r$estimate, digits = 4, format = "g"),
           if (r$stable) " - stable" else " - not yet stable",
           " (spread across seeds ", pct(r$spread), ", change from the previous size ", pct(r$last_change),
           if (!is.na(r$gap_to_exact)) paste0(", ", pct(r$gap_to_exact), " below the exact value") else "",
           "; at least ", r$min_feasible, " feasible pairs).")
  }, "")
  c(lines, if (all(s$stable))
    paste("All factors have levelled off under this sampling: the values are numerically stable lower estimates.",
          "This does not show that every feasible setting was searched, nor that the model is right.") else
    "Some factors are still changing: increase the samples per factor before reporting them, and present them as lower estimates, not as the feasible MPI.")
}
