# Stage 1: data & model --------------------------------------------------------------

.code_columns <- function(d, factors) {
  coding <- data.frame(factor = factors, low = NA_real_, high = NA_real_)
  for (i in seq_along(factors)) {
    x <- d[[factors[i]]]
    lo <- min(x, na.rm = TRUE); hi <- max(x, na.rm = TRUE)
    coding$low[i] <- lo; coding$high[i] <- hi
    if (hi > lo) d[[factors[i]]] <- (x - (hi + lo) / 2) / ((hi - lo) / 2)
  }
  list(data = d, coding = coding)
}

.rsm_formula <- function(response, factors, order = c("SO", "FO+TWI", "FO"), block = NULL) {
  order <- match.arg(order)
  terms <- factors
  if (order != "FO" && length(factors) > 1) {
    terms <- c(terms, apply(utils::combn(factors, 2), 2, paste, collapse = ":"))
  }
  if (order == "SO") terms <- c(terms, sprintf("I(%s^2)", factors))
  if (!is.null(block) && nzchar(block)) terms <- c(block, terms)
  stats::reformulate(terms, response = response)
}

.fit_models <- function(d, responses, factors, order, block = NULL, code = TRUE) {
  if (!length(responses)) cli::cli_abort("Choose at least one response.")
  if (!length(factors)) cli::cli_abort("Choose at least one factor.")
  coding <- NULL
  if (code) {
    cd <- .code_columns(d, factors)
    d <- cd$data
    coding <- cd$coding
  }
  if (!is.null(block) && nzchar(block)) d[[block]] <- factor(d[[block]])
  models <- lapply(stats::setNames(responses, responses), function(r) {
    f <- .rsm_formula(r, factors, order, block)
    m <- stats::lm(f, data = d)
    m$call$formula <- f
    m$data <- d   # lets bootstrap / selection paths refit on the same (coded) data
    m
  })
  list(models = models, data = d, coding = coding)
}

#' UI of the data stage
#' @return A `shiny::tabPanel`.
#' @keywords internal
#' @noRd
stage_data_ui <- function() {
  shiny::tabPanel(
    "1 Data & model",
    guide_box_ui(1),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons("source", "Source", c(
          "Paper Table 1 (pastry, 4 responses)" = "pastry_table1",
          "Paper simulation (11 factors x 20 responses)" = "dss_sim",
          "Pastry dough data (fit a model)" = "pastrydough",
          "Upload data (CSV)" = "upload_data",
          "Upload coefficient table (CSV)" = "upload_coef"
        )),
        shiny::conditionalPanel(
          "input.source == 'upload_data' || input.source == 'upload_coef'",
          shiny::fileInput("file", "CSV file", accept = c(".csv", "text/csv"))
        ),
        shiny::conditionalPanel(
          "input.source == 'upload_coef'",
          shiny::p(class = "note", "Columns: response, term, estimate and optionally se. Terms use",
                   shiny::code("x1"), shiny::code("x1^2"), shiny::code("x1:x2"), "notation, coded units.",
                   "Include", shiny::code("se"), "to unlock uncertainty intervals.")
        ),
        shiny::conditionalPanel(
          "input.source == 'pastrydough' || input.source == 'upload_data'",
          shiny::selectizeInput("responses", "Responses", NULL, multiple = TRUE),
          shiny::selectizeInput("factors", "Factors", NULL, multiple = TRUE),
          shiny::selectInput("block", "Block (optional)", NULL),
          shiny::radioButtons("order", "Model", c("Second order (SO)" = "SO",
                                                  "Linear + interactions" = "FO+TWI",
                                                  "Linear (FO)" = "FO")),
          shiny::checkboxInput("code", "Code factors to [-1, 1] (min/max)", TRUE)
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput("source_info"),
        shiny::h4("Coefficients used for MPI"),
        shiny::tableOutput("coef_table"),
        shiny::uiOutput("nuisance_info"),
        shiny::uiOutput("coding_info")
      )
    )
  )
}

#' Server of the data stage
#' @param input,output,session Shiny objects.
#' @return A list of reactives: `analysis`, `responses`, `term_maps`,
#'   `capabilities`, and the helper function `mpi_input`.
#' @keywords internal
#' @noRd
stage_data_server <- function(input, output, session) {
  env <- environment()
  utils::data("pastry_table1", "dss_sim", "pastrydough", package = "herminR", envir = env)

  upload <- shiny::reactive({
    shiny::req(input$file)
    utils::read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })

  raw_data <- shiny::reactive({
    switch(input$source,
      pastrydough = env$pastrydough,
      upload_data = upload(),
      NULL)
  })

  # selections saved in a bookmark link, applied once when the data is loaded
  restored <- shiny::reactiveVal(NULL)
  shiny::onRestore(function(state) restored(state$input[c("responses", "factors", "block")]))

  shiny::observeEvent(raw_data(), {
    d <- raw_data()
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    if (identical(input$source, "pastrydough")) {
      resp <- "y"; fac <- c("x1", "x2", "x3"); blk <- "block"
    } else {
      resp <- utils::tail(num, 1); fac <- setdiff(num, resp); blk <- ""
    }
    rs <- restored()
    if (!is.null(rs)) {
      if (length(rs$responses) && all(unlist(rs$responses) %in% num)) resp <- unlist(rs$responses)
      if (length(rs$factors) && all(unlist(rs$factors) %in% num)) fac <- unlist(rs$factors)
      if (!is.null(rs$block) && rs$block %in% c("", names(d))) blk <- rs$block
      restored(NULL)
    }
    shiny::updateSelectizeInput(session, "responses", choices = num, selected = resp)
    shiny::updateSelectizeInput(session, "factors", choices = num, selected = fac)
    shiny::updateSelectInput(session, "block", choices = c("(none)" = "", names(d)), selected = blk)
  })

  # analysis object: models (fitted) or coefficient vectors, with metadata
  analysis <- shiny::reactive({
    src <- input$source
    if (src == "pastry_table1") {
      return(list(kind = "coef", coefs = env$pastry_table1, factors = c("x1", "x2", "x3"),
                  label = "paper Table 1 (pastry)", code = "data(pastry_table1); input <- pastry_table1"))
    }
    if (src == "dss_sim") {
      return(list(kind = "coef", coefs = env$dss_sim$coefs, factors = paste0("Factor_", 1:11),
                  label = "paper simulation (seed 1234)", code = "data(dss_sim); input <- dss_sim$coefs"))
    }
    if (src == "upload_coef") {
      d <- upload()
      if (!all(c("response", "term", "estimate") %in% names(d))) {
        shiny::validate("The coefficient table needs columns response, term, estimate (and optionally se).")
      }
      for (v in intersect(c("estimate", "se"), names(d))) {
        shiny::validate(shiny::need(is.numeric(d[[v]]), sprintf("Column '%s' of the coefficient table must contain numbers.", v)))
      }
      if ("se" %in% names(d)) {
        shiny::validate(shiny::need(all(is.na(d$se) | d$se >= 0), "Column 'se' must not contain negative standard errors."))
      }
      sp <- split(d, d$response)
      has_se <- "se" %in% names(d)
      tabs <- lapply(sp, function(x) x[, intersect(c("term", "estimate", "se"), names(x)), drop = FALSE])
      fs <- unique(unlist(lapply(tabs, function(t) .term_map(stats::setNames(t$estimate, t$term))$factors)))
      return(list(kind = if (has_se) "table" else "coef", tables = tabs,
                  coefs = lapply(tabs, function(t) stats::setNames(t$estimate, t$term)),
                  factors = .natural_sort(fs), label = paste("uploaded table", input$file$name),
                  code = sprintf('tab <- read.csv("%s")\ninput <- lapply(split(tab, tab$response), function(x) %s)',
                                 input$file$name,
                                 if (has_se) "x[, c(\"term\", \"estimate\", \"se\")]" else "setNames(x$estimate, x$term)")))
    }
    d <- shiny::req(raw_data())
    shiny::req(input$responses, input$factors)
    resp <- setdiff(input$responses, input$factors)
    shiny::validate(shiny::need(length(resp) > 0, "Responses and factors must differ."))
    chk <- explorer_check_data(d, resp, input$factors, input$block %||% "", input$order)
    shiny::validate(shiny::need(!length(chk$errors), paste(chk$errors, collapse = "\n")))
    fit <- shiny::withProgress(
      message = sprintf("Fitting %d model%s", length(resp), if (length(resp) == 1) "" else "s"), value = 0.5,
      tryCatch(.fit_models(d, resp, input$factors, input$order, block = input$block, code = isTRUE(input$code)),
               error = function(e) shiny::validate(conditionMessage(e))))
    f_txt <- deparse(.rsm_formula("RESP", input$factors, input$order, input$block), width.cutoff = 500L)
    data_code <- if (src == "pastrydough") "data(pastrydough); d <- pastrydough" else sprintf('d <- read.csv("%s")', input$file$name)
    coding_code <- if (isTRUE(input$code)) {
      sprintf(paste0("for (f in c(%s)) d[[f]] <- (d[[f]] - (max(d[[f]]) + min(d[[f]])) / 2) /\n",
                     "  ((max(d[[f]]) - min(d[[f]])) / 2)   # code to [-1, 1]\n"),
              paste(sprintf('"%s"', input$factors), collapse = ", "))
    } else ""
    block_code <- if (nzchar(input$block %||% "")) sprintf('d[["%s"]] <- factor(d[["%s"]])\n', input$block, input$block) else ""
    model_code <- sprintf('input <- list(%s)',
                          paste(sprintf('%s = lm(%s, data = d)', resp,
                                        vapply(resp, function(r) sub("RESP", r, f_txt, fixed = TRUE), "")),
                                collapse = ",\n              "))
    chk_fit <- explorer_check_fit(fit$models, input$order)
    shiny::validate(shiny::need(!length(chk_fit$errors), paste(chk_fit$errors, collapse = "\n")))
    list(kind = "model", models = fit$models, coding = fit$coding,
         factors = .natural_sort(input$factors), label = paste(src, "-", input$order, "fit"),
         code = paste0(data_code, "\n", coding_code, block_code, model_code),
         notes = c(chk$notes, chk_fit$notes))
  })

  responses <- shiny::reactive({
    a <- analysis()
    names(if (a$kind == "model") a$models else if (a$kind == "table") a$tables else a$coefs)
  })

  mpi_input <- function(a, which = NULL) {
    src <- if (a$kind == "model") a$models else if (a$kind == "table") a$tables else a$coefs
    if (!is.null(which)) src <- src[which]
    src
  }

  capabilities <- shiny::reactive({
    k <- tryCatch(analysis()$kind, error = function(e) NA_character_)
    if (is.na(k)) explorer_capabilities("none") else explorer_capabilities(k)
  })
  # exposed to the browser for conditionalPanel()s
  output$cap_intervals <- shiny::reactive(capabilities()$intervals)
  output$cap_bootstrap <- shiny::reactive(capabilities()$bootstrap)
  output$cap_selection <- shiny::reactive(capabilities()$selection)
  for (o in c("cap_intervals", "cap_bootstrap", "cap_selection")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }

  shiny::observeEvent(responses(), {
    r <- responses()
    for (id in c("unc_response", "rob_response", "feas_response")) {
      keep <- shiny::isolate(input[[id]])
      sel <- if (!is.null(keep) && keep %in% c(".all", r)) keep else
        if (id == "unc_response" && length(r) <= 6) ".all" else r[1]
      shiny::updateSelectInput(session, id, choices = c(if (id == "unc_response") c("All responses" = ".all"), r),
                               selected = sel)
    }
  })

  shiny::observeEvent(analysis(), {
    fs <- analysis()$factors
    keep <- shiny::isolate(input$keep)
    sel <- if (length(keep) && all(keep %in% fs)) keep else fs
    shiny::updateCheckboxGroupInput(session, "keep", choices = fs, selected = sel, inline = TRUE)
    # residual-SD scaling only makes sense with fitted models
    if (!identical(analysis()$kind, "model") && identical(shiny::isolate(input$std), "residual_sd")) {
      shiny::updateSelectInput(session, "std", selected = "none")
    }
  })

  output$source_info <- shiny::renderUI({
    a <- analysis()
    cap <- capabilities()
    avail <- function(ok, what) shiny::tags$li(if (ok) "\u2713 " else "\u2717 ", what,
                                               style = if (ok) "color:#1b5e20" else "color:#888")
    shiny::tagList(
      shiny::p(shiny::strong("Using: "), a$label, " ", shiny::span(class = "tag", a$kind),
               " - ", length(responses()), " response(s), ", length(a$factors), " factor(s)."),
      shiny::p(class = "note", "With this data you can use:"),
      shiny::tags$ul(class = "note",
        avail(TRUE, "Influence map (MPI) and follow-up / feasible region"),
        avail(cap$intervals, "Uncertainty intervals (need fitted models or standard errors)"),
        avail(cap$bootstrap, "Bootstrap rank stability and selection paths (need raw data)")),
      if (length(a$notes)) shiny::div(class = "unreliable", role = "status", shiny::strong("Data checks"),
                                      shiny::tags$ul(lapply(a$notes, shiny::tags$li)))
    )
  })
  term_maps <- shiny::reactive({
    a <- analysis()
    lapply(mpi_input(a), function(x) suppressWarnings(.term_map(x)))
  })
  output$coef_table <- shiny::renderTable({
    tms <- term_maps()
    terms <- unique(unlist(lapply(tms, function(t) names(t$coef))))
    tab <- data.frame(term = terms, check.names = FALSE)
    for (r in names(tms)) tab[[r]] <- unname(tms[[r]]$coef[terms])
    tab
  }, digits = 4, na = "")
  output$nuisance_info <- shiny::renderUI({
    nu <- unique(unlist(lapply(term_maps(), `[[`, "nuisance")))
    al <- unique(unlist(lapply(term_maps(), `[[`, "aliased")))
    shiny::tagList(
      if (length(nu)) shiny::p(class = "note", "Not used for MPI (block / categorical nuisance terms): ",
                               paste(nu, collapse = ", ")),
      if (length(al)) shiny::p(class = "note warn", "Aliased (not estimable) terms ignored: ", paste(al, collapse = ", ")))
  })
  output$coding_info <- shiny::renderUI({
    a <- analysis()
    if (is.null(a$coding)) return(NULL)
    tab <- data.frame(factor = a$coding$factor, `coded -1` = a$coding$low, `coded +1` = a$coding$high,
                      check.names = FALSE)
    shiny::tagList(shiny::h4("Coding (natural -> coded)"), shiny::renderTable(tab))
  })

  list(analysis = analysis, responses = responses, term_maps = term_maps,
       capabilities = capabilities, mpi_input = mpi_input)
}

#' Check uploaded data before fitting
#'
#' Catches the problems that would otherwise give cryptic errors or
#' misleading results: text in factor or response columns, factors with a
#' single value, missing values, and two-level factors in a second-order
#' model.
#' @param d Data frame.
#' @param responses,factors Column names.
#' @param block Block column or `""`.
#' @param order Model order (`"FO"`, `"TWI"`, `"SO"`).
#' @return `list(errors, notes)` (character vectors).
#' @keywords internal
#' @noRd
explorer_check_data <- function(d, responses, factors, block = "", order = "SO") {
  errors <- notes <- character(0)
  miss_cols <- setdiff(c(responses, factors, if (nzchar(block %||% "")) block), names(d))
  if (length(miss_cols)) return(list(errors = sprintf("Column%s not found in the file: %s.",
                                                      if (length(miss_cols) > 1) "s" else "", .join_words(miss_cols)), notes = notes))
  for (v in c(factors, responses)) {
    x <- d[[v]]
    if (!is.numeric(x)) {
      num <- suppressWarnings(as.numeric(as.character(x)))
      bad <- unique(as.character(x)[is.na(num) & !is.na(x) & nzchar(trimws(as.character(x)))])
      if (length(bad)) {
        errors <- c(errors, sprintf("%s '%s' must contain numbers, but it contains text such as %s.%s",
                                    if (v %in% factors) "Factor" else "Response", v,
                                    paste(sprintf("'%s'", utils::head(bad, 3)), collapse = ", "),
                                    if (v %in% factors) " Code categorical settings as numbers, or use the column as a block." else ""))
      }
    }
  }
  if (length(errors)) return(list(errors = errors, notes = notes))
  used <- c(responses, factors, if (nzchar(block %||% "")) block)
  complete <- stats::complete.cases(d[, used, drop = FALSE])
  if (any(!complete)) {
    with_na <- used[vapply(used, function(v) anyNA(d[[v]]), logical(1))]
    notes <- c(notes, sprintf("%d run%s with missing values (in %s) %s left out: %d of %d runs are used.",
                              sum(!complete), if (sum(!complete) > 1) "s" else "", .join_words(with_na),
                              if (sum(!complete) > 1) "are" else "is", sum(complete), nrow(d)))
  }
  dd <- d[complete, , drop = FALSE]
  for (f in factors) {
    lv <- unique(suppressWarnings(as.numeric(dd[[f]])))
    if (length(lv) < 2) {
      errors <- c(errors, sprintf("Factor '%s' takes a single value (%s) in the runs used, so its effect cannot be studied: remove it from the factors.",
                                  f, if (length(lv)) format(lv) else "none"))
    } else if (length(lv) == 2 && identical(order, "SO")) {
      notes <- c(notes, sprintf("Factor '%s' has only two levels, so its quadratic effect cannot be estimated and is left out.", f))
    }
  }
  list(errors = errors, notes = notes)
}

#' Check fitted models: enough runs, identifiable terms
#' @param models Named list of `lm` fits.
#' @param order Model order.
#' @return `list(errors, notes)`.
#' @keywords internal
#' @noRd
explorer_check_fit <- function(models, order = "SO") {
  errors <- notes <- character(0)
  for (r in names(models)) {
    m <- models[[r]]
    n <- stats::nobs(m)
    p <- length(stats::coef(m))
    df <- stats::df.residual(m)
    if (df <= 0 || n <= p) {
      errors <- c(errors, sprintf(paste0("%s: %d run%s for a model with %d terms leaves no degrees of freedom, so the model cannot be ",
                                         "estimated reliably and no uncertainty can be computed. At least %d runs are needed: add runs, ",
                                         "or choose a smaller model (first order, or with interactions only)."),
                                  r, n, if (n == 1) "" else "s", p, p + 1))
      next
    }
    al <- names(stats::coef(m))[is.na(stats::coef(m))]
    al <- al[!grepl("^I\\(.*\\^2\\)$", al)]   # two-level quadratic terms are reported by explorer_check_data()
    if (length(al)) {
      notes <- c(notes, sprintf("%s: the design cannot separate %s (aliased); %s left out of the model.", r, .join_words(al),
                                if (length(al) > 1) "they are" else "it is"))
    }
    if (df < 3) {
      notes <- c(notes, sprintf("%s: only %d residual degree%s of freedom: intervals and the bootstrap will be very uncertain.",
                                r, df, if (df == 1) "" else "s"))
    }
  }
  list(errors = unique(errors), notes = unique(notes))
}
