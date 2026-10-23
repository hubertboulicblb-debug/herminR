# Term mapping ---------------------------------------------------------------
#
# Every coefficient of a fitted model is mapped to a *monomial*: a named
# integer vector of exponents over the design factors, e.g.
#   x1          -> c(x1 = 1)
#   I(x1^2)     -> c(x1 = 2)
#   x1:x2       -> c(x1 = 1, x2 = 1)
#   PQ(...)x3^2 -> c(x3 = 2)
# Coefficients belonging to non-numeric variables (blocks, categorical
# nuisance terms) are set aside as `nuisance`. Anything that cannot be mapped
# exactly is a hard error: an MPI built on a mis-parsed term is wrong silently.

#' Map model coefficients to factor monomials
#'
#' @param x An `lm`/`rsm` object, a named numeric coefficient vector, or a data
#'   frame with columns `term`, `estimate` and optionally `se`.
#' @param max_order Highest monomial order accepted (default 2). Terms of
#'   order 3 up to `max_order` are accepted with a warning; higher orders are
#'   an error.
#' @param factors Optional character vector of factor names to report even if
#'   they appear in no term (they receive MPI = 0).
#' @return An object of class `herminR_terms` (a list) with elements `coef`,
#'   `powers`, `factors`, `nuisance`, `aliased`, `vcov`, `df`, `sigma`,
#'   `source`.
#' @keywords internal
#' @noRd
.term_map <- function(x, max_order = 2L, factors = NULL) {
  if (inherits(x, "herminR_terms")) return(x)
  if (inherits(x, c("lm", "rsm"))) {
    tm <- .term_map_model(x)
  } else if (is.data.frame(x)) {
    if (!all(c("term", "estimate") %in% names(x))) {
      cli::cli_abort("Data-frame input must contain columns {.field term} and {.field estimate}.")
    }
    b <- stats::setNames(as.numeric(x$estimate), as.character(x$term))
    tm <- .term_map_vector(b)
    if ("se" %in% names(x)) {
      se <- stats::setNames(as.numeric(x$se), as.character(x$term))[names(tm$coef)]
      if (all(is.finite(se))) {
        tm$vcov <- diag(se^2, nrow = length(se))
        dimnames(tm$vcov) <- list(names(se), names(se))
        tm$vcov_diagonal <- TRUE
      }
    }
    tm$source <- "table"
  } else if (is.numeric(x) && !is.null(names(x))) {
    tm <- .term_map_vector(x)
  } else {
    cli::cli_abort(c(
      "{.arg model} must be an {.cls lm}/{.cls rsm} object, a named numeric vector, or a coefficient data frame.",
      "x" = "Got an object of class {.cls {class(x)}}."
    ))
  }

  ord <- vapply(tm$powers, sum, numeric(1))
  too_high <- names(ord)[ord > max_order]
  if (length(too_high)) {
    cli::cli_abort(c(
      "Terms of order > {.arg max_order} = {max_order}: {.val {too_high}}.",
      "i" = "Raise {.arg max_order} to accept them (MPI aggregation is then an upper bound)."
    ))
  }
  if (any(ord > 2)) {
    cli::cli_warn(c(
      "Terms of order > 2 found: {.val {names(ord)[ord > 2]}}.",
      "i" = "Eq. (4) is defined for second-order models; for these factors MPI is a per-term upper bound and direction is undetermined."
    ))
  }

  tm$factors <- .natural_sort(unique(c(tm$factors, factors)))
  class(tm) <- "herminR_terms"
  tm
}

# Coefficient vector input: every symbol is a design factor.
.term_map_vector <- function(b) {
  b <- b[names(b) != "(Intercept)"]
  aliased <- names(b)[is.na(b)]
  b <- b[!is.na(b)]
  powers <- lapply(names(b), .parse_coef_name)
  names(powers) <- names(b)
  list(
    coef = b, powers = powers,
    factors = unique(unlist(lapply(powers, names))),
    nuisance = character(0), aliased = aliased,
    vcov = NULL, vcov_diagonal = FALSE, df = Inf, sigma = NA_real_,
    source = "vector"
  )
}

# Fitted model input: use the model's own term metadata to separate design
# terms from nuisance (block / categorical) terms, then parse names exactly.
.term_map_model <- function(model) {
  b <- stats::coef(model)
  tt <- stats::terms(model)
  mm_assign <- attr(stats::model.matrix(model), "assign")
  term_labels <- attr(tt, "term.labels")
  fac_mat <- attr(tt, "factors")
  data_classes <- attr(tt, "dataClasses")
  resp <- if (attr(tt, "response") > 0) rownames(fac_mat)[1] else NULL
  if (is.null(data_classes)) {
    mf <- stats::model.frame(model)
    data_classes <- vapply(mf, function(v) class(v)[1], character(1))
  }
  is_num_var <- function(v) {
    cl <- data_classes[v]
    !is.na(cl) && (cl == "numeric" || grepl("^nmatrix", cl))
  }

  nuisance <- character(0)
  keep <- logical(length(b))
  for (j in seq_along(b)) {
    a <- mm_assign[j]
    if (a == 0) next # intercept
    vars <- rownames(fac_mat)[fac_mat[, a] > 0]
    if (all(vapply(vars, is_num_var, logical(1)))) {
      keep[j] <- TRUE
    } else {
      nuisance <- c(nuisance, names(b)[j])
    }
  }
  bd <- b[keep]
  aliased <- names(bd)[is.na(bd)]
  bd <- bd[!is.na(bd)]

  powers <- lapply(names(bd), .parse_coef_name)
  names(powers) <- names(bd)

  # Factors = numeric variables named in the formula (even if every one of
  # their coefficients was aliased away).
  num_vars <- setdiff(rownames(fac_mat), resp)
  num_vars <- num_vars[vapply(num_vars, is_num_var, logical(1))]
  formula_factors <- unique(unlist(lapply(num_vars, function(v) all.vars(str2lang(v)))))

  V <- tryCatch(stats::vcov(model), error = function(e) NULL)
  if (!is.null(V)) {
    ok <- intersect(names(bd), rownames(V))
    V <- V[ok, ok, drop = FALSE]
    if (any(!is.finite(V))) V <- NULL
  }
  df <- tryCatch(stats::df.residual(model), error = function(e) Inf)
  if (is.null(df) || is.na(df)) df <- Inf
  sig <- tryCatch(stats::sigma(model), error = function(e) NA_real_)

  list(
    coef = bd, powers = powers,
    factors = unique(c(formula_factors, unlist(lapply(powers, names)))),
    nuisance = nuisance, aliased = aliased,
    vcov = if (!is.null(V) && df > 0) V else NULL,
    vcov_diagonal = FALSE,
    df = df, sigma = if (df > 0) sig else NA_real_,
    source = if (inherits(model, "rsm")) "rsm" else "lm"
  )
}

#' Parse one coefficient name into a monomial (named exponent vector)
#' @noRd
.parse_coef_name <- function(nm) {
  # rsm wrappers: "FO(x1, x2)x1", "TWI(x1, x2)x1:x2", "PQ(x1, x2)x1^2"
  core <- sub("^(FO|TWI|PQ|SO)\\((.*?)\\)(?=[A-Za-z.`])", "", nm, perl = TRUE)
  comps <- .split_top_level(core, ":")
  pw <- integer(0)
  for (cp in comps) pw <- .add_powers(pw, .parse_component(cp, nm))
  pw <- pw[pw != 0L]
  if (!length(pw)) {
    cli::cli_abort("Could not map coefficient {.val {nm}} to any factor.")
  }
  pw[order(names(pw))]
}

.split_top_level <- function(s, sep) {
  chars <- strsplit(s, "")[[1]]
  depth <- 0L
  out <- character(0)
  cur <- ""
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") depth <- depth - 1L
    if (ch == sep && depth == 0L) {
      out <- c(out, cur)
      cur <- ""
    } else {
      cur <- paste0(cur, ch)
    }
  }
  c(out, cur)
}

.add_powers <- function(a, b) {
  for (k in names(b)) a[k] <- (if (is.na(a[k])) 0L else a[k]) + b[[k]]
  storage.mode(a) <- "integer"
  a
}

.parse_component <- function(cp, full_name) {
  cp <- trimws(cp)
  # poly(x, d, raw = TRUE)k  or  poly(x1, x2, degree = 2, raw = TRUE)k.l
  if (grepl("^poly\\(.*\\)[0-9.]+$", cp)) {
    call_txt <- sub("^(poly\\(.*\\))[0-9.]+$", "\\1", cp)
    suffix <- sub("^poly\\(.*\\)([0-9.]+)$", "\\1", cp)
    cl <- str2lang(call_txt)
    mc <- match.call(stats::poly, cl)
    raw <- if (is.null(mc$raw)) FALSE else isTRUE(eval(mc$raw, baseenv()))
    if (!raw) {
      cli::cli_abort(c(
        "Orthogonal {.fn poly} term {.val {full_name}} cannot be mapped to coded-unit effects.",
        "i" = "Refit with {.code poly(..., raw = TRUE)}, {.code I(x^2)}, or {.pkg rsm}'s {.code SO()}."
      ))
    }
    # poly(x, ...): positional args after x land in `...`; numeric ones are
    # the degree, symbols are further variables.
    args <- as.list(mc)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    pos <- args[nms %in% c("x", "")]
    var_names <- vapply(pos[vapply(pos, is.symbol, logical(1))], deparse, character(1))
    degs <- as.integer(strsplit(suffix, ".", fixed = TRUE)[[1]])
    if (length(degs) != length(var_names)) {
      cli::cli_abort("Cannot match {.fn poly} column suffix in {.val {full_name}}.")
    }
    return(stats::setNames(degs, var_names))
  }
  expr <- tryCatch(str2lang(cp), error = function(e) NULL)
  if (is.null(expr)) {
    cli::cli_abort("Unparseable term {.val {full_name}}.")
  }
  .monomial(expr, full_name)
}

.monomial <- function(e, full_name) {
  if (is.symbol(e)) return(stats::setNames(1L, as.character(e)))
  if (is.call(e)) {
    fn <- as.character(e[[1]])
    if (fn %in% c("I", "(") && length(e) == 2L) return(.monomial(e[[2]], full_name))
    if (fn == "^" && length(e) == 3L && is.numeric(e[[3]])) {
      k <- e[[3]]
      if (k >= 1 && k == round(k)) {
        m <- .monomial(e[[2]], full_name)
        return(m * as.integer(k))
      }
    }
    if (fn == "*" && length(e) == 3L) {
      return(.add_powers(.monomial(e[[2]], full_name), .monomial(e[[3]], full_name)))
    }
  }
  cli::cli_abort(c(
    "Term {.val {full_name}} is not a monomial in the design factors.",
    "i" = "Supported: {.code x}, {.code x^k}, {.code I(x^k)}, {.code I(x*y)}, {.code x:y}, raw {.fn poly}, and {.pkg rsm} {.code FO/TWI/PQ/SO}."
  ))
}

# Coded units ----------------------------------------------------------------

#' Raw factor values used in a fit (rows aligned with the model frame)
#' @noRd
.factor_data <- function(model, factors) {
  if (!length(factors)) return(NULL)
  .raw_data(model, factors)
}

#' Raw variables used in a fit, rows aligned with the model frame
#'
#' Tries the model's own stored data (rsm keeps it in `$data`), then
#' `expand.model.frame()` (evaluates the `data` argument in the formula's
#' environment).
#' @noRd
.raw_data <- function(model, vars) {
  rows <- rownames(stats::model.frame(model))
  if (is.data.frame(model$data) && all(vars %in% names(model$data)) &&
      all(rows %in% rownames(model$data))) {
    return(model$data[rows, vars, drop = FALSE])
  }
  mf <- stats::model.frame(model)
  if (all(vars %in% names(mf))) {
    # every raw variable is itself a model-frame column (e.g. x1 + I(x1^2))
    return(mf[, vars, drop = FALSE])
  }
  ef <- tryCatch(
    stats::expand.model.frame(model, stats::reformulate(vars), na.expand = FALSE),
    error = function(e) NULL
  )
  if (is.null(ef)) return(NULL)
  out <- as.data.frame(lapply(stats::setNames(vars, vars), function(v) {
    j <- which(names(ef) == v)
    if (length(j)) ef[[j[length(j)]]] else ef[[paste0(v, ".1")]]
  }), check.names = FALSE)
  rownames(out) <- rownames(ef)
  out
}

#' Check whether a model's factors look coded (approximately within [-2, 2])
#' @noRd
.is_coded <- function(model, factors) {
  if (inherits(model, "rsm") && !is.null(rsm::codings(model))) return(TRUE)
  d <- .factor_data(model, factors)
  if (is.null(d)) return(NA)
  all(vapply(d, function(v) is.numeric(v) && max(abs(v), na.rm = TRUE) <= 2 + 1e-8, logical(1)))
}

.warn_if_uncoded <- function(model, factors) {
  coded <- .is_coded(model, factors)
  if (isFALSE(coded)) {
    cli::cli_warn(c(
      "Factors do not appear to be in coded units (some |x| > 2).",
      "!" = "MPI is a one-unit change: on natural units it is not comparable across factors.",
      "i" = "Code the data first, e.g. with {.fn rsm::coded.data}."
    ))
  }
  invisible(coded)
}

#' Soft-threshold a coefficient by k * SE
#' @noRd
.soft_threshold <- function(b, se, k) {
  sign(b) * pmax(0, abs(b) - k * se)
}

# Natural order: x2 before x10, Factor_2 before Factor_10
.natural_sort <- function(x) {
  if (!length(x)) return(x)
  stem <- sub("[0-9]+$", "", x)
  num <- suppressWarnings(as.numeric(sub("^.*?([0-9]+)$", "\\1", x)))
  num[!grepl("[0-9]$", x)] <- -1
  x[order(stem, num, x)]
}
