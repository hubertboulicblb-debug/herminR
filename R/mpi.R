#' Compute Maximal Possible Influence (MPI) from a response-surface model
#'
#' @description
#' For a second-order model in coded units,
#' \deqn{y = \beta_0 + \sum_n \beta_n x_n + \sum_n \gamma_n x_n^2 + \sum_{n<m} \delta_{nm} x_n x_m,}
#' the MPI of factor \eqn{n} is the largest absolute change in the response
#' produced by a **one-unit increase** of \eqn{x_n} inside the coded range
#' (from \eqn{a} to \eqn{a+1}, \eqn{a \in [-1, 0]}) with every other factor set
#' at its most favourable level in \eqn{[-1, 1]}:
#' \deqn{\Delta_n(a, x) = \beta_n + \gamma_n (2a + 1) + \sum_{m \ne n} \delta_{nm} x_m,}
#' \deqn{\mathrm{MPI}_n = \max_{a, x} |\Delta_n(a, x)| = |\beta_n| + |\gamma_n| + \sum_{m \ne n} |\delta_{nm}|,}
#' which is Eq. (4) of Otava and Mylona (2020) (the `"l1"` norm).
#'
#' **Sign and direction.** The signed MPI carries the sign of the change at the
#' maximiser. Because \eqn{a} can be chosen to align the quadratic term with
#' \eqn{\beta_n}, that sign is \eqn{\mathrm{sign}(\beta_n)} whenever
#' \eqn{\beta_n \ne 0}. If \eqn{\beta_n = 0} both signs are attained with the
#' same magnitude; the `direction` column is then `"either"` and the signed
#' estimate is reported as positive. The sign is the direction of the response
#' change for an *increase* of the factor at the most favourable setting; it
#' is not a statement about the direction of the response over the whole range.
#'
#' **Other norms.** `"l2"` and `"linf"` aggregate the same per-term maximal
#' contributions (\eqn{|\beta_n|, |\gamma_n|, |\delta_{nm}|}) with the
#' corresponding norm (referee W3). They are **contribution scores, not
#' attainable response changes**: only the L1 MPI is the largest change the
#' factor can produce. Their sign and `direction` are copied from the L1
#' maximiser (the direction of the largest attainable change); they are not
#' the direction of an independently maximised L2 or L-infinity quantity.
#' Printed objects and heatmaps state this whenever a non-L1 norm is shown.
#'
#' **Higher-order terms** (accepted only when `max_order > 2`) contribute their
#' per-term maximal change; the L1 value is then an upper bound and direction is
#' `NA`.
#'
#' @param model An object of class `lm` or `rsm` (coded units), a named list of
#'   such objects (one per response), a named numeric vector of coefficients,
#'   or a data frame with columns `term`, `estimate` (and optionally `se`).
#' @param norm One of `"l1"` (default, Eq. 4), `"l2"`, `"linf"`.
#' @param bounds One of `"none"`, `"se"`, `"delta"`, `"ellipsoid"`; adds
#'   `lower`/`upper` columns via [mpi_se()], [mpi_delta()] or [mpi_ellipsoid()].
#' @param level Confidence level for bounds.
#' @param by_response Kept for API compatibility; multiple models are always
#'   stacked with a `response` column.
#' @param abs If `TRUE`, return the unsigned |MPI| (screening orientation).
#' @param max_order Highest monomial order accepted (default 2).
#' @param factors Optional character vector of factors to report even when no
#'   term involves them (their MPI is 0).
#' @param ... Passed to the bound helpers.
#'
#' @return A tibble of class `herminR_mpi` with columns `factor`, `response`,
#'   `estimate`, `se` (delta-method, covariance-aware; `NA` without a
#'   covariance matrix), `lower`, `upper`, `norm`, `terms_contributed`,
#'   `direction`.
#'
#' @examples
#' # Worked example of Otava and Mylona (2020) (Table 1, response 4):
#' # MPI_34 = |0.2294| + |0.4185| + |0.3706| = 1.0185 (main, quadratic, x2:x3)
#' # MPI_24 = -(|-0.2844| + |0.3706|) = -0.655
#' mpi(c("x2" = -0.2844, "x3" = 0.2294, "x3^2" = 0.4185, "x2:x3" = 0.3706),
#'     factors = "x1")
#'
#' data(pastrydough)
#' fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2),
#'           data = pastrydough)
#' mpi(fit, bounds = "delta")
#' @references Otava, M. and Mylona, K. (2020). Communicating statistical
#'   conclusions of experiments to scientists. *Quality and Reliability
#'   Engineering International*, 36(8), 2688-2698. \doi{10.1002/qre.2697}
#' @export
mpi <- function(model,
                norm = c("l1", "l2", "linf"),
                bounds = c("none", "se", "delta", "ellipsoid"),
                level = 0.95,
                by_response = TRUE,
                abs = FALSE,
                max_order = 2L,
                factors = NULL,
                ...) {
  norm <- rlang::arg_match(norm)
  bounds <- rlang::arg_match(bounds)

  if (.is_model_list(model)) {
    return(mpi_multi(model, norm = norm, bounds = bounds, level = level,
                     abs = abs, max_order = max_order, factors = factors, ...))
  }

  tm <- .term_map(model, max_order = max_order, factors = factors)
  if (inherits(model, c("lm", "rsm"))) .warn_if_uncoded(model, tm$factors)
  if (length(tm$aliased)) {
    cli::cli_warn("Aliased (NA) coefficients ignored: {.val {tm$aliased}}.")
  }

  out <- .mpi_table(tm, norm = norm, abs = abs)
  out$response <- .response_name(model)

  if (bounds != "none" && nrow(out)) {
    b <- switch(bounds,
      se = mpi_se(tm, level = level, norm = norm, ...),
      delta = mpi_delta(tm, level = level, norm = norm, ...),
      ellipsoid = mpi_ellipsoid(tm, level = level, norm = norm, ...)
    )
    out$lower <- b$lower[match(out$factor, b$factor)]
    out$upper <- b$upper[match(out$factor, b$factor)]
  }

  out <- .new_mpi(out, model = if (inherits(model, c("lm", "rsm"))) model else NULL,
                  tm = tm, abs = abs)
  if (bounds != "none") attr(out, "bounds") <- c(type = bounds, level = level)
  out
}

#' Compute MPI for multiple response models
#'
#' Factors are aligned across responses: a factor absent from one response's
#' model is reported with MPI = 0 for that response.
#'
#' @param models Named list of `lm`/`rsm` objects (or coefficient vectors).
#' @param ... Passed to [mpi()].
#' @param factors Optional additional factors to report.
#' @param max_order Passed to [mpi()].
#' @return A stacked `herminR_mpi` tibble; the models are kept in the
#'   `"models"` attribute (used by `standardize_mpi(method = "residual_sd")`).
#' @export
mpi_multi <- function(models, ..., factors = NULL, max_order = 2L) {
  if (is.null(names(models)) || any(names(models) == "")) {
    names(models) <- paste0("Y", seq_along(models))
  }
  all_f <- unique(c(factors, unlist(lapply(models, function(m) {
    suppressWarnings(.term_map(m, max_order = max_order)$factors)
  }))))
  each <- lapply(names(models), function(nm) {
    out <- mpi(models[[nm]], factors = all_f, max_order = max_order, ...)
    out$response <- nm
    out
  })
  tms <- stats::setNames(lapply(each, attr, "terms"), names(models))
  res <- dplyr::bind_rows(lapply(each, tibble::as_tibble))
  res <- .new_mpi(res, model = NULL, tm = NULL, abs = isTRUE(list(...)$abs))
  attr(res, "models") <- models
  attr(res, "term_maps") <- tms
  attr(res, "bounds") <- attr(each[[1]], "bounds")
  res
}

#' Compare MPI under different norms
#'
#' @param model Model object as in [mpi()].
#' @param norms Character vector of norms to compute.
#' @param ... Passed to [mpi()].
#' @return A list of `herminR_mpi` objects (one per norm) with class
#'   `herminR_norms`, plus a `rank_agreement` attribute (Spearman correlation
#'   of factor rankings between norms).
#' @export
mpi_compare_norms <- function(model, norms = c("l1", "l2", "linf"), ...) {
  res <- stats::setNames(lapply(norms, function(n) mpi(model, norm = n, ...)), norms)
  est <- sapply(res, function(r) abs(r$estimate))
  if (is.matrix(est) && nrow(est) > 1) {
    attr(res, "rank_agreement") <- suppressWarnings(stats::cor(est, method = "spearman"))
  }
  class(res) <- c("herminR_norms", "list")
  res
}

# Internals ------------------------------------------------------------------

.is_model_list <- function(x) {
  is.list(x) && !is.data.frame(x) && !inherits(x, c("lm", "rsm", "herminR_terms")) &&
    length(x) > 0 && all(vapply(x, function(m) inherits(m, c("lm", "rsm")), logical(1)))
}

.response_name <- function(model) {
  if (inherits(model, c("lm", "rsm"))) {
    f <- stats::formula(model)
    if (length(f) == 3L) return(paste(deparse(f[[2]]), collapse = ""))
  }
  "response"
}

.new_mpi <- function(out, model, tm, abs) {
  out <- tibble::as_tibble(out)
  cols <- c("factor", "response", "estimate", "se", "lower", "upper", "norm",
            "terms_contributed", "direction")
  out <- out[, c(intersect(cols, names(out)), setdiff(names(out), cols))]
  class(out) <- c("herminR_mpi", "tbl_df", "tbl", "data.frame")
  attr(out, "model") <- model
  attr(out, "terms") <- tm
  attr(out, "abs") <- abs
  out
}

.full_ranges <- function(factors, ranges = list()) {
  r <- stats::setNames(rep(list(c(-1, 1)), length(factors)), factors)
  for (k in intersect(names(ranges), factors)) {
    v <- sort(as.numeric(ranges[[k]]))
    if (length(v) != 2L || any(!is.finite(v))) {
      cli::cli_abort("Range for {.val {k}} must be two finite numbers.")
    }
    r[[k]] <- v
  }
  unknown <- setdiff(names(ranges), factors)
  if (length(unknown)) cli::cli_abort("Unknown factor(s) in ranges: {.val {unknown}}.")
  r
}

#' Per-factor ingredients of MPI
#'
#' @param own `"unit"`: one-unit increase within [-1, 1] regardless of the
#'   factor's own range (the definition of Otava and Mylona, 2020, used by mpi_adjust_range);
#'   `"attainable"`: increase by min(1, width) inside the factor's own range
#'   (used by mpi_feasible).
#' @return list(terms, coef, D (per-term contribution scale, so contribution =
#'   D * |coef|), max_change, min_change, second_order)
#' @noRd
.factor_parts <- function(tm, f, ranges, own = c("unit", "attainable")) {
  own <- match.arg(own)
  terms <- names(tm$powers)[vapply(tm$powers, function(p) f %in% names(p), logical(1))]
  if (!length(terms)) {
    return(list(terms = character(0), coef = numeric(0), D = numeric(0),
                max_change = 0, min_change = 0, second_order = TRUE))
  }
  rng <- ranges[[f]]
  if (own == "unit") {
    d <- 1
    a <- c(-1, 0)
  } else {
    d <- min(1, diff(rng))
    a <- c(rng[1], rng[2] - d)
  }
  cf <- tm$coef[terms]
  D <- stats::setNames(numeric(length(terms)), terms)
  second <- TRUE
  beta <- 0
  gam <- 0
  inter_hi <- 0
  inter_lo <- 0
  for (t in terms) {
    p <- tm$powers[[t]]
    pn <- p[[f]]
    oth <- p[names(p) != f]
    own_fac <- if (pn == 1L) d else {
      aa <- seq(a[1], a[2], length.out = 401)
      max(abs((aa + d)^pn - aa^pn))
    }
    oth_fac <- if (length(oth)) {
      prod(vapply(names(oth), function(m) max(abs(ranges[[m]]))^oth[[m]], numeric(1)))
    } else 1
    D[t] <- own_fac * oth_fac
    b <- cf[[t]]
    if (pn == 1L && !length(oth)) {
      beta <- beta + b
    } else if (pn == 2L && !length(oth)) {
      gam <- gam + b
    } else if (pn == 1L && length(oth) == 1L && oth[[1]] == 1L) {
      r <- ranges[[names(oth)]]
      inter_hi <- inter_hi + max(b * r)
      inter_lo <- inter_lo + min(b * r)
    } else {
      second <- FALSE
    }
  }
  if (second) {
    tq <- 2 * a + d
    max_change <- d * (beta + max(gam * tq) + inter_hi)
    min_change <- d * (beta + min(gam * tq) + inter_lo)
  } else {
    s <- sum(D * abs(cf))
    max_change <- s
    min_change <- -s
  }
  list(terms = terms, coef = cf, D = D, max_change = max_change,
       min_change = min_change, second_order = second)
}

.norm_value <- function(v, norm) {
  if (!length(v)) return(0)
  switch(norm,
    l1 = sum(abs(v)),
    l2 = sqrt(sum(v^2)),
    linf = max(abs(v))
  )
}

.direction <- function(parts) {
  if (!length(parts$terms)) return("none")
  if (!parts$second_order) return(NA_character_)
  hi <- parts$max_change
  lo <- -parts$min_change
  tol <- 1e-10 * max(1, abs(hi), abs(lo))
  if (abs(hi - lo) <= tol) "either" else if (hi > lo) "increase" else "decrease"
}

# Magnitude for a factor given its parts
.magnitude <- function(parts, norm) {
  if (!length(parts$terms)) return(0)
  if (norm == "l1" && parts$second_order) {
    return(max(parts$max_change, -parts$min_change, 0))
  }
  .norm_value(parts$D * abs(parts$coef), norm)
}

# Delta-method SE of the magnitude (covariance-aware)
.delta_se <- function(parts, norm, V) {
  if (is.null(V) || !length(parts$terms)) return(NA_real_)
  if (!all(parts$terms %in% rownames(V))) return(NA_real_)
  c_s <- parts$D * parts$coef
  g <- switch(norm,
    l1 = parts$D * sign(parts$coef),
    l2 = {
      nv <- sqrt(sum(c_s^2))
      if (nv > 0) parts$D * c_s / nv else rep(0, length(c_s))
    },
    linf = {
      j <- which.max(abs(c_s))
      gg <- rep(0, length(c_s))
      gg[j] <- parts$D[j] * sign(parts$coef[j])
      gg
    }
  )
  Vf <- V[parts$terms, parts$terms, drop = FALSE]
  sqrt(max(0, as.numeric(t(g) %*% Vf %*% g)))
}

#' Build an MPI table from a term map
#' @noRd
.mpi_table <- function(tm, norm = "l1", abs = FALSE, ranges = list(), own = "unit") {
  fs <- tm$factors
  rr <- .full_ranges(fs, ranges)
  if (!length(fs)) {
    return(tibble::tibble(
      factor = character(0), response = character(0), estimate = numeric(0),
      se = numeric(0), lower = numeric(0), upper = numeric(0),
      norm = character(0), terms_contributed = integer(0), direction = character(0)
    ))
  }
  # one pass over the factors, one table at the end (building a small table per
  # factor dominated the run time of the bootstrap)
  n <- length(fs)
  est <- se <- numeric(n)
  dirn <- character(n)
  nterms <- integer(n)
  for (i in seq_len(n)) {
    p <- .factor_parts(tm, fs[i], rr, own = own)
    mag <- .magnitude(p, norm)
    dirn[i] <- .direction(p)
    est[i] <- if (!abs && identical(dirn[i], "decrease")) -mag else mag
    se[i] <- .delta_se(p, norm, tm$vcov)
    nterms[i] <- length(p$terms)
  }
  tibble::new_tibble(list(
    factor = fs, response = rep("response", n), estimate = est, se = se,
    lower = rep(NA_real_, n), upper = rep(NA_real_, n), norm = rep(norm, n),
    terms_contributed = nterms, direction = dirn
  ), nrow = n)
}


#' @export
print.herminR_mpi <- function(x, ...) {
  cat("<herminR_mpi> ", nrow(x), " factor-response pairs",
      if ("norm" %in% names(x)) paste0(", norm = ", paste(unique(x$norm), collapse = ", ")),
      if (isTRUE(attr(x, "abs"))) " (absolute)" else " (signed)", "\n", sep = "")
  note <- if ("norm" %in% names(x)) .norm_note(x$norm) else NULL
  if (!is.null(note)) cat("Note: ", note, "\n", sep = "")
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' @export
summary.herminR_mpi <- function(object, ...) {
  cat("Maximal Possible Influence summary\n")
  cat("Norm(s):", paste(unique(object$norm), collapse = ", "), "\n")
  cat("Factors:", paste(unique(object$factor), collapse = ", "), "\n")
  cat("Responses:", paste(unique(object$response), collapse = ", "), "\n")
  tm <- attr(object, "terms")
  if (!is.null(tm)) {
    if (length(tm$nuisance)) cat("Nuisance terms excluded:", paste(tm$nuisance, collapse = ", "), "\n")
    if (length(tm$aliased)) cat("Aliased terms ignored:", paste(tm$aliased, collapse = ", "), "\n")
  }
  print(tibble::as_tibble(object))
  invisible(object)
}

# Explanatory note for non-L1 norms (NULL for L1)
.norm_note <- function(norms) {
  n <- setdiff(unique(norms), c("l1", NA))
  if (!length(n)) return(NULL)
  lab <- c(l2 = "L2", linf = "L-infinity")[n]
  paste0(paste(lab, collapse = "/"), " values are contribution scores, not attainable ",
         "response changes; sign/direction is taken from the L1 maximiser.")
}
