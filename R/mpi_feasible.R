#' Feasible-region restricted MPI
#'
#' The attainable influence of each factor when the factors are restricted to
#' a feasible region (referee W2). For factor \eqn{n}, the attainable change is
#' the largest \eqn{|y(x + d\,e_n) - y(x)|} over pairs of points that are both
#' feasible, where \eqn{d = \min(1, u_n - l_n)} is a one-unit step (or the whole
#' feasible width of \eqn{x_n} if that is shorter than one unit).
#'
#' * `method = "exact"` (default) solves this exactly for a rectangular
#'   `region` and a second-order model: the change is linear in the other
#'   factors and in the starting point's quadratic term, so the extremes are at
#'   box vertices.
#' * `method = "lhs"` or `"grid"` samples pairs of points, evaluates the fitted
#'   polynomial directly and keeps pairs where both points satisfy `region`
#'   *and* the optional `constraint` (any shape, e.g. \eqn{x_1 + x_2 \le 0}).
#'   The sampled maximum is a lower estimate of the true supremum; increase
#'   `size` to tighten it.
#'
#' Feasible influence is defined only as the attainable response change, i.e.
#' the L1 MPI. L2 and L-infinity are aggregation scores of per-term
#' contributions, not attainable changes, so they are not offered here.
#'
#' @param model An `lm`/`rsm` object or a coefficient vector/table.
#' @param region Named list of `c(lower, upper)` coded ranges for restricted
#'   factors. Unlisted factors stay on \eqn{[-1, 1]}.
#' @param constraint Optional function taking a data frame of factor settings
#'   (one column per factor) and returning a logical vector with exactly one
#'   non-missing value per row (`TRUE` = feasible); anything else is an error.
#'   Requires a sampling method.
#' @param method `"exact"`, `"lhs"` or `"grid"` (grid needs \eqn{\le 4}
#'   factors).
#' @param size Number of sampled pairs per factor (sampling methods).
#' @param seed Random seed (sampling methods).
#' @param norm Must be `"l1"` (kept for API compatibility).
#' @param factors Optional factor names to include even when no term involves
#'   them (they then appear with MPI 0 and can be used in `region` and
#'   `constraint`).
#' @param ... Unused.
#'
#' @return A `herminR_mpi` tibble with unsigned `estimate` (restricted MPI)
#'   plus `mpi_unrestricted`, `feas_ratio`, and for sampling methods
#'   `n_feasible` (feasible pairs found for that factor).
#' @examples
#' b <- c(x1 = 1, x2 = 0.5, "x1:x2" = 0.3, "x1^2" = -0.2)
#' mpi_feasible(b, region = list(x2 = c(-0.5, 0.5)))
#' mpi_feasible(b, constraint = function(d) d$x1 + d$x2 <= 0,
#'              method = "lhs", size = 5000, seed = 1)
#' @export
mpi_feasible <- function(model,
                         region = list(),
                         constraint = NULL,
                         method = c("exact", "lhs", "grid"),
                         size = 10000L,
                         seed = NULL,
                         norm = c("l1", "l2", "linf"),
                         factors = NULL,
                         ...) {
  method <- rlang::arg_match(method)
  norm <- rlang::arg_match(norm)
  tm <- .term_map(model, factors = factors)
  fs <- tm$factors
  rr <- .full_ranges(fs, region)
  if (!is.null(constraint) && method == "exact") {
    cli::cli_abort("A {.arg constraint} function needs {.code method = \"lhs\"} or {.code \"grid\"}.")
  }
  if (norm != "l1") {
    cli::cli_abort(c(
      "Feasible MPI is the attainable response change, which is the L1 MPI.",
      "i" = "L2 / L-infinity are contribution scores, not attainable changes; use {.code norm = \"l1\"}."
    ))
  }

  base <- .mpi_table(tm, norm = norm, abs = TRUE)

  if (method == "exact") {
    restr <- .mpi_table(tm, norm = norm, abs = TRUE, ranges = region, own = "attainable")
    est <- restr$estimate
    dirn <- restr$direction
    n_feas <- NA_integer_
  } else {
    run <- function() lapply(fs, function(f) .feasible_sample(tm, f, rr, constraint, method, size))
    res <- if (is.null(seed)) run() else withr::with_seed(seed, run())
    est <- vapply(res, `[[`, numeric(1), "value")
    dirn <- vapply(res, `[[`, character(1), "direction")
    n_feas <- vapply(res, `[[`, integer(1), "n")
  }

  out <- base
  out$response <- .response_name(model)
  out$mpi_unrestricted <- base$estimate
  out$estimate <- est
  out$direction <- dirn
  out$feas_ratio <- ifelse(out$mpi_unrestricted > 0, out$estimate / out$mpi_unrestricted, NA_real_)
  if (method != "exact") out$n_feasible <- n_feas
  out$se <- NA_real_
  out <- .new_mpi(out, model = if (inherits(model, c("lm", "rsm"))) model else NULL, tm = tm, abs = TRUE)
  attr(out, "region") <- region
  attr(out, "method") <- method
  out
}

.poly_eval <- function(tm, X) {
  v <- numeric(nrow(X))
  for (t in names(tm$coef)) {
    p <- tm$powers[[t]]
    m <- rep(1, nrow(X))
    for (k in names(p)) m <- m * X[[k]]^p[[k]]
    v <- v + tm$coef[[t]] * m
  }
  v
}

.unit_design <- function(n, p, method) {
  if (method == "lhs") {
    vapply(seq_len(p), function(j) (sample.int(n) - stats::runif(n)) / n, numeric(n))
  } else {
    k <- max(2L, floor(n^(1 / p)))
    as.matrix(expand.grid(rep(list(seq(0, 1, length.out = k)), p)))
  }
}

.feasible_sample <- function(tm, f, rr, constraint, method, size) {
  fs <- names(rr)
  if (method == "grid" && length(fs) > 4) {
    cli::cli_abort("{.code method = \"grid\"} supports at most 4 factors; use {.code \"lhs\"}.")
  }
  rng <- rr[[f]]
  d <- min(1, diff(rng))
  U <- .unit_design(as.integer(size), length(fs), method)
  colnames(U) <- fs
  X0 <- as.data.frame(U)
  for (k in fs) {
    lo <- rr[[k]][1]
    hi <- if (k == f) rr[[k]][2] - d else rr[[k]][2]
    X0[[k]] <- lo + U[, k] * (hi - lo)
  }
  X1 <- X0
  X1[[f]] <- X0[[f]] + d
  ok <- rep(TRUE, nrow(X0))
  if (!is.null(constraint)) {
    ok <- .check_constraint(constraint(X0), nrow(X0)) & .check_constraint(constraint(X1), nrow(X1))
  }
  if (!any(ok)) return(list(value = NA_real_, direction = NA_character_, n = 0L))
  delta <- .poly_eval(tm, X1[ok, , drop = FALSE]) - .poly_eval(tm, X0[ok, , drop = FALSE])
  hi <- max(delta)
  lo <- -min(delta)
  list(value = max(hi, lo, 0),
       direction = if (hi >= lo) "increase" else "decrease",
       n = sum(ok))
}

.check_constraint <- function(v, n) {
  if (!is.logical(v) || length(v) != n) {
    cli::cli_abort(c(
      "{.arg constraint} must return one logical value per row of its input.",
      "x" = "Got {.cls {class(v)}} of length {length(v)} for {n} rows."
    ))
  }
  if (anyNA(v)) cli::cli_abort("{.arg constraint} returned NA for {sum(is.na(v))} row{?s}.")
  v
}
