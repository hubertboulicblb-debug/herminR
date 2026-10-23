# Shared preamble for bound functions: accept a model, an MPI object, or a
# term map; return the term map and checked covariance.
.bounds_input <- function(model, what) {
  if (inherits(model, "herminR_mpi")) {
    tm <- attr(model, "terms")
    if (is.null(tm)) cli::cli_abort("This MPI object has no attached term map; call {.fn {what}} on the model.")
    return(tm)
  }
  if (inherits(model, "herminR_terms")) return(model)
  .term_map(model)
}

.na_bounds <- function(tm, msg) {
  cli::cli_warn(msg)
  tibble::tibble(factor = tm$factors, estimate = NA_real_, lower = NA_real_,
                 upper = NA_real_, se = NA_real_)
}

#' Soft-thresholded standard-error bounds for MPI
#'
#' Each term's maximal contribution \eqn{|b|} is replaced by
#' \eqn{\max(0, |b| - k\,SE)} (lower) and \eqn{|b| + k\,SE} (upper) before
#' aggregation with the chosen norm.
#'
#' These are *per-coefficient* bounds: each coefficient interval has the
#' nominal level marginally, but the aggregated bound has no exact joint
#' coverage. Use `k_adj = TRUE` (Bonferroni over the factor's terms) for
#' conservative joint coverage within a factor, or [mpi_ellipsoid()] for exact
#' simultaneous bounds.
#'
#' @param model An `lm`/`rsm` object or an MPI object computed from one.
#' @param k SE multiplier. Default: \eqn{t_{1-\alpha/2,\,df}}, or with
#'   `k_adj = TRUE`, \eqn{t_{1-\alpha/(2g),\,df}} where \eqn{g} is the number of
#'   terms in the factor's group (Bonferroni).
#' @param level Confidence level.
#' @param k_adj Logical; Bonferroni adjustment of `k` per factor group.
#' @param pseudo_se Lenth pseudo-SEs for saturated designs. Not implemented
#'   yet: `TRUE` is an error.
#' @param norm Norm used for aggregation.
#' @param ... Unused.
#' @return A tibble with columns `factor`, `estimate`, `lower`, `upper`, `se`.
#' @export
mpi_se <- function(model, k = NULL, level = 0.95, k_adj = FALSE,
                   pseudo_se = FALSE, norm = c("l1", "l2", "linf"), ...) {
  norm <- rlang::arg_match(norm)
  if (isTRUE(pseudo_se)) {
    cli::cli_abort("Lenth pseudo-SEs ({.code pseudo_se = TRUE}) are not implemented yet.")
  }
  tm <- .bounds_input(model, "mpi_se")
  V <- tm$vcov
  if (is.null(V)) {
    return(.na_bounds(tm, c(
      "Standard errors unavailable (saturated or unreplicated design?); returning NA bounds.",
      "i" = "Lenth-style pseudo-SEs are planned."
    )))
  }
  rr <- .full_ranges(tm$factors)
  alpha <- 1 - level
  rows <- lapply(tm$factors, function(f) {
    p <- .factor_parts(tm, f, rr)
    if (!length(p$terms)) {
      return(tibble::tibble(factor = f, estimate = 0, lower = 0, upper = 0, se = 0))
    }
    se_b <- sqrt(diag(V)[p$terms])
    kk <- if (!is.null(k)) k else {
      g <- if (k_adj) length(p$terms) else 1
      stats::qt(1 - alpha / (2 * g), df = tm$df)
    }
    lo <- p$D * pmax(0, abs(p$coef) - kk * se_b)
    hi <- p$D * (abs(p$coef) + kk * se_b)
    tibble::tibble(
      factor = f,
      estimate = .magnitude(p, norm),
      lower = .norm_value(lo, norm),
      upper = .norm_value(hi, norm),
      se = .delta_se(p, norm, V)
    )
  })
  dplyr::bind_rows(rows)
}

#' Delta-method bounds for MPI
#'
#' First-order (delta-method) standard error of the factor's norm, using the
#' full covariance matrix of its coefficient group, with a t interval (residual
#' degrees of freedom; normal when unavailable) truncated at zero.
#'
#' The norm is not differentiable where a coefficient is zero (and, for
#' `"linf"`, where two contributions tie), so the interval is unreliable when a
#' coefficient is within about two standard errors of zero. Such factors are
#' flagged in the `near_kink` column.
#'
#' @inheritParams mpi_se
#' @return Tibble with `factor`, `estimate`, `lower`, `upper`, `se`,
#'   `near_kink`.
#' @export
mpi_delta <- function(model, level = 0.95, norm = c("l1", "l2", "linf"), ...) {
  norm <- rlang::arg_match(norm)
  tm <- .bounds_input(model, "mpi_delta")
  V <- tm$vcov
  if (is.null(V)) return(.na_bounds(tm, "vcov() unavailable; returning NA bounds."))
  # t quantile: the SE uses the estimated residual variance
  z <- if (is.finite(tm$df)) stats::qt(1 - (1 - level) / 2, tm$df) else stats::qnorm(1 - (1 - level) / 2)
  rr <- .full_ranges(tm$factors)
  rows <- lapply(tm$factors, function(f) {
    p <- .factor_parts(tm, f, rr)
    est <- .magnitude(p, norm)
    se <- if (length(p$terms)) .delta_se(p, norm, V) else 0
    near <- length(p$terms) > 0 && any(abs(p$coef) < 2 * sqrt(diag(V)[p$terms]))
    tibble::tibble(factor = f, estimate = est, lower = max(0, est - z * se),
                   upper = est + z * se, se = se, near_kink = near)
  })
  dplyr::bind_rows(rows)
}

#' Ellipsoid (simultaneous) bounds for MPI
#'
#' For each factor, the minimum and maximum of its norm over the joint
#' confidence ellipsoid of its coefficient group
#' \eqn{\{b : (b - \hat b)^\top V^{-1} (b - \hat b) \le c\}}.
#'
#' The extrema are computed from exact formulations, solved as follows:
#' \itemize{
#'   \item L1 maximum: enumeration of all \eqn{2^g} sign vectors (closed form
#'     per vector; exact up to floating point).
#'   \item L-infinity maximum: closed form.
#'   \item L2 maximum and minimum: trust-region subproblem; the secular
#'     equation is solved by root finding (`uniroot`, tolerance 1e-14),
#'     including the "hard case".
#'   \item L1 minimum: its convex dual over the box \eqn{[-1, 1]^g}, solved by
#'     L-BFGS-B.
#'   \item L-infinity minimum: bisection (60 halvings) on box-ellipsoid
#'     intersection, each step a box-constrained quadratic programme (L-BFGS-B),
#'     then a dual certificate.
#'   \item For the L1 and L-infinity minima the value returned is the dual
#'     objective, which is a valid lower bound whatever the solver's accuracy
#'     (weak duality), so the lower confidence bound stays conservative. The
#'     duality gap to the best primal point is checked and a warning is issued
#'     if it exceeds \eqn{10^{-6}} (relative).
#'   \item All minima are 0 when the ellipsoid contains the origin.
#' }
#' The numerical solvers are checked in the test suite against an independent
#' multistart optimiser, including near-singular covariances and groups of up
#' to 8 terms. A singular covariance block gives `NA` bounds with a warning.
#'
#' @inheritParams mpi_se
#' @param gap_tol Relative duality-gap tolerance above which the L1 and
#'   L-infinity minima emit a warning (default \eqn{10^{-6}}). It only controls
#'   the warning; the returned lower bound is always the certified dual value.
#' @param max_terms Largest factor group for which the L1 maximum enumerates
#'   \eqn{2^g} sign vectors (default 16, i.e. 65,536 vectors). Enumeration is
#'   done in chunks of 4,096 vectors, so memory stays bounded; raising the
#'   limit increases computing time exponentially.
#' @param crit Critical value type: `"F"` (default; exact for a Gaussian linear
#'   model, \eqn{c = g\,F_{g,df}(level)}) or `"chisq"` (\eqn{\chi^2_g}).
#' @return Tibble with `factor`, `estimate`, `lower`, `upper`.
#' @export
mpi_ellipsoid <- function(model, level = 0.95, norm = c("l1", "l2", "linf"),
                          crit = c("F", "chisq"), max_terms = 16L, gap_tol = 1e-6, ...) {
  norm <- rlang::arg_match(norm)
  crit <- rlang::arg_match(crit)
  tm <- .bounds_input(model, "mpi_ellipsoid")
  V <- tm$vcov
  if (is.null(V)) return(.na_bounds(tm, "vcov() unavailable; returning NA bounds."))
  if (isTRUE(tm$vcov_diagonal)) {
    cli::cli_warn("Only standard errors were supplied: the ellipsoid assumes uncorrelated coefficients.")
  }
  rr <- .full_ranges(tm$factors)
  rows <- lapply(tm$factors, function(f) {
    p <- .factor_parts(tm, f, rr)
    g <- length(p$terms)
    if (!g) return(tibble::tibble(factor = f, estimate = 0, lower = 0, upper = 0))
    cc <- if (crit == "F" && is.finite(tm$df)) {
      g * stats::qf(level, g, tm$df)
    } else {
      stats::qchisq(level, g)
    }
    ch <- p$D * p$coef
    Vs <- diag(p$D, g) %*% V[p$terms, p$terms, drop = FALSE] %*% diag(p$D, g)
    bnd <- .ellipsoid_norm_range(ch, Vs, cc, norm, max_terms = max_terms, label = f,
                                 gap_tol = gap_tol)
    tibble::tibble(factor = f, estimate = .magnitude(p, norm),
                   lower = bnd[1], upper = bnd[2])
  })
  dplyr::bind_rows(rows)
}

# Exact range of ||b|| over {b : (b - b0)' V^-1 (b - b0) <= cc} ----------------

.ellipsoid_norm_range <- function(b0, V, cc, norm, max_terms = 16L, label = "group",
                                  gap_tol = 1e-6) {
  g <- length(b0)
  V <- (V + t(V)) / 2
  Vinv <- tryCatch(solve(V), error = function(e) NULL)
  if (is.null(Vinv)) {
    cli::cli_warn("Covariance block for {.val {label}} is singular; ellipsoid bounds set to NA.")
    return(c(NA_real_, NA_real_))
  }
  inside <- as.numeric(t(b0) %*% Vinv %*% b0) <= cc
  sq <- function(s) sqrt(max(0, cc * as.numeric(t(s) %*% V %*% s)))
  # dual objective of min ||b|| over the ellipsoid, for s in the dual-norm ball
  dual <- function(s) sum(s * b0) - sq(s)
  # Return the certified lower bound D(s); warn if the duality gap to the best
  # primal point is not negligible.
  certify <- function(s, nrm, what, primal_upper = Inf) {
    lb <- max(0, dual(s))
    q <- sq(s)
    ub <- primal_upper
    if (q > 0) ub <- min(ub, nrm(b0 - cc * as.numeric(V %*% s) / q))
    gap <- ub - lb
    if (!is.finite(gap) || gap > gap_tol * max(1, ub)) {
      cli::cli_warn("Ellipsoid {what} minimum for {.val {label}}: duality gap {signif(gap, 3)}; the reported (conservative) lower bound may be loose.")
    }
    attr(lb, "gap") <- gap
    lb
  }

  upper <- switch(norm,
    l1 = {
      if (g > max_terms) {
        cli::cli_abort(c(
          "The L1 ellipsoid maximum for {.val {label}} enumerates 2^{g} sign vectors.",
          "i" = "Raise {.arg max_terms} (currently {max_terms}) to allow it; time grows as 2^g."
        ))
      }
      best <- -Inf
      n_all <- 2^g
      chunk <- 4096
      for (start in seq(0, n_all - 1, by = chunk)) {
        idx <- start:min(n_all - 1, start + chunk - 1)
        S <- 2 * outer(idx, 0:(g - 1), function(i, j) (i %/% 2^j) %% 2) - 1
        val <- drop(S %*% b0) + sqrt(pmax(0, cc * rowSums((S %*% V) * S)))
        best <- max(best, val)
      }
      best
    },
    linf = max(abs(b0) + sqrt(cc * diag(V))),
    l2 = sqrt(.trs(b0, V, cc, maximise = TRUE))
  )
  lower <- if (inside) 0 else switch(norm,
    l1 = {
      # Dual: min ||b||_1 = max_{s in [-1,1]^g} D(s). Any s gives a lower bound
      # (weak duality), so the returned value is certified conservative.
      fn <- function(s) -dual(s)
      gr <- function(s) {
        q <- sq(s)
        -(b0 - if (q > 0) cc * as.numeric(V %*% s) / q else 0)
      }
      opt <- stats::optim(sign(b0) * 0.5, fn, gr, method = "L-BFGS-B", lower = -1, upper = 1,
                          control = list(factr = 1e2, pgtol = 0, maxit = 2000))
      certify(opt$par, function(b) sum(abs(b)), "L1")
    },
    linf = {
      # Primal: smallest t such that the box [-t, t]^g meets the ellipsoid
      # (bisection); dual certificate from the touching point.
      b_best <- NULL
      meets <- function(t) {
        fn <- function(b) as.numeric(t(b - b0) %*% Vinv %*% (b - b0))
        gr <- function(b) as.numeric(2 * Vinv %*% (b - b0))
        o <- stats::optim(pmin(pmax(b0, -t), t), fn, gr, method = "L-BFGS-B", lower = -t, upper = t,
                          control = list(factr = 1e3, pgtol = 0))
        if (o$value <= cc * (1 + 1e-9)) b_best <<- o$par
        o$value <= cc * (1 + 1e-9)
      }
      lo <- 0
      hi <- max(abs(b0))
      for (i in 1:60) {
        mid <- (lo + hi) / 2
        if (meets(mid)) hi <- mid else lo <- mid
      }
      if (is.null(b_best)) b_best <- b0
      # KKT: the dual lives on the coordinates at the box face, with their signs
      s0 <- as.numeric(Vinv %*% (b0 - b_best))
      active <- abs(b_best) >= hi * (1 - 1e-6)
      s0 <- ifelse(active & sign(s0) == sign(b_best), s0, 0)
      if (sum(abs(s0)) == 0) s0 <- sign(b0)
      # polish the dual over the L1 sphere (homogeneous objective)
      nd <- function(v) -dual(v / sum(abs(v)))
      pol <- stats::optim(s0 / sum(abs(s0)), nd, method = "BFGS")
      s1 <- if (-pol$value > dual(s0 / sum(abs(s0)))) pol$par / sum(abs(pol$par)) else s0 / sum(abs(s0))
      certify(s1, function(b) max(abs(b)), "L-infinity", primal_upper = hi)
    },
    l2 = sqrt(.trs(b0, V, cc, maximise = FALSE))
  )
  c(lower, upper)
}

# Trust-region subproblem: extremise ||b0 + L u||^2 over ||u|| <= 1 where
# L L' = cc V (L symmetric square root). Returns the optimal squared norm.
.trs <- function(b0, V, cc, maximise) {
  ev <- eigen(cc * V, symmetric = TRUE)
  lam <- pmax(ev$values, 0)
  Q <- ev$vectors
  Lh <- Q %*% diag(sqrt(lam), length(lam)) %*% t(Q)
  w <- as.numeric(t(Q) %*% (Lh %*% b0)) # coordinates of g = L b0
  f_at <- function(u) sum((b0 + Lh %*% u)^2)
  if (maximise) {
    # (mu I - A) u = g, mu >= lambda_max, ||u|| = 1
    lmax <- max(lam)
    nrm <- function(mu) sqrt(sum(w^2 / (mu - lam)^2))
    top <- abs(lam - lmax) < 1e-12 * max(1, lmax)
    if (sum(w[top]^2) < 1e-24) {
      # hard case: mu = lambda_max, fill remaining norm along top eigenvector
      uc <- ifelse(top, 0, w / (lmax - lam))
      rem <- max(0, 1 - sum(uc^2))
      cand <- lapply(c(-1, 1), function(s) {
        u <- uc
        u[which(top)[1]] <- s * sqrt(rem)
        Q %*% u
      })
      return(max(vapply(cand, f_at, numeric(1))))
    }
    hi <- lmax + sqrt(sum(w^2)) + 1
    mu <- stats::uniroot(function(m) nrm(m) - 1, c(lmax + 1e-12 * max(1, lmax) + 1e-300, hi),
                         tol = 1e-14)$root
    u <- Q %*% (w / (mu - lam))
    return(f_at(u))
  }
  # minimise: (A + mu I) u = -g, mu >= 0, ||u|| = 1 (origin outside ellipsoid)
  nrm <- function(mu) sqrt(sum(w^2 / (lam + mu)^2))
  hi <- sqrt(sum(w^2)) + 1
  lo <- if (min(lam) > 0) 0 else 1e-12
  if (nrm(lo) <= 1) {
    u <- Q %*% (-w / (lam + lo))
    return(f_at(u))
  }
  mu <- stats::uniroot(function(m) nrm(m) - 1, c(lo, hi), tol = 1e-14)$root
  u <- Q %*% (-w / (lam + mu))
  f_at(u)
}
