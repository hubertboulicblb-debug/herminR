#' Does a sampled feasible-region MPI converge?
#'
#' A sampled feasible-region MPI ([mpi_feasible()] with `method = "lhs"` or
#' `"grid"`) is the largest change found among the sampled pairs, so it is a
#' **lower estimate** of the true maximum. This function repeats the sampling
#' over increasing sample sizes and several seeds so you can see whether the
#' estimate has stabilised. When there is no `constraint` (a box region), the
#' exact value is computed too and every sampled value must lie at or below
#' it.
#'
#' A factor is called *stable* when, at the largest sample size, the spread
#' across seeds and the change from the previous size are both below `tol`
#' (relative to the estimate).
#'
#' @inheritParams mpi_feasible
#' @param sizes Increasing sample sizes (pairs per factor).
#' @param seeds Seeds; each size is run once per seed.
#' @param tol Relative tolerance for calling a factor stable.
#' @return A tibble of class `herminR_convergence` with `factor`, `size`,
#'   `seed`, `estimate`, `n_feasible` and `exact` (`NA` with a constraint);
#'   attribute `summary`: one row per factor with `estimate` (median at the
#'   largest size), `spread` (relative range across seeds at the largest
#'   size), `last_change` (relative change of the median from the previous
#'   size), `gap_to_exact` (relative, `NA` with a constraint), `min_feasible`
#'   (fewest feasible pairs in any run) and `stable`.
#' @examples
#' b <- c(x1 = 1, x2 = 0.5, "x1:x2" = 0.3, "x1^2" = -0.2)
#' cv <- mpi_feasible_convergence(b, constraint = function(d) d$x1 + d$x2 <= 0,
#'                                sizes = c(250, 500, 1000), seeds = 1:3)
#' attr(cv, "summary")
#' plot(cv)
#' @export
mpi_feasible_convergence <- function(model, region = list(), constraint = NULL, method = c("lhs", "grid"),
                                     sizes = c(500, 1000, 2000, 5000, 10000), seeds = 1:5,
                                     factors = NULL, tol = 0.01) {
  method <- rlang::arg_match(method)
  if (!is.numeric(sizes) || length(sizes) < 2 || any(!is.finite(sizes)) || any(sizes < 10) || is.unsorted(sizes, strictly = TRUE)) {
    cli::cli_abort("{.arg sizes} must be at least two increasing numbers >= 10.")
  }
  if (!length(seeds)) cli::cli_abort("{.arg seeds} must contain at least one seed.")
  exact <- if (is.null(constraint)) {
    e <- mpi_feasible(model, region = region, method = "exact", factors = factors)
    stats::setNames(e$estimate, e$factor)
  } else NULL
  runs <- do.call(rbind, lapply(sizes, function(n) do.call(rbind, lapply(seeds, function(s) {
    r <- mpi_feasible(model, region = region, constraint = constraint, method = method, size = n, seed = s,
                      factors = factors)
    data.frame(factor = r$factor, size = n, seed = s, estimate = r$estimate, n_feasible = r$n_feasible,
               stringsAsFactors = FALSE)
  }))))
  runs$exact <- if (is.null(exact)) NA_real_ else unname(exact[runs$factor])
  out <- tibble::as_tibble(runs)
  attr(out, "summary") <- .convergence_summary(out, tol)
  attr(out, "method") <- method
  attr(out, "tol") <- tol
  class(out) <- c("herminR_convergence", class(out))
  out
}

.convergence_summary <- function(x, tol) {
  sizes <- sort(unique(x$size))
  last <- sizes[length(sizes)]
  prev <- sizes[length(sizes) - 1L]
  rel <- function(a, b) ifelse(abs(b) > 0, a / abs(b), ifelse(abs(a) > 0, Inf, 0))
  do.call(rbind, lapply(unique(x$factor), function(f) {
    d <- x[x$factor == f, , drop = FALSE]
    at_last <- d$estimate[d$size == last]
    med_last <- stats::median(at_last)
    med_prev <- stats::median(d$estimate[d$size == prev])
    spread <- rel(diff(range(at_last)), med_last)
    change <- rel(abs(med_last - med_prev), med_last)
    gap <- if (all(is.na(d$exact))) NA_real_ else rel(d$exact[1] - max(at_last), d$exact[1])
    data.frame(factor = f, estimate = med_last, spread = spread, last_change = change, gap_to_exact = gap,
               min_feasible = min(d$n_feasible), stable = spread <= tol & change <= tol, stringsAsFactors = FALSE)
  }))
}

#' @export
print.herminR_convergence <- function(x, ...) {
  s <- attr(x, "summary")
  cat(sprintf("Convergence of sampled feasible-region MPI (%s, %d sizes x %d seeds; tolerance %s%%)\n",
              toupper(attr(x, "method")), length(unique(x$size)), length(unique(x$seed)), format(100 * attr(x, "tol"))))
  print(tibble::as_tibble(s))
  invisible(x)
}

#' Plot the convergence of a sampled feasible-region MPI
#'
#' One panel per factor: each point is one run (size x seed), the line joins
#' the median across seeds, and the dashed line is the exact value when it is
#' known (box regions). Estimates should climb towards the exact value and
#' level off; a curve still rising at the largest size means more samples are
#' needed.
#' @param x Output of [mpi_feasible_convergence()].
#' @param ... Unused.
#' @return A ggplot object.
#' @export
plot.herminR_convergence <- function(x, ...) {
  d <- as.data.frame(x)
  s <- attr(x, "summary")
  med <- stats::aggregate(estimate ~ factor + size, data = d, FUN = stats::median)
  lab <- stats::setNames(paste0(s$factor, if (any(!s$stable)) ifelse(s$stable, " (stable)", " (not yet stable)") else " (stable)"),
                         s$factor)
  d$panel <- factor(lab[d$factor], levels = lab)
  med$panel <- factor(lab[med$factor], levels = lab)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$size, y = .data$estimate)) +
    ggplot2::geom_point(alpha = 0.55, size = 1.8, colour = "grey30",
                        position = ggplot2::position_jitter(width = 0.02, height = 0, seed = 1)) +
    ggplot2::geom_line(data = med, colour = "#2F4B7C", linewidth = 1) +
    ggplot2::scale_x_log10() +
    ggplot2::facet_wrap(~panel, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Sampled pairs per factor (log scale)", y = "Feasible MPI (sampled maximum)",
                  title = "Does the sampled feasible-region MPI converge?",
                  subtitle = paste0("Points: one run per seed; line: median",
                                    if (any(!is.na(d$exact))) "; dashed: exact value (box region)" else
                                      "; no exact value with a constraint: look for a flat line"),
                  caption = "A sampled maximum is a lower estimate: it can only approach the true value from below.")
  if (any(!is.na(d$exact))) {
    ex <- unique(d[!is.na(d$exact), c("panel", "exact")])
    p <- p + ggplot2::geom_hline(data = ex, ggplot2::aes(yintercept = .data$exact), linetype = "dashed", colour = "#E6550D")
  }
  p
}
