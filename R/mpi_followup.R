# Helpers to recompute an MPI object (single or multi-response) from its term
# maps, so follow-up manipulations are exact rather than re-weighted.

.term_maps_of <- function(x) {
  tms <- attr(x, "term_maps")
  if (!is.null(tms)) return(tms)
  tm <- attr(x, "terms")
  if (is.null(tm)) {
    cli::cli_abort("MPI object lacks term information; recompute it with {.fn mpi}.")
  }
  stats::setNames(list(tm), unique(x$response)[1])
}

.recompute <- function(x, tms, ranges, keep = NULL) {
  norm <- unique(x$norm)[1]
  abs <- isTRUE(attr(x, "abs"))
  res <- lapply(names(tms), function(resp) {
    tm <- tms[[resp]]
    out <- .mpi_table(tm, norm = norm, abs = abs, ranges = ranges[intersect(names(ranges), tm$factors)])
    out$response <- resp
    if (!is.null(keep)) out <- out[out$factor %in% keep, , drop = FALSE]
    # bounds no longer apply to the modified quantity
    out$lower <- NA_real_
    out$upper <- NA_real_
    out
  })
  out <- .new_mpi(dplyr::bind_rows(res), model = attr(x, "model"),
                  tm = if (length(tms) == 1L) tms[[1]] else NULL, abs = abs)
  attr(out, "term_maps") <- tms
  attr(out, "models") <- attr(x, "models")
  out
}

#' Adjust MPI for a change in a factor's studied range
#'
#' Implements the rule of Otava and Mylona (2020), Section 3.3, exactly. When factor \eqn{X_n}'s coded range
#' becomes \eqn{[l, u]}, every interaction contribution \eqn{\delta_{nm}} that
#' enters *another* factor's MPI through \eqn{X_n} is evaluated at the most
#' favourable level of \eqn{X_n} inside \eqn{[l, u]}, i.e. scaled by
#' \eqn{\max(|l|, |u|)} (the new half-width for a symmetric range such as
#' \eqn{(-0.1, 0.1)}). Main and quadratic effects are untouched because MPI is
#' a one-unit change on the transformed scale, and \eqn{X_n}'s own MPI is
#' unchanged because the other factors still span their full ranges.
#' Asymmetric ranges are handled exactly (signs that cannot be aligned inside
#' the range are not counted).
#'
#' @param mpi_object An object of class `herminR_mpi`.
#' @param ranges Named list of new coded ranges, e.g. `list(x3 = c(-0.1, 0.1))`.
#' @param warn_extrapolation Warn when a range extends beyond \eqn{[-1, 1]}.
#'
#' @return A modified `herminR_mpi` object (bounds reset to `NA`); the ranges
#'   are stored in the `"ranges"` attribute so that [mpi_subset()] can be
#'   stacked on top.
#' @examples
#' b <- c(x1 = 0.5, x2 = -0.3, x3 = 0.2, "x2:x3" = 0.4, "x1:x3" = 0.1)
#' mpi_adjust_range(mpi(b), ranges = list(x3 = c(-0.1, 0.1)))
#' @export
mpi_adjust_range <- function(mpi_object, ranges = list(), warn_extrapolation = TRUE) {
  if (!inherits(mpi_object, "herminR_mpi")) {
    cli::cli_abort("{.arg mpi_object} must be a {.cls herminR_mpi} object.")
  }
  for (f in names(ranges)) {
    if (length(ranges[[f]]) != 2L) cli::cli_abort("Each range must be a numeric vector of length 2.")
    if (warn_extrapolation && any(abs(ranges[[f]]) > 1 + 1e-8)) {
      cli::cli_warn("Range for {.val {f}} extends beyond coded [-1, 1]; this extrapolates the fitted surface.")
    }
  }
  prev <- attr(mpi_object, "ranges")
  if (is.null(prev)) prev <- list()
  prev[names(ranges)] <- ranges
  keep <- attr(mpi_object, "subset_keep")
  tms <- .term_maps_of(mpi_object)
  out <- .recompute(mpi_object, tms, prev, keep = keep)
  attr(out, "ranges") <- prev
  attr(out, "subset_keep") <- keep
  out
}

#' Recompute MPI after restricting the set of retained factors
#'
#' Implements the logic of Figure 4 of Otava and Mylona (2020). With `others = "centre"`, excluded
#' factors are fixed at their centre point (0 in coded units), so every term
#' involving an excluded factor drops out of the retained factors' MPI: a
#' factor whose apparent importance was carried by interactions with excluded
#' factors deflates. With `others = "favourable"` the excluded factors keep
#' their most favourable level and the retained factors' MPI is unchanged
#' (the naive full-set value, useful for ratio plots).
#'
#' @param mpi_object An object of class `herminR_mpi`.
#' @param keep Character vector of factor names, or integer positions in the
#'   object's factor order.
#' @param others `"centre"` (default) or `"favourable"`.
#' @return A modified `herminR_mpi` object containing only the kept factors.
#' @export
mpi_subset <- function(mpi_object, keep, others = c("centre", "favourable")) {
  others <- rlang::arg_match(others)
  if (!inherits(mpi_object, "herminR_mpi")) {
    cli::cli_abort("{.arg mpi_object} must be a {.cls herminR_mpi} object.")
  }
  factors <- unique(mpi_object$factor)
  if (is.numeric(keep)) {
    if (any(keep < 1 | keep > length(factors))) cli::cli_abort("{.arg keep} positions out of range.")
    keep <- factors[keep]
  }
  missing <- setdiff(as.character(keep), factors)
  if (length(missing)) cli::cli_abort("Unknown factor(s) in {.arg keep}: {.val {missing}}.")
  keep <- as.character(keep)

  ranges <- attr(mpi_object, "ranges")
  if (is.null(ranges)) ranges <- list()

  if (others == "favourable") {
    out <- mpi_object[mpi_object$factor %in% keep, , drop = FALSE]
    out <- .new_mpi(out, attr(mpi_object, "model"), attr(mpi_object, "terms"), attr(mpi_object, "abs"))
    attr(out, "term_maps") <- attr(mpi_object, "term_maps")
  } else {
    tms <- lapply(.term_maps_of(mpi_object), function(tm) {
      ok <- vapply(tm$powers, function(p) all(names(p) %in% keep), logical(1))
      tm$coef <- tm$coef[ok]
      tm$powers <- tm$powers[ok]
      tm$factors <- intersect(tm$factors, keep)
      if (!is.null(tm$vcov)) tm$vcov <- tm$vcov[names(tm$coef), names(tm$coef), drop = FALSE]
      tm
    })
    out <- .recompute(mpi_object, tms, ranges, keep = keep)
  }
  attr(out, "ranges") <- if (length(ranges)) ranges else NULL
  attr(out, "subset_keep") <- keep
  attr(out, "others") <- others
  out
}
