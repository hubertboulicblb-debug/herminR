#' Standardize MPI values
#'
#' \itemize{
#'   \item `"colmax"` – Eq. (5) of Otava and Mylona (2020): divide by the maximum |MPI| within each
#'     group. `by = "response"` (default) scales each response so its most
#'     influential factor is 1; `by = "factor"` scales each factor across
#'     responses. Check which grouping their heatmap uses before
#'     reproducing its figures.
#'   \item `"residual_sd"` – divide by the response's residual standard
#'     deviation, so MPI is expressed in noise units (requires fitted models).
#'   \item `"rank"` – replace |MPI| by its rank within each response (the most
#'     influential factor gets the highest rank). Ranks are ordinal and are not
#'     comparable as magnitudes.
#' }
#' `lower`, `upper` and `se` are scaled by the same divisor (set to `NA` for
#' ranks).
#'
#' @param mpi_object An object of class `herminR_mpi`.
#' @param method Standardization method.
#' @param by Grouping for `"colmax"`: `"response"` or `"factor"`.
#'
#' @return The modified `herminR_mpi` object, with the original values in
#'   `estimate_raw`.
#' @export
standardize_mpi <- function(mpi_object, method = c("colmax", "residual_sd", "rank"),
                            by = c("response", "factor")) {
  method <- rlang::arg_match(method)
  by <- rlang::arg_match(by)
  if (!inherits(mpi_object, "herminR_mpi")) {
    cli::cli_abort("{.arg mpi_object} must be a {.cls herminR_mpi} object.")
  }
  keep_attr <- attributes(mpi_object)[c("model", "terms", "abs", "models", "term_maps", "ranges", "subset_keep")]
  out <- tibble::as_tibble(mpi_object)
  out$estimate_raw <- out$estimate
  div <- rep(NA_real_, nrow(out))

  if (method == "colmax") {
    g <- out[[by]]
    for (lev in unique(g)) {
      i <- which(g == lev)
      m <- suppressWarnings(max(abs(out$estimate[i]), na.rm = TRUE))
      div[i] <- if (is.finite(m) && m > 0) m else 1
    }
  } else if (method == "residual_sd") {
    sig <- .response_sigmas(mpi_object)
    if (is.null(sig)) {
      cli::cli_warn("No fitted model attached; {.code residual_sd} standardization not applied.")
      div[] <- 1
    } else {
      div <- unname(sig[out$response])
      if (anyNA(div)) {
        cli::cli_warn("Residual SD unavailable for some responses; left unscaled.")
        div[is.na(div)] <- 1
      }
    }
  }

  if (method == "rank") {
    out$estimate <- stats::ave(abs(out$estimate), out$response,
                               FUN = function(v) rank(v, ties.method = "average"))
    out$lower <- out$upper <- out$se <- NA_real_
  } else {
    out$estimate <- out$estimate / div
    out$lower <- out$lower / div
    out$upper <- out$upper / div
    out$se <- out$se / div
  }

  out <- .new_mpi(out, keep_attr[["model"]], keep_attr[["terms"]], keep_attr[["abs"]])
  for (a in c("models", "term_maps", "ranges", "subset_keep")) attr(out, a) <- keep_attr[[a]]
  attr(out, "standardization") <- c(method = method, by = if (method == "colmax") by else NA)
  out
}

.response_sigmas <- function(x) {
  models <- attr(x, "models")
  if (!is.null(models)) {
    return(vapply(models, function(m) tryCatch(stats::sigma(m), error = function(e) NA_real_), numeric(1)))
  }
  m <- attr(x, "model")
  if (!is.null(m)) return(stats::setNames(stats::sigma(m), unique(x$response)[1]))
  tm <- attr(x, "terms")
  if (!is.null(tm) && is.finite(tm$sigma)) return(stats::setNames(tm$sigma, unique(x$response)[1]))
  NULL
}
