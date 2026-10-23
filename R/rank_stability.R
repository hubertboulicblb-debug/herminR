#' Bootstrap ranking stability for MPI
#'
#' Refits the model on bootstrap samples, recomputes |MPI| and summarises the
#' distribution of each factor's rank (1 = most influential).
#'
#' * `type = "residual"` (default) keeps the design fixed and resamples
#'   residuals: \eqn{y^* = \hat y + e^*}. Appropriate for designed
#'   experiments, where the factor settings are not random.
#' * `type = "case"` resamples whole runs (the "pairs" bootstrap). Resampled
#'   designs can lose rank; replicates with aliased coefficients are counted
#'   in `n_aliased` and dropped.
#' * `type = "pairs"` is accepted as an alias of `"case"` (in versions before
#'   0.1.1 it wrongly performed residual resampling).
#'
#' Works with \pkg{rsm} `SO()` fits: the model is re-expressed with explicit
#' monomial terms on its raw data before resampling.
#'
#' @param model An `lm` or `rsm` object.
#' @param n Number of bootstrap replicates.
#' @param type `"residual"`, `"case"` or `"pairs"` (= `"case"`).
#' @param level Coverage of the rank interval.
#' @param norm Norm passed to [mpi()].
#' @param seed Random seed.
#'
#' @return A tibble of class `herminR_ranks` with `factor`, `estimate` (|MPI|
#'   of the original fit), `rank_mean`, `rank_lower`, `rank_upper` (empirical
#'   quantiles, type 1), `top1_frac`, `top3_frac`; attributes `n_ok`,
#'   `n_failed`, `n_aliased` (these three add up to `n`), `type`, `level`,
#'   `norm`, and `rank_matrix` (one row per successful replicate, one column
#'   per factor, in the order of `factor`).
#'
#' **Ties** (for example two factors with no remaining terms) share the
#' smallest rank (`ties.method = "min"`), so several factors can rank first in
#' the same replicate and `top1_frac` can sum to more than 1 across factors.
#' Replicates where the refit fails, or has aliased coefficients, are dropped
#' and counted; a warning is given when more than 10% are dropped.
#' @export
bootstrap_ranks <- function(model,
                            n = 1000L,
                            type = c("residual", "case", "pairs"),
                            level = 0.95,
                            norm = "l1",
                            seed = NULL) {
  type <- rlang::arg_match(type)
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2 || n != round(n)) {
    cli::cli_abort("{.arg n} must be a single whole number >= 2.")
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level <= 0 || level >= 1) {
    cli::cli_abort("{.arg level} must be a single number strictly between 0 and 1.")
  }
  norm <- rlang::arg_match(norm, c("l1", "l2", "linf"))
  if (type == "pairs") {
    cli::cli_inform(c("i" = "{.code type = \"pairs\"} now means case resampling (as in the standard terminology)."))
    type <- "case"
  }
  base <- .as_monomial_lm(model)
  fit0 <- base$fit
  factors <- base$factors
  m0 <- suppressWarnings(mpi(fit0, norm = norm, abs = TRUE, factors = factors))
  dd <- base$data
  yname <- deparse(base$response)
  if (!yname %in% names(dd)) {
    cli::cli_abort("Bootstrap needs a plain response variable (got {.code {yname}}).")
  }
  fitted0 <- stats::fitted(fit0)
  res0 <- stats::residuals(fit0)
  n_obs <- nrow(dd)
  fml <- stats::formula(fit0)

  run_one <- function(i) {
    if (type == "residual") {
      d_star <- dd
      d_star[[yname]] <- fitted0 + sample(res0, n_obs, replace = TRUE)
    } else {
      d_star <- dd[sample.int(n_obs, n_obs, replace = TRUE), , drop = FALSE]
    }
    m_star <- tryCatch(stats::lm(fml, data = d_star), error = function(e) NULL)
    if (is.null(m_star)) return("failed")
    if (anyNA(stats::coef(m_star))) return("aliased")
    mp <- tryCatch(suppressWarnings(mpi(m_star, norm = norm, abs = TRUE, factors = factors)),
                   error = function(e) NULL)
    if (is.null(mp)) return("failed")
    stats::setNames(.rank_desc(mp$estimate), mp$factor)[factors]
  }

  reps <- if (is.null(seed)) lapply(seq_len(n), run_one) else withr::with_seed(seed, lapply(seq_len(n), run_one))
  status <- vapply(reps, function(r) if (is.character(r)) r else "ok", character(1))
  ok <- reps[status == "ok"]
  if (!length(ok)) cli::cli_abort("All bootstrap replicates failed ({sum(status == 'aliased')} aliased).")
  if (mean(status != "ok") > 0.1) {
    cli::cli_warn("{sum(status != 'ok')} of {n} replicates dropped ({sum(status == 'aliased')} aliased).")
  }
  mat <- do.call(rbind, ok)
  a <- (1 - level) / 2
  out <- tibble::tibble(
    factor = factors,
    estimate = m0$estimate[match(factors, m0$factor)],
    rank_mean = unname(colMeans(mat)),
    rank_lower = unname(apply(mat, 2, stats::quantile, probs = a, type = 1)),
    rank_upper = unname(apply(mat, 2, stats::quantile, probs = 1 - a, type = 1)),
    top1_frac = unname(colMeans(mat == 1)),
    top3_frac = unname(colMeans(mat <= 3))
  )
  attr(out, "rank_matrix") <- unname(mat)
  attr(out, "n_ok") <- length(ok)
  attr(out, "n_failed") <- sum(status == "failed")
  attr(out, "n_aliased") <- sum(status == "aliased")
  attr(out, "type") <- type
  attr(out, "level") <- level
  attr(out, "norm") <- norm
  class(out) <- c("herminR_ranks", class(out))
  out
}

#' Plot bootstrap rank stability
#'
#' `type = "interval"` (default): one row per factor, ordered by mean
#' bootstrap rank: the point is the mean rank, the bar the rank interval at
#' the requested level, and the label the share of replicates in which the
#' factor ranked first. Wide bars mean the ranking of that factor is not
#' stable under resampling, whatever its estimated influence.
#'
#' `type = "flow"`: a Sankey diagram from each factor (left) to the ranks it
#' took in the replays (right); ribbon width is the share of replicates. A
#' factor that always lands in the same place is one straight ribbon; one
#' that fans out has an uncertain rank.
#'
#' @param x Output of [bootstrap_ranks()].
#' @param type `"interval"` or `"flow"`.
#' @param highlight Optional factor name to emphasise (`type = "flow"`).
#' @param compact Labels inside the diagram, for narrow screens (`type = "flow"`).
#' @param ... Unused.
#' @return A `ggplot` object.
#' @export
plot.herminR_ranks <- function(x, type = c("interval", "flow"), highlight = NULL, compact = FALSE, ...) {
  type <- rlang::arg_match(type)
  if (type == "flow") return(.rank_flow_plot(x, highlight, compact))
  df <- tibble::as_tibble(x)
  df$factor <- factor(df$factor, levels = rev(df$factor[order(df$rank_mean)]))
  q <- .quantity_label(attr(x, "norm"))
  lvl <- attr(x, "level")
  ggplot2::ggplot(df, ggplot2::aes(y = .data$factor)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$rank_lower, xmax = .data$rank_upper),
                            linewidth = 1.2, colour = "grey55") +
    ggplot2::geom_point(ggplot2::aes(x = .data$rank_mean), size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(x = .data$rank_upper, label = sprintf("top-1: %.0f%%", 100 * .data$top1_frac)),
                       hjust = -0.15, size = 3) +
    ggplot2::scale_x_continuous(breaks = seq_len(nrow(df)),
                                expand = ggplot2::expansion(mult = c(0.05, 0.25))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(x = "Bootstrap rank (1 = most influential)", y = NULL,
                  title = paste("Rank stability of", q$short),
                  subtitle = sprintf("%s bootstrap, %d replicates; bars: %.0f%% rank interval",
                                     attr(x, "type"), attr(x, "n_ok"), 100 * if (is.null(lvl)) 0.95 else lvl),
                  caption = "Labels: share of replicates in which the factor ranked first.")
}

.rank_flow_plot <- function(x, highlight = NULL, compact = FALSE) {
  mat <- attr(x, "rank_matrix")
  if (is.null(mat)) cli::cli_abort("This result has no replicate ranks; rerun {.fn bootstrap_ranks} with herminR >= 0.4.0.")
  fs <- x$factor
  f_order <- fs[order(x$rank_mean)]
  tab <- do.call(rbind, lapply(seq_along(fs), function(j) {
    t <- table(mat[, j])
    data.frame(factor = fs[j], rank = as.integer(names(t)), share = as.numeric(t) / nrow(mat))
  }))
  ranks <- sort(unique(tab$rank))
  pal <- .factor_palette(f_order)
  top1 <- x$top1_frac[match(f_order, fs)]
  nodes <- rbind(data.frame(id = paste0("f:", f_order), col = 1L,
                            label = paste0(f_order, "  (first in ", round(100 * top1), "%)"), fill = f_order),
                 data.frame(id = paste0("r:", ranks), col = 2L, label = paste("rank", ranks), fill = "rank"))
  links <- data.frame(from = paste0("f:", tab$factor), to = paste0("r:", tab$rank), value = tab$share, fill = tab$factor)
  .ggsankey(nodes, links, colours = c(pal, rank = "grey40"), col_titles = c("Factor", "Rank in the replays"),
            highlight = if (!is.null(highlight)) paste0("f:", highlight),
            value_fmt = NULL, compact = compact) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(title = "Where each factor lands when the experiment is replayed",
                  subtitle = sprintf("%s bootstrap, %d replicates; ribbon width: share of replicates (ties share the best rank)",
                                     attr(x, "type"), attr(x, "n_ok")))
}

# Rank 1 = largest; ties share the smallest rank
.rank_desc <- function(x) {
  r <- rank(-x, ties.method = "min")
  if (!is.null(names(x))) names(r) <- names(x)
  r
}
