#' Plot MPI estimates with their uncertainty intervals
#'
#' One row per factor (per response panel), ordered by the estimate, with the
#' interval computed by [mpi()] `bounds =`. Factors whose lower bound is above
#' zero are drawn solid; those whose interval reaches zero are drawn open;
#' factors without an interval (bounds unavailable) are drawn as a cross.
#'
#' "Lower bound > 0" is a visual convention, not a significance test: the
#' intervals are confidence regions for the factor's influence, built by the
#' chosen bound method, and no multiplicity correction across factors or
#' responses is applied.
#'
#' Read it as "how precisely is each influence estimated", next to the
#' ranking. Intervals are per factor: they are not simultaneous across
#' factors, so two overlapping intervals do not show that two factors are
#' equally influential, and non-overlap is not a formal test. Use
#' [bootstrap_ranks()] for the stability of the ranking itself.
#'
#' @param x A `herminR_mpi` object computed with `bounds = "se"`, `"delta"`
#'   or `"ellipsoid"`.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @examples
#' data(pastrydough)
#' fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2),
#'           data = pastrydough)
#' mpi_interval_plot(mpi(fit, bounds = "ellipsoid"))
#' @export
mpi_interval_plot <- function(x, ...) {
  if (!inherits(x, "herminR_mpi")) cli::cli_abort("{.arg x} must be a {.cls herminR_mpi} object.")
  df <- tibble::as_tibble(x)
  if (all(is.na(df$lower)) || all(is.na(df$upper))) {
    cli::cli_abort("No bounds to plot; compute with {.code mpi(..., bounds = \"delta\")} (or \"se\", \"ellipsoid\").")
  }
  q <- .quantity_label(df$norm)
  df$value <- abs(df$estimate)
  df$precise <- factor(ifelse(is.na(df$lower), "no interval",
                              ifelse(df$lower > 0, "lower bound > 0", "interval reaches 0")),
                       levels = c("lower bound > 0", "interval reaches 0", "no interval"))
  # order factors within each response panel
  df$key <- paste(df$response, df$factor, sep = "\r")
  ord <- order(df$response, df$value)
  df$key <- factor(df$key, levels = unique(df$key[ord]))
  b <- attr(x, "bounds")
  sub <- if (!is.null(b)) {
    sprintf("%s bounds, level %s", b[["type"]], b[["level"]])
  } else NULL
  p <- ggplot2::ggplot(df, ggplot2::aes(y = .data$key)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_linerange(data = df[!is.na(df$lower) & !is.na(df$upper), , drop = FALSE],
                            ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                            linewidth = 1.1, colour = "grey45") +
    ggplot2::geom_point(ggplot2::aes(x = .data$value, shape = .data$precise), size = 2.6,
                        fill = "white", stroke = 0.9) +
    ggplot2::scale_shape_manual(values = c("lower bound > 0" = 16, "interval reaches 0" = 21, "no interval" = 4),
                                drop = TRUE) +
    ggplot2::scale_y_discrete(labels = function(k) sub("^.*\r", "", k)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom") +
    ggplot2::labs(x = paste0("|", q$short, "|"), y = NULL, shape = NULL,
                  title = paste(q$short, "with uncertainty intervals"),
                  subtitle = paste(c(sub, .norm_note(df$norm)), collapse = "\n"),
                  caption = paste("Intervals are per factor, not simultaneous: overlap does not imply equal influence;",
                                  "\"lower bound > 0\" is not a significance test."))
  if (length(unique(df$response)) > 1) {
    p <- p + ggplot2::facet_wrap(~response, scales = "free_y")
  }
  p
}
