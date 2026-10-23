#' Plot method for herminR_mpi objects
#'
#' A ggplot2 heatmap of MPI values (factors × responses).
#'
#' @param x A `herminR_mpi` object.
#' @param method Standardization applied first: `"none"`, `"colmax"`,
#'   `"residual_sd"` or `"rank"` (see [standardize_mpi()]).
#' @param panel Column to display: `"estimate"`, `"lower"`, `"upper"`, or
#'   `"significance"` (whether the lower bound excludes zero; needs bounds).
#' @param annotate Add numeric labels inside tiles.
#' @param transpose If `TRUE`, responses on the x axis and factors on the y
#'   axis (the layout of Otava and Mylona, 2020).
#' @param reorder Seriate factors and responses (hierarchical clustering when
#'   there are at least three of each, otherwise by mean |MPI|).
#' @param signed Use a signed display. Default: `TRUE` when the object holds
#'   signed values with at least one negative, otherwise `FALSE`.
#' @param palette `"ermine"` (black and white, print friendly; in signed mode
#'   intensity shows |MPI| and a black-and-white diamond marks negative cells)
#'   or `"colour"` (blue–red diverging when signed, blue ramp when not).
#' @param text_size Size of the value labels (ggplot2 mm units).
#' @param cluster_method Linkage for `reorder` clustering, passed to
#'   [stats::hclust()] (default `"complete"`).
#' @param dist_method Distance for `reorder` clustering, passed to
#'   [stats::dist()] (default `"euclidean"`).
#' @param ... Passed to [ggplot2::geom_tile()].
#'
#' @return A `ggplot` object.
#' @export
plot.herminR_mpi <- function(x,
                             method = c("none", "colmax", "residual_sd", "rank"),
                             panel = c("estimate", "lower", "upper", "significance"),
                             annotate = TRUE,
                             reorder = TRUE,
                             transpose = FALSE,
                             signed = NULL,
                             palette = c("ermine", "colour"),
                             text_size = 2.6,
                             cluster_method = "complete",
                             dist_method = "euclidean",
                             ...) {
  method <- rlang::arg_match(method)
  panel <- rlang::arg_match(panel)
  palette <- rlang::arg_match(palette)

  obj <- if (method != "none") standardize_mpi(x, method = method) else x
  df <- tibble::as_tibble(obj)

  if (panel == "significance") {
    df$plot_val <- ifelse(is.na(df$lower), NA, as.numeric(df$lower > 0))
    if (all(is.na(df$plot_val))) cli::cli_warn("No bounds available; compute MPI with {.arg bounds}.")
  } else {
    df$plot_val <- df[[panel]]
  }
  if (all(is.na(df$plot_val))) {
    cli::cli_warn("All values in panel {.val {panel}} are missing.")
  }
  if (is.null(signed)) {
    signed <- !isTRUE(attr(x, "abs")) && any(df$plot_val < 0, na.rm = TRUE) && method != "rank"
  }

  df <- .order_axes(df, reorder, cluster_method = cluster_method, dist_method = dist_method)
  if (panel == "significance") {
    df$sig <- factor(ifelse(is.na(df$plot_val), NA, ifelse(df$plot_val > 0, "lower > 0", "includes 0")),
                     levels = c("lower > 0", "includes 0"))
  }
  norm_note <- if ("norm" %in% names(df)) .norm_note(df$norm) else NULL
  q <- .quantity_label(if ("norm" %in% names(df)) df$norm else "l1")
  # legend: the quantity itself for the estimate panel, otherwise "panel (quantity)"
  fill_lab <- function(abs_val) {
    nm <- if (abs_val) paste0("|", q$short, "|") else q$short
    if (panel == "estimate") nm else paste0(panel, " (", q$short, ")")
  }
  absmax <- suppressWarnings(max(abs(df$plot_val), na.rm = TRUE))
  if (!is.finite(absmax) || absmax == 0) absmax <- 1

  if (transpose) {
    df$xv <- df$response; df$yv <- df$factor
  } else {
    df$xv <- df$factor; df$yv <- df$response
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$xv, y = .data$yv)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = if (transpose) "Response" else "Factor",
                  y = if (transpose) "Process factor" else "Response",
                  title = q$title,
                  subtitle = if (!is.null(norm_note)) strwrap(norm_note, 80) |> paste(collapse = "\n"))

  if (panel == "significance") {
    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = .data$sig), colour = "grey80", ...) +
      ggplot2::scale_fill_manual(values = c("lower > 0" = "grey20", "includes 0" = "white"),
                                 na.value = "grey90", drop = FALSE) +
      ggplot2::labs(fill = "Bound")
    return(p)
  }

  if (palette == "ermine") {
    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = abs(.data$plot_val)), colour = "grey80", ...) +
      ggplot2::scale_fill_gradient(low = "white", high = "black", limits = c(0, absmax),
                                   na.value = "grey90") +
      ggplot2::labs(fill = fill_lab(TRUE))
    if (signed) {
      neg <- df[!is.na(df$plot_val) & df$plot_val < 0, , drop = FALSE]
      if (nrow(neg)) {
        p <- p + ggplot2::geom_point(data = neg, shape = 23, size = 2.2, fill = "black",
                                     colour = "white", stroke = 0.6,
                                     position = ggplot2::position_nudge(x = 0.32, y = 0.3)) +
          ggplot2::labs(caption = paste0("\u25c6 negative ", q$short, " (decreasing response)"))
      }
    }
  } else if (signed) {
    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = .data$plot_val), colour = "grey80", ...) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                    midpoint = 0, limits = c(-absmax, absmax), na.value = "grey90") +
      ggplot2::labs(fill = fill_lab(FALSE))
  } else {
    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = .data$plot_val), colour = "grey80", ...) +
      ggplot2::scale_fill_gradient(low = "white", high = "#08306B", na.value = "grey90") +
      ggplot2::labs(fill = fill_lab(FALSE))
  }

  if (annotate) {
    df$lab <- ifelse(is.na(df$plot_val), "", sprintf("%.2f", df$plot_val))
    df$dark <- !is.na(df$plot_val) & abs(df$plot_val) / absmax > 0.55
    p <- p + ggplot2::geom_text(data = df, ggplot2::aes(label = .data$lab, colour = .data$dark),
                                size = text_size, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"))
  }
  p
}

.order_axes <- function(df, reorder, cluster_method = "complete", dist_method = "euclidean") {
  fac <- unique(df$factor)
  resp <- unique(df$response)
  if (reorder && length(fac) > 1) {
    M <- tapply(abs(df$plot_val), list(df$factor, df$response), function(v) v[1])
    M[is.na(M)] <- 0
    if (length(fac) >= 3 && length(resp) >= 3) {
      hc <- function(X) stats::hclust(stats::dist(X, method = dist_method), method = cluster_method)$order
      fac <- rownames(M)[hc(M)]
      resp <- colnames(M)[hc(t(M))]
    } else {
      fac <- rownames(M)[order(-rowMeans(M))]
      resp <- colnames(M)[order(-colMeans(M))]
    }
  }
  df$factor <- factor(df$factor, levels = fac)
  df$response <- factor(df$response, levels = resp)
  df
}

#' @rdname plot.herminR_mpi
#' @param object A `herminR_mpi` object.
#' @method autoplot herminR_mpi
#' @export
autoplot.herminR_mpi <- function(object, ...) {
  plot.herminR_mpi(object, ...)
}

#' Side-by-side heatmaps for several norms
#'
#' @param x Output of [mpi_compare_norms()].
#' @param ... Unused.
#' @return A `ggplot` object (one panel per norm). Each panel is scaled by its
#'   own maximum, so the plot compares the *pattern* of factor influence
#'   between norms; values are not comparable in magnitude across panels
#'   (only L1 is an attainable change). The caption states this.
#' @export
plot.herminR_norms <- function(x, ...) {
  df <- dplyr::bind_rows(lapply(names(x), function(n) {
    d <- tibble::as_tibble(x[[n]])
    d$value <- abs(d$estimate) / max(max(abs(d$estimate), na.rm = TRUE), .Machine$double.eps)
    d$norm <- n
    d
  }))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$factor, y = .data$response, fill = .data$value)) +
    ggplot2::geom_tile(colour = "grey80") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value), colour = .data$value > 0.55),
                       size = 3.2, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white")) +
    ggplot2::facet_wrap(~norm) +
    ggplot2::scale_fill_gradient(low = "white", high = "grey30") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Factor", y = "Response", fill = "|value| / max",
                  title = "Factor influence summaries under different norms",
                  subtitle = "L1 = MPI (attainable change); L2, Linf = contribution scores, not MPI",
                  caption = paste("Each panel is scaled by its own maximum: compare patterns within a norm,",
                                  "not magnitudes across norms."))
}

#' Waterfall-style effects plot (alternative visualization)
#'
#' The competitor graphic from referee W7: factors sorted by |MPI|.
#'
#' @param model An `lm`/`rsm` object or `herminR_mpi` object.
#' @param ... Passed to [mpi()] when a model is supplied.
#' @return A ggplot object.
#' @export
mpi_waterfall <- function(model, ...) {
  if (!inherits(model, "herminR_mpi")) model <- mpi(model, ...)
  df <- tibble::as_tibble(model)
  if (length(unique(df$response)) > 1) {
    cli::cli_abort("{.fn mpi_waterfall} shows one response; filter the object first.")
  }
  df <- df[order(abs(df$estimate), decreasing = TRUE), ]
  df$factor <- factor(df$factor, levels = rev(df$factor))
  q <- .quantity_label(if ("norm" %in% names(df)) df$norm else "l1")
  ggplot2::ggplot(df, ggplot2::aes(x = .data$factor, y = .data$estimate)) +
    ggplot2::geom_col(fill = "grey30") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = NULL, y = q$short, title = paste(q$short, "waterfall"),
                  subtitle = .norm_note(if ("norm" %in% names(df)) df$norm else "l1"))
}

# Plot title and short label for the quantity shown (MPI only for L1)
.quantity_label <- function(norms) {
  n <- unique(stats::na.omit(norms))
  if (!length(n) || identical(n, "l1")) {
    return(list(title = "Maximal Possible Influence", short = "MPI"))
  }
  if (length(n) > 1) return(list(title = "Factor influence scores (mixed norms)", short = "score"))
  lab <- c(l2 = "L2", linf = "L-infinity")[[n]]
  short <- c(l2 = "L2 score", linf = "Linf score")[[n]]
  list(title = paste0("Factor influence score (", lab, " norm, not MPI)"), short = short)
}
