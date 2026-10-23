#' The one-unit step that defines MPI, for a second-order model
#'
#' Finds where the Maximal Possible Influence of `factor` is attained
#' (Otava and Mylona, 2020, Eq. 4, full coded ranges): the start level of `factor` (-1, 0 or -0.5 when
#' it has no quadratic term), the direction of the change, and the most
#' favourable level of every other factor (+1 or -1 according to the sign of
#' its interaction with `factor`; 0 when it does not interact).
#'
#' @param tm A term map (one response).
#' @param factor Factor name.
#' @return A list: `direction` (+1 or -1), `start`, `end` (named numeric
#'   vectors of coded settings for all factors), `change` (fitted change
#'   along the step), `mpi`.
#' @keywords internal
#' @noRd
.mpi_step <- function(tm, factor) {
  fs <- tm$factors
  rr <- .full_ranges(fs)
  parts <- .factor_parts(tm, factor, rr)
  if (!length(parts$terms)) cli::cli_abort("{.val {factor}} has no term in this model, so its MPI is 0.")
  if (!parts$second_order) {
    cli::cli_abort("The contour view needs a model of order 2 at most (main, quadratic and two-factor interaction terms).")
  }
  beta <- 0; gam <- 0
  delta <- stats::setNames(numeric(length(fs)), fs)
  for (t in parts$terms) {
    p <- tm$powers[[t]]
    b <- tm$coef[[t]]
    if (length(p) == 1L && p[[1]] == 1L) beta <- beta + b
    else if (length(p) == 1L && p[[1]] == 2L) gam <- gam + b
    else {
      m <- setdiff(names(p), factor)
      delta[m] <- delta[m] + b
    }
  }
  hi <- beta + abs(gam) + sum(abs(delta))
  lo <- beta - abs(gam) - sum(abs(delta))
  s <- if (hi >= -lo) 1 else -1
  a <- if (gam * s > 0) 0 else if (gam * s < 0) -1 else -0.5
  lev <- s * sign(delta)
  start <- lev
  start[factor] <- a
  end <- start
  end[factor] <- a + 1
  change <- .surface_value(tm, end) - .surface_value(tm, start)
  list(direction = s, start = start, end = end, change = change, mpi = max(hi, -lo))
}

# Fitted surface without the intercept (relative to the design centre)
.surface_value <- function(tm, x) {
  v <- 0
  for (t in names(tm$powers)) {
    p <- tm$powers[[t]]
    v <- v + tm$coef[[t]] * prod(x[names(p)]^p)
  }
  unname(v)
}

#' Contour view of the MPI definition
#'
#' Draws the fitted response surface over two factors, with every other
#' factor held at its most favourable level for `factor`, and the one-coded-unit
#' step whose change is the MPI of `factor` (Otava and Mylona, 2020, Eq. 4) as an annotated
#' arrow. The vertical position of the arrow is the partner factor's most
#' favourable level, so the plot shows why the MPI is "the largest change
#' from a one-unit increase, the other factors set where they amplify it
#' most".
#'
#' The surface is the model's polynomial in the coded factors without the
#' intercept and block terms, i.e. the fitted response relative to the design
#' centre; differences along the arrow are the same as for the fitted
#' response. Second-order models only (main, quadratic and two-factor
#' interaction terms), full coded ranges.
#'
#' @param x A `herminR_mpi` object.
#' @param factor Factor whose MPI step is drawn (x axis).
#' @param partner Second factor (y axis). Default: the factor with the
#'   strongest interaction with `factor` (or the next factor).
#' @param response Response (default: the first).
#' @param n Grid size per axis.
#' @return A ggplot object with attribute `"mpi_step"` (a list with
#'   `factor`, `partner`, `start`, `end`, `change`, `mpi`, `direction`).
#' @examples
#' data(pastrydough)
#' fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2),
#'           data = pastrydough)
#' mpi_contour(mpi(fit), factor = "x2")
#' @export
mpi_contour <- function(x, factor, partner = NULL, response = NULL, n = 81L) {
  if (!inherits(x, "herminR_mpi")) cli::cli_abort("{.arg x} must be a {.cls herminR_mpi} object.")
  tms <- .term_maps_of(x)
  if (is.null(response)) response <- names(tms)[1]
  if (length(response) != 1L || !response %in% names(tms)) {
    cli::cli_abort("{.arg response} must be one of {.val {names(tms)}}.")
  }
  if (length(attr(x, "ranges"))) {
    cli::cli_inform(c("i" = "The contour view uses the full coded ranges [-1, 1] (the MPI definition)."))
  }
  tm <- tms[[response]]
  fs <- tm$factors
  if (length(fs) < 2L) cli::cli_abort("The contour view needs at least two factors.")
  if (length(factor) != 1L || !factor %in% fs) cli::cli_abort("{.arg factor} must be one of {.val {fs}}.")
  st <- .mpi_step(tm, factor)
  if (is.null(partner)) {
    inter <- vapply(setdiff(fs, factor), function(m) {
      sum(vapply(names(tm$powers), function(t) {
        p <- tm$powers[[t]]
        if (setequal(names(p), c(factor, m)) && all(p == 1L)) abs(tm$coef[[t]]) else 0
      }, numeric(1)))
    }, numeric(1))
    partner <- if (any(inter > 0)) names(which.max(inter)) else setdiff(fs, factor)[1]
  }
  if (length(partner) != 1L || !partner %in% setdiff(fs, factor)) {
    cli::cli_abort("{.arg partner} must be one of {.val {setdiff(fs, factor)}}.")
  }
  g <- seq(-1, 1, length.out = n)
  grid <- expand.grid(u = g, v = g)
  base <- st$start
  grid$z <- vapply(seq_len(nrow(grid)), function(i) {
    xx <- base
    xx[factor] <- grid$u[i]
    xx[partner] <- grid$v[i]
    .surface_value(tm, xx)
  }, numeric(1))
  y0 <- base[[partner]]
  arrow_df <- data.frame(x = st$start[[factor]], xend = st$end[[factor]], y = y0, yend = y0)
  others <- setdiff(fs, c(factor, partner))
  fixed <- if (length(others)) paste(paste0(others, " = ", formatC(base[others], format = "g")), collapse = ", ") else NULL
  q <- .quantity_label("l1")
  lab <- paste0("+1 coded unit of ", factor, ":\nfitted ", response, " changes by ",
                formatC(st$change, digits = 3, format = "f"), " (|", q$short, "| = ", formatC(st$mpi, digits = 3, format = "f"), ")")
  p <- ggplot2::ggplot(grid, ggplot2::aes(x = .data$u, y = .data$v)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$z), interpolate = TRUE) +
    ggplot2::geom_contour(ggplot2::aes(z = .data$z), colour = "white", alpha = 0.7, bins = 12, linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = y0, colour = "grey30", linetype = "dotted") +
    ggplot2::geom_segment(data = arrow_df, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                          inherit.aes = FALSE, colour = "#E6550D", linewidth = 1.6,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.35, "cm"), type = "closed")) +
    ggplot2::geom_point(data = data.frame(x = st$start[[factor]], y = y0), ggplot2::aes(x = .data$x, y = .data$y),
                        inherit.aes = FALSE, size = 3, shape = 21, fill = "white", colour = "#E6550D", stroke = 1.4) +
    rlang::exec(ggplot2::annotate, "label", x = 0, y = y0, vjust = if (y0 > 0.5) 1.6 else -0.6, label = lab,
                size = 3.4, fill = grDevices::adjustcolor("white", 0.85), !!!.label_no_border()) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = .surface_value(tm, base * 0)) +
    ggplot2::coord_equal(xlim = c(-1.06, 1.06), ylim = c(-1.06, 1.06), expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title.position = "plot") +
    ggplot2::labs(x = paste(factor, "(coded)"), y = paste(partner, "(coded)"),
                  fill = paste0("Fitted ", response, "\n(relative to\nthe centre)"),
                  title = paste0("What the MPI of ", factor, " means (", response, ")"),
                  subtitle = paste(strwrap(paste0("Arrow: the one-unit step of ", factor,
                                                  " with the other factors where they amplify it most; dotted line: ", partner, " = ",
                                                  formatC(y0, format = "g"), " (its most favourable level)."), 70), collapse = "\n"),
                  caption = if (!is.null(fixed)) paste0("Held at their most favourable levels: ", fixed, ".") else NULL)
  attr(p, "mpi_step") <- list(factor = factor, partner = partner, start = st$start, end = st$end,
                              change = st$change, mpi = st$mpi, direction = st$direction)
  p
}
