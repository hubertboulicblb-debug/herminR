# Flow views of MPI: where influence comes from, how factors interact, and how
# stable the ranking is. Drawn with ggplot2 only.

#' Term-by-term anatomy of MPI
#'
#' Splits each factor's influence into the model terms it is built from. For a
#' one-unit increase of factor \eqn{X_n} with the other factors set most
#' favourably, a term contributes \eqn{D_t |b_t|}, where \eqn{b_t} is its
#' coefficient and \eqn{D_t} the largest change of the term's monomial over
#' the step (1 for main effects, 1 for quadratic effects, the partner factor's
#' half-range for two-factor interactions). An interaction term therefore
#' contributes to **both** of its factors: this is how one factor's influence
#' can be amplified by another.
#'
#' For the L1 norm (MPI) and full, symmetric ranges the contributions of a
#' factor add up exactly to its MPI (`check` column). For L2 and L-infinity
#' scores they are the components that the norm combines; with asymmetric
#' ranges ([mpi_adjust_range()]) not every sign can be aligned and the sum is
#' an upper bound.
#'
#' @param x A `herminR_mpi` object (from [mpi()] or [mpi_multi()]).
#' @param response Optional response name(s) to keep.
#' @return A tibble with `response`, `factor`, `term` (readable label),
#'   `type` (`"main"`, `"quadratic"`, `"interaction"`, `"higher order"`),
#'   `partners` (other factors in the term), `coef`, `contribution`
#'   (\eqn{D_t |b_t|}), `share` (of the factor's total) and `mpi` (the factor's
#'   value in `x`).
#' @examples
#' data(pastry_table1)
#' mpi_contributions(mpi_multi(pastry_table1), response = "y4")
#' @export
mpi_contributions <- function(x, response = NULL) {
  if (!inherits(x, "herminR_mpi")) cli::cli_abort("{.arg x} must be a {.cls herminR_mpi} object.")
  tms <- .term_maps_of(x)
  if (!is.null(response)) {
    miss <- setdiff(response, names(tms))
    if (length(miss)) cli::cli_abort("Unknown response(s): {.val {miss}}.")
    tms <- tms[response]
  }
  ranges <- attr(x, "ranges")
  if (is.null(ranges)) ranges <- list()
  df <- tibble::as_tibble(x)
  keep <- attr(x, "subset_keep")
  rows <- lapply(names(tms), function(r) {
    tm <- tms[[r]]
    rr <- .full_ranges(tm$factors, ranges[intersect(names(ranges), tm$factors)])
    fs <- if (is.null(keep)) tm$factors else intersect(tm$factors, keep)
    do.call(rbind, lapply(fs, function(f) {
      p <- .factor_parts(tm, f, rr)
      if (!length(p$terms)) return(NULL)
      pw <- tm$powers[p$terms]
      data.frame(
        response = r, factor = f, term = vapply(pw, .term_label, character(1)),
        type = vapply(pw, function(q) .term_type(q, f), character(1)),
        partners = vapply(pw, function(q) paste(setdiff(names(q), f), collapse = ","), character(1)),
        coef = unname(p$coef), contribution = unname(p$D * abs(p$coef)),
        stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) {
    return(tibble::tibble(response = character(0), factor = character(0), term = character(0),
                          type = character(0), partners = character(0), coef = numeric(0),
                          contribution = numeric(0), share = numeric(0), mpi = numeric(0)))
  }
  tot <- stats::ave(out$contribution, out$response, out$factor, FUN = sum)
  out$share <- ifelse(tot > 0, out$contribution / tot, 0)
  out$mpi <- abs(df$estimate[match(paste(out$response, out$factor), paste(df$response, df$factor))])
  tibble::as_tibble(out)
}

.term_label <- function(p) {
  paste(ifelse(p == 1L, names(p), paste0(names(p), "^", p)), collapse = ":")
}

.term_type <- function(p, f) {
  if (length(p) == 1L) {
    if (p[[1]] == 1L) "main" else if (p[[1]] == 2L) "quadratic" else "higher order"
  } else if (length(p) == 2L && all(p == 1L)) "interaction" else "higher order"
}

.term_colours <- function() {
  c(main = "#4C72B0", quadratic = "#8172B3", interaction = "#DD8452", `higher order` = "#937860")
}

# Sankey engine ---------------------------------------------------------------------

#' Lay out and draw a Sankey diagram with ggplot2
#'
#' @param nodes data.frame: `id`, `col` (integer column), `label`, `fill`
#'   (group for the colour scale); row order = vertical order within a column.
#' @param links data.frame: `from`, `to` (node ids), `value` (> 0), `fill`.
#' @param colours Named colour vector for `fill` groups.
#' @param col_titles Column headings.
#' @param highlight Optional node id(s) whose links are emphasised.
#' @param value_fmt Formatter for node values in labels (or `NULL`).
#' @return A ggplot.
#' @keywords internal
#' @noRd
.ggsankey <- function(nodes, links, colours, col_titles = NULL, highlight = NULL,
                      value_fmt = function(v) formatC(v, digits = 3, format = "g"), legend_drop = NULL,
                      compact = FALSE) {
  links <- links[links$value > 0, , drop = FALSE]
  nodes <- nodes[nodes$id %in% c(links$from, links$to), , drop = FALSE]
  inflow <- tapply(links$value, factor(links$to, levels = nodes$id), sum)
  outflow <- tapply(links$value, factor(links$from, levels = nodes$id), sum)
  inflow[is.na(inflow)] <- 0
  outflow[is.na(outflow)] <- 0
  nodes$value <- as.numeric(pmax(inflow, outflow)[nodes$id])
  cols <- sort(unique(nodes$col))
  tot <- vapply(cols, function(k) sum(nodes$value[nodes$col == k]), numeric(1))
  nmax <- max(table(nodes$col))
  gap <- 0.25 * max(tot) / max(nmax, 4)
  height <- max(tot + gap * (as.numeric(table(nodes$col)[as.character(cols)]) - 1))
  nodes$y0 <- NA_real_
  for (k in cols) {
    i <- which(nodes$col == k)
    used <- sum(nodes$value[i]) + gap * (length(i) - 1)
    y <- (height - used) / 2
    for (j in i) {
      nodes$y0[j] <- y
      y <- y + nodes$value[j] + gap
    }
  }
  nodes$y1 <- nodes$y0 + nodes$value
  w <- 0.06
  pos <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  # stack links at each end in the order of the node at the other end
  links <- links[order(pos[links$from], pos[links$to]), , drop = FALSE]
  links$s0 <- NA_real_
  for (id in unique(links$from)) {
    i <- which(links$from == id)
    i <- i[order(pos[links$to[i]])]
    links$s0[i] <- nodes$y0[pos[id]] + c(0, utils::head(cumsum(links$value[i]), -1))
  }
  links$t0 <- NA_real_
  for (id in unique(links$to)) {
    i <- which(links$to == id)
    i <- i[order(pos[links$from[i]])]
    links$t0[i] <- nodes$y0[pos[id]] + c(0, utils::head(cumsum(links$value[i]), -1))
  }
  u <- seq(0, 1, length.out = 40)
  s <- (1 - cos(pi * u)) / 2
  polys <- do.call(rbind, lapply(seq_len(nrow(links)), function(k) {
    l <- links[k, ]
    xs <- nodes$col[pos[l$from]] + w / 2
    xt <- nodes$col[pos[l$to]] - w / 2
    x <- xs + (xt - xs) * u
    top <- l$s0 + (l$t0 - l$s0) * s
    bot <- top + l$value
    hl <- is.null(highlight) || l$from %in% highlight || l$to %in% highlight
    data.frame(link = k, x = c(x, rev(x)), y = c(top, rev(bot)), fill = l$fill, hl = hl)
  }))
  last <- max(cols)
  nodes$lab <- if (is.null(value_fmt)) nodes$label else paste0(nodes$label, "  ", value_fmt(nodes$value))
  # labels of very thin nodes would overlap their neighbours: leave them out (the tables list them)
  col_tot <- tapply(nodes$value, nodes$col, sum)
  tiny <- nodes$value < 0.015 * col_tot[as.character(nodes$col)]
  n_tiny <- sum(tiny)
  nodes$lab[tiny] <- ""
  # compact (narrow screens): first-column labels go inside, to the right of their node
  left_out <- !compact
  nodes$lx <- ifelse(nodes$col == min(cols) & left_out, nodes$col - w / 2 - 0.02, nodes$col + w / 2 + 0.02)
  nodes$hj <- ifelse(nodes$col == min(cols) & left_out, 1, 0)
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = polys, ggplot2::aes(x = .data$x, y = .data$y, group = .data$link,
                                                     fill = .data$fill, alpha = .data$hl), colour = NA) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 0.55, `FALSE` = 0.12), guide = "none") +
    ggplot2::geom_rect(data = nodes, ggplot2::aes(xmin = .data$col - w / 2, xmax = .data$col + w / 2,
                                                  ymin = .data$y0, ymax = .data$y1, fill = .data$fill),
                       colour = "grey25", linewidth = 0.25) +
    rlang::exec(if (compact) ggplot2::geom_label else ggplot2::geom_text,
                data = nodes[nzchar(nodes$lab), , drop = FALSE],
                mapping = ggplot2::aes(x = .data$lx, y = (.data$y0 + .data$y1) / 2, label = .data$lab, hjust = .data$hj),
                size = if (compact) 3 else 3.4,
                !!!(if (compact) c(list(fill = grDevices::adjustcolor("white", 0.8), label.padding = ggplot2::unit(0.1, "lines")),
                                   .label_no_border()) else list())) +
    ggplot2::scale_fill_manual(values = colours, breaks = setdiff(intersect(names(colours), unique(c(nodes$fill, links$fill))), legend_drop)) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_continuous(breaks = cols, labels = if (is.null(col_titles)) NULL else col_titles[seq_along(cols)],
                                position = "top", expand = ggplot2::expansion(add = c(0.08, 0.08))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    # room for the labels outside the outer columns (about 5.6 pt per character at size 3.4)
    ggplot2::theme(axis.text.x.top = ggplot2::element_text(face = "bold", size = 11, margin = ggplot2::margin(b = 4)),
                   legend.position = "bottom",
                   plot.margin = ggplot2::margin(8, 8 + (if (compact) 5 else 5.6) * max(c(0, nchar(nodes$lab[nodes$col == last]))), 8,
                                                 8 + if (left_out) 5.6 * max(c(0, nchar(nodes$lab[nodes$col == min(cols)]))) else 34)) +
    ggplot2::labs(fill = NULL,
                  caption = if (n_tiny) sprintf("%d very small node%s unlabelled (see the table).", n_tiny, if (n_tiny > 1) "s" else "") else NULL)
  p
}

#' Sankey diagrams of MPI
#'
#' Two views of the same numbers:
#'
#' * `view = "terms"`: for one response, how each factor's influence is built
#'   from the model's terms ([mpi_contributions()]). Ribbon width is the term's
#'   contribution; an interaction term sends one ribbon to each of its two
#'   factors, which shows how factors amplify each other. Colours give the
#'   type of term.
#' * `view = "responses"`: which factors drive which responses. Ribbon width is
#'   the factor's |MPI| in that response, after optional standardisation (use
#'   `standardize = "colmax"`, Eq. 5 of Otava and Mylona (2020), when the responses are on
#'   different scales). Small flows can be hidden with `min_value`.
#'
#' @param x A `herminR_mpi` object.
#' @param view `"terms"` or `"responses"`.
#' @param response Response shown by `view = "terms"` (default: the first).
#' @param standardize For `view = "responses"`: `"colmax"` (default, per
#'   response maximum = 1) or `"none"`.
#' @param min_value Hide flows smaller than this (after standardisation).
#' @param highlight Optional factor name to emphasise.
#' @param compact Put the first column's labels inside the diagram (for narrow
#'   screens).
#' @return A ggplot object.
#' @examples
#' data(pastry_table1)
#' m <- mpi_multi(pastry_table1)
#' mpi_sankey(m, response = "y4")
#' mpi_sankey(m, view = "responses")
#' @export
mpi_sankey <- function(x, view = c("terms", "responses"), response = NULL,
                       standardize = c("colmax", "none"), min_value = 0, highlight = NULL, compact = FALSE) {
  view <- rlang::arg_match(view)
  standardize <- rlang::arg_match(standardize)
  if (!inherits(x, "herminR_mpi")) cli::cli_abort("{.arg x} must be a {.cls herminR_mpi} object.")
  q <- .quantity_label(unique(x$norm))
  if (view == "terms") {
    resp <- unique(x$response)
    if (is.null(response)) response <- resp[1]
    if (length(response) != 1L || !response %in% resp) cli::cli_abort("{.arg response} must be one of {.val {resp}}.")
    cc <- mpi_contributions(x, response = response)
    cc <- cc[cc$contribution > min_value, , drop = FALSE]
    if (!nrow(cc)) cli::cli_abort("No term contributes to {.val {response}}.")
    fac_tot <- tapply(cc$contribution, cc$factor, sum)
    f_order <- names(sort(fac_tot, decreasing = TRUE))
    # terms ordered by their main factor, interactions after the first of their factors
    first_f <- tapply(match(cc$factor, f_order), cc$term, min)
    ttype <- tapply(cc$type, cc$term, `[`, 1)
    type_rank <- match(ttype, c("main", "quadratic", "interaction", "higher order"))
    t_order <- names(first_f)[order(first_f, type_rank, names(first_f))]
    nodes <- rbind(
      data.frame(id = paste0("t:", t_order), col = 1L, label = t_order, fill = unname(ttype[t_order])),
      data.frame(id = paste0("f:", f_order), col = 2L, label = f_order, fill = "factor"))
    links <- data.frame(from = paste0("t:", cc$term), to = paste0("f:", cc$factor), value = cc$contribution,
                        fill = cc$type)
    p <- .ggsankey(nodes, links, colours = c(.term_colours(), factor = "grey40"), legend_drop = "factor",
                   col_titles = c("Model term", paste("Factor (", q$short, ")", sep = "")),
                   highlight = if (!is.null(highlight)) paste0("f:", highlight), compact = compact)
    l1 <- identical(unique(x$norm), "l1")
    p + ggplot2::labs(
      title = paste0("Where the influence on ", response, " comes from"),
      subtitle = if (l1) "Ribbon width: the term's contribution to the factor's MPI; an interaction feeds both of its factors."
                 else paste("Ribbon width: the term's component of the", q$short, "(the norm combines them; they do not add up)."))
  } else {
    m <- if (standardize == "colmax") standardize_mpi(x, "colmax", by = "response") else x
    d <- tibble::as_tibble(m)
    d$value <- abs(d$estimate)
    d <- d[!is.na(d$value) & d$value > min_value, , drop = FALSE]
    if (!nrow(d)) cli::cli_abort("No flow above {.arg min_value}.")
    f_order <- names(sort(tapply(d$value, d$factor, sum), decreasing = TRUE))
    r_order <- unique(x$response)
    r_order <- r_order[r_order %in% d$response]
    # responses ordered by their strongest factor, to reduce crossings
    r_key <- vapply(r_order, function(r) {
      dr <- d[d$response == r, , drop = FALSE]
      match(dr$factor[which.max(dr$value)], f_order)
    }, numeric(1))
    r_order <- r_order[order(r_key)]
    pal <- .factor_palette(f_order)
    nodes <- rbind(data.frame(id = paste0("f:", f_order), col = 1L, label = f_order, fill = f_order),
                   data.frame(id = paste0("r:", r_order), col = 2L, label = r_order, fill = "response"))
    links <- data.frame(from = paste0("f:", d$factor), to = paste0("r:", d$response), value = d$value, fill = d$factor)
    p <- .ggsankey(nodes, links, colours = c(pal, response = "grey40"),
                   col_titles = c("Factor", "Response"),
                   highlight = if (!is.null(highlight)) paste0("f:", highlight),
                   value_fmt = NULL, compact = compact)
    p + ggplot2::guides(fill = "none") +
      ggplot2::labs(title = "Which factors drive which responses",
                    subtitle = paste0("Ribbon width: |", q$short, "|",
                                      if (standardize == "colmax") ", scaled so each response's largest = 1 (Eq. 5)" else "",
                                      if (min_value > 0) paste0("; flows below ", min_value, " hidden") else ""))
  }
}

.factor_palette <- function(f) {
  base <- c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#8172B3", "#937860", "#DA8BC3", "#8C8C8C",
            "#CCB974", "#64B5CD", "#2F4B7C", "#A05195")
  stats::setNames(rep(base, length.out = length(f)), f)
}

#' Interaction network of a response surface
#'
#' Factors are drawn on a circle. A node's area is the factor's |MPI| (or
#' score) for the chosen response, its shade the share coming from the
#' factor's own terms (main and quadratic effects) rather than from
#' interactions. An edge joins two factors that interact; its width is the
#' interaction's contribution \eqn{|\delta|} and its line type the sign of the
#' coefficient. Strongly connected factors are the ones whose influence depends
#' on where the others are set.
#'
#' @param x A `herminR_mpi` object.
#' @param response Response to draw (default: the first).
#' @param highlight Optional factor name to emphasise.
#' @return A ggplot object.
#' @examples
#' data(pastry_table1)
#' mpi_network(mpi_multi(pastry_table1), response = "y4")
#' @export
mpi_network <- function(x, response = NULL, highlight = NULL) {
  if (!inherits(x, "herminR_mpi")) cli::cli_abort("{.arg x} must be a {.cls herminR_mpi} object.")
  resp <- unique(x$response)
  if (is.null(response)) response <- resp[1]
  if (length(response) != 1L || !response %in% resp) cli::cli_abort("{.arg response} must be one of {.val {resp}}.")
  d <- tibble::as_tibble(x)
  d <- d[d$response == response, , drop = FALSE]
  cc <- mpi_contributions(x, response = response)
  fs <- d$factor
  k <- length(fs)
  ang <- pi / 2 - 2 * pi * (seq_len(k) - 1) / k
  nodes <- data.frame(factor = fs, x = cos(ang), y = sin(ang), value = abs(d$estimate))
  own <- tapply(cc$contribution[cc$type %in% c("main", "quadratic")], cc$factor[cc$type %in% c("main", "quadratic")], sum)
  tot <- tapply(cc$contribution, cc$factor, sum)
  nodes$own_share <- ifelse(is.na(tot[fs]) | tot[fs] == 0, 0, ifelse(is.na(own[fs]), 0, own[fs]) / tot[fs])
  inter <- cc[cc$type == "interaction", , drop = FALSE]
  inter <- inter[!duplicated(inter$term), , drop = FALSE]
  edges <- if (nrow(inter)) {
    data.frame(a = inter$factor, b = inter$partners, value = inter$contribution,
               sign = ifelse(inter$coef >= 0, "positive", "negative"), term = inter$term)
  } else data.frame(a = character(0), b = character(0), value = numeric(0), sign = character(0), term = character(0))
  edges$x <- nodes$x[match(edges$a, fs)]
  edges$y <- nodes$y[match(edges$a, fs)]
  edges$xend <- nodes$x[match(edges$b, fs)]
  edges$yend <- nodes$y[match(edges$b, fs)]
  edges$hl <- if (is.null(highlight)) rep(TRUE, nrow(edges)) else edges$a %in% highlight | edges$b %in% highlight
  q <- .quantity_label(unique(x$norm))
  vmax <- max(c(nodes$value, 1e-12))
  edges$sign <- factor(edges$sign, levels = c("positive", "negative"))
  p <- ggplot2::ggplot()
  if (nrow(edges)) {
    p <- p +
      ggplot2::geom_segment(data = edges, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
                                                       linewidth = .data$value, linetype = .data$sign, alpha = .data$hl),
                            colour = "#DD8452", lineend = "round") +
      rlang::exec(ggplot2::geom_label, data = edges,
                  mapping = ggplot2::aes(x = (.data$x + .data$xend) / 2, y = (.data$y + .data$yend) / 2,
                                         label = formatC(.data$value, digits = 2, format = "g"), alpha = .data$hl),
                  size = 3, fill = "white", colour = "#A0522D", !!!.label_no_border()) +
      ggplot2::scale_linewidth(range = c(0.6, 7), guide = "none") +
      ggplot2::scale_linetype_manual(values = c(positive = "solid", negative = "22"),
                                     breaks = levels(droplevels(edges$sign)), name = "Interaction sign")
  }
  p <- p +
    ggplot2::geom_point(data = nodes, ggplot2::aes(x = .data$x, y = .data$y, size = .data$value, fill = .data$own_share),
                        shape = 21, colour = if (is.null(highlight)) "grey20" else "grey50", stroke = 0.8) +
    ggplot2::geom_text(data = nodes, ggplot2::aes(x = 1.4 * .data$x, y = 1.4 * .data$y,
                                                  label = paste0(.data$factor, "\n", formatC(.data$value, digits = 3, format = "g"))),
                       size = 3.6, lineheight = 0.9) +
    ggplot2::scale_size_area(max_size = 20, limits = c(0, vmax), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 0.9, `FALSE` = 0.15), guide = "none") +
    ggplot2::scale_fill_gradient(low = "#FBE3D2", high = "#2F4B7C", limits = c(0, 1),
                                 labels = function(v) paste0(round(100 * v), "%")) +
    ggplot2::coord_equal(xlim = c(-1.7, 1.7), ylim = c(-1.6, 1.6), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.key.width = ggplot2::unit(1.6, "cm"),
                   plot.margin = ggplot2::margin(8, 8, 8, 8)) +
    ggplot2::labs(title = paste0("How the factors interact (", response, ")"),
                  subtitle = paste0("Node area: |", q$short, "|; edge width: interaction contribution |delta|."),
                  fill = "Share from own terms")
  if (!is.null(highlight) && highlight %in% fs) {
    h <- nodes[nodes$factor == highlight, , drop = FALSE]
    p <- p + ggplot2::geom_point(data = h, ggplot2::aes(x = .data$x, y = .data$y, size = .data$value), shape = 21,
                                 colour = "#E6550D", fill = NA, stroke = 2.4)
  }
  p
}

# A label without a border, across ggplot2 versions (label.size became
# linewidth in ggplot2 3.5.0)
.label_no_border <- function() {
  if (utils::packageVersion("ggplot2") >= "3.5.0") list(linewidth = 0) else list(label.size = 0)
}
