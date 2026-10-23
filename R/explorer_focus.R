# Hover details and the highlighted factor, linked across stages -------------------
#
# Uses Shiny's own plot interaction (plotOutput(hover =, click =)); no extra
# packages. A click on a heatmap cell, an interval row or a bootstrap row
# highlights that factor on every stage; hovering shows the numbers behind
# the mark.

#' Label under the pointer on a discrete axis
#' @param ev A Shiny hover or click event.
#' @param axis `"x"` or `"y"`.
#' @return The label, or `NULL` when outside the plotted categories.
#' @keywords internal
#' @noRd
explorer_pick <- function(ev, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (is.null(ev) || is.null(ev[[axis]])) return(NULL)
  lv <- ev$domain$discrete_limits[[axis]]
  if (is.null(lv)) return(NULL)
  i <- round(ev[[axis]])
  if (!is.finite(i) || i < 1 || i > length(lv) || abs(ev[[axis]] - i) > 0.5) return(NULL)
  as.character(lv[[i]])
}

#' Draw a ggplot and record where its panels are
#'
#' Shiny can only translate a pointer position into plot categories when it
#' can read the plot's coordinate map; with ggplot2 >= 4.0 and older Shiny
#' versions it cannot, and hover and click events arrive without categories.
#' The explorer therefore records the panel geometry itself: it prints the
#' plot, locates every panel viewport on the device and keeps, per panel, its
#' position (as fractions of the image), its axis ranges, its discrete limits
#' and its facet value.
#'
#' @param p A ggplot.
#' @param store An environment; the geometry is stored as `store[[id]]`.
#' @param id Output id.
#' @return `invisible(NULL)` (for use as the last line of `renderPlot()`).
#' @keywords internal
#' @noRd
explorer_draw_interactive <- function(p, store, id) {
  print(p)
  store[[id]] <- tryCatch(.plot_geometry(p), error = function(e) NULL)
  invisible(NULL)
}

.plot_geometry <- function(p) {
  grid::grid.force()
  vps <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
  vps <- unique(grep("^panel", vps, value = TRUE))
  if (!length(vps)) return(NULL)
  size <- grDevices::dev.size("in")
  boxes <- do.call(rbind, lapply(vps, function(v) {
    grid::seekViewport(v)
    ll <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE)
    ur <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"), valueOnly = TRUE)
    grid::upViewport(0)
    data.frame(left = ll$x / size[1], right = ur$x / size[1], bottom = ll$y / size[2], top = ur$y / size[2])
  }))
  # panels in reading order (top to bottom, left to right) = layout order (ROW, COL)
  boxes <- boxes[order(-round(boxes$top, 4), boxes$left), , drop = FALSE]
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  lay <- lay[order(lay$ROW, lay$COL), , drop = FALSE]
  if (nrow(lay) != nrow(boxes)) return(NULL)
  lim <- function(sc) tryCatch(sc$get_limits(), error = function(e) NULL)
  facet_cols <- setdiff(names(lay), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD"))
  lapply(seq_len(nrow(lay)), function(i) {
    pp <- b$layout$panel_params[[as.integer(lay$PANEL[i])]]
    xl <- lim(pp$x); yl <- lim(pp$y)
    list(box = boxes[i, ], x_range = pp$x.range, y_range = pp$y.range,
         x_limits = if (is.character(xl)) xl else NULL, y_limits = if (is.character(yl)) yl else NULL,
         panel = if (length(facet_cols)) as.character(lay[[facet_cols[1]]][i]) else NULL)
  })
}

#' Give a pointer event its plot coordinates from the recorded geometry
#' @param ev Hover or click event (needs `coords_css`).
#' @param geom Output of `.plot_geometry()`.
#' @param width,height Size of the plot image in CSS pixels.
#' @return An event with `x`, `y`, `panelvar1` and `domain$discrete_limits`
#'   for the panel under the pointer, or `NULL`.
#' @keywords internal
#' @noRd
explorer_geometry_event <- function(ev, geom, width, height) {
  if (is.null(ev) || is.null(geom) || is.null(ev$coords_css) || is.null(width) || is.null(height) ||
      !is.finite(width) || !is.finite(height) || width <= 0 || height <= 0) return(NULL)
  fx <- ev$coords_css$x / width
  fy <- 1 - ev$coords_css$y / height
  for (g in geom) {
    bx <- g$box
    if (fx >= bx$left && fx <= bx$right && fy >= bx$bottom && fy <= bx$top) {
      x <- g$x_range[1] + (fx - bx$left) / (bx$right - bx$left) * diff(g$x_range)
      y <- g$y_range[1] + (fy - bx$bottom) / (bx$top - bx$bottom) * diff(g$y_range)
      return(list(x = x, y = y, panelvar1 = g$panel, coords_css = ev$coords_css,
                  domain = list(discrete_limits = list(x = g$x_limits, y = g$y_limits))))
    }
  }
  NULL
}

#' Factor and response under the pointer, for each interactive plot
#' @param ev Event.
#' @param plot `"heatmap"`, `"interval"` or `"rank"`.
#' @param factors Factor names of the analysis.
#' @param response Response of the rank plot.
#' @return `list(factor, response)` or `NULL`.
#' @keywords internal
#' @noRd
explorer_event_cell <- function(ev, plot, factors, response = NULL) {
  if (is.null(ev)) return(NULL)
  out <- switch(plot,
    heatmap = {
      x <- explorer_pick(ev, "x"); y <- explorer_pick(ev, "y")
      if (is.null(x) || is.null(y)) NULL
      else if (y %in% factors) list(factor = y, response = x)   # paper layout (transposed)
      else list(factor = x, response = y)
    },
    interval = {
      k <- explorer_pick(ev, "y")
      if (is.null(k)) NULL else list(factor = sub("^.*\r", "", k), response = sub("\r.*$", "", k))
    },
    rank = {
      f <- explorer_pick(ev, "y")
      if (is.null(f)) NULL else list(factor = f, response = response)
    })
  if (is.null(out) || !(out$factor %in% factors)) return(NULL)
  out
}

.rank_abs <- function(v) rank(-abs(v), ties.method = "min", na.last = "keep")

#' Plain facts about one factor in one response
#'
#' @param f,r Factor and response.
#' @param shown The MPI object on screen (possibly standardised).
#' @param base The unstandardised MPI object.
#' @param unc MPI with intervals, or `NULL`.
#' @param boot `bootstrap_ranks()` result for `boot_response`, or `NULL`.
#' @param boot_response Response of `boot`.
#' @param cmp Output of `explorer_compare()`, or `NULL`.
#' @param std Standardisation code.
#' @param flags Output of `explorer_interval_flags()`, or `NULL`.
#' @return A named character vector (label -> text); empty when unknown.
#' @keywords internal
#' @noRd
explorer_factor_facts <- function(f, r, shown, base, unc = NULL, boot = NULL, boot_response = NULL,
                                  cmp = NULL, std = "none", flags = NULL) {
  b <- tibble::as_tibble(base)
  s <- tibble::as_tibble(shown)
  i <- which(b$factor == f & b$response == r)
  if (!length(i)) return(character())
  q <- .quantity_label(b$norm[i])$short
  est <- b$estimate[i]
  out <- c(Cell = sprintf("%s on %s", f, r))
  dir <- b$direction[i]
  out[[q]] <- sprintf("%.4g%s", est,
                      if (!is.na(dir) && dir %in% c("increase", "decrease")) sprintf(" (raising %s %ss %s)", f, dir, r) else "")
  if (!identical(std, "none")) {
    j <- which(s$factor == f & s$response == r)
    if (length(j)) out[["Shown value"]] <- sprintf("%.3g (%s)", s$estimate[j], .std_label(std))
  }
  sr <- s[s$response == r, , drop = FALSE]
  rk <- .rank_abs(sr$estimate)[sr$factor == f]
  if (length(rk) && !is.na(rk)) out[["Rank in response"]] <- sprintf("%d of %d", rk, nrow(sr))
  if (!is.null(unc)) {
    u <- tibble::as_tibble(unc)
    k <- which(u$factor == f & u$response == r)
    bd <- attr(unc, "bounds")
    if (length(k) && !is.na(u$lower[k])) {
      out[["Interval"]] <- sprintf("%.3g to %.3g%s", u$lower[k], u$upper[k],
                                   if (!is.null(bd)) sprintf(" (%s, %s)", bd[["type"]], bd[["level"]]) else "")
      if (!is.null(flags)) {
        fk <- which(flags$factor == f & flags$response == r)
        if (length(fk) && !flags$reliable[fk[1]]) out[["Interval"]] <- paste(out[["Interval"]], "- may be too narrow")
      }
    }
  }
  if (!is.null(boot) && identical(boot_response, r)) {
    k <- which(boot$factor == f)
    if (length(k)) {
      out[["Bootstrap"]] <- sprintf("ranked first in %.0f%% of %d replicates; rank interval %g-%g",
                                    100 * boot$top1_frac[k], attr(boot, "n_ok"), boot$rank_lower[k], boot$rank_upper[k])
      hold <- explorer_rank_hold(boot, f)
      if (!is.null(hold)) {
        out[["Rank held"]] <- sprintf("rank %d in %.0f%% of the replays", hold$rank, 100 * hold$share)
      }
    }
  }
  if (!is.null(cmp)) {
    k <- which(cmp$factor == f & cmp$response == r)
    if (length(k)) {
      out[["Snapshot"]] <- sprintf("%.3g, rank %s (now rank %s)", cmp$before[k], cmp$rank_before[k], cmp$rank_after[k])
    }
  }
  out
}

#' Share of bootstrap replays in which a factor keeps its rank
#' @param boot `bootstrap_ranks()` result (with `rank_matrix`).
#' @param f Factor.
#' @return `list(rank, share)` (rank of `f` in the original fit) or `NULL`.
#' @keywords internal
#' @noRd
explorer_rank_hold <- function(boot, f) {
  mat <- attr(boot, "rank_matrix")
  j <- match(f, boot$factor)
  if (is.null(mat) || is.na(j)) return(NULL)
  r0 <- .rank_desc(boot$estimate)[j]
  list(rank = as.integer(r0), share = mean(mat[, j] == r0))
}

.std_label <- function(std) {
  c(none = "no standardisation", colmax_response = "Eq. 5, per-response maximum",
    colmax_factor = "per-factor maximum", residual_sd = "residual SD", rank = "rank within response")[[std]]
}

#' Floating tooltip next to the pointer
#' @param ev Hover event (with `coords_css`).
#' @param facts Output of `explorer_factor_facts()`.
#' @return A Shiny tag or `NULL`.
#' @keywords internal
#' @noRd
explorer_tip_ui <- function(ev, facts) {
  if (is.null(ev) || !length(facts)) return(NULL)
  x <- ev$coords_css$x %||% ev$coords_img$x
  y <- ev$coords_css$y %||% ev$coords_img$y
  if (is.null(x) || is.null(y)) return(NULL)
  shiny::div(class = "herminR-tip", style = sprintf("left:%.0fpx;top:%.0fpx", x + 14, y + 14),
    shiny::strong(facts[[1]]),
    shiny::tags$table(lapply(names(facts)[-1], function(n) shiny::tags$tr(shiny::tags$td(n), shiny::tags$td(facts[[n]])))),
    shiny::div(class = "tip-hint", "Click to highlight this factor on every stage"))
}

#' Plot output with hover and click, and a tooltip slot
#' @param id Output id.
#' @param vh,min,max Height (see `explorer_plot_output()`).
#' @return A Shiny tag.
#' @keywords internal
#' @noRd
explorer_interactive_plot <- function(id, vh = 60, min = 320, max = 620) {
  shiny::div(class = "herminR-plotwrap",
    explorer_plot_output(id, vh = vh, min = min, max = max,
                         hover = shiny::hoverOpts(paste0(id, "_hover"), delay = 120, delayType = "debounce"),
                         click = paste0(id, "_click")),
    shiny::uiOutput(paste0(id, "_tip")))
}

#' Outline the highlighted factor on a heatmap
#' @param p A heatmap from `plot.herminR_mpi()`.
#' @param focus `list(factor, response)` or `NULL`.
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_highlight_heatmap <- function(p, focus) {
  if (is.null(focus) || is.null(p$data$xv)) return(p)
  d <- p$data[p$data$factor == focus$factor, , drop = FALSE]
  if (!nrow(d)) return(p)
  p <- p + ggplot2::geom_tile(data = d, fill = NA, colour = "#E6550D", linewidth = 1.1, inherit.aes = FALSE,
                              ggplot2::aes(x = .data$xv, y = .data$yv))
  dc <- d[d$response %in% focus$response, , drop = FALSE]
  if (nrow(dc)) {
    p <- p + ggplot2::geom_tile(data = dc, fill = NA, colour = "#E6550D", linewidth = 2.2, inherit.aes = FALSE,
                                ggplot2::aes(x = .data$xv, y = .data$yv))
  }
  p
}

#' Outline the highlighted factor on tile plots whose x axis is the factor
#' (norm comparison, selection paths)
#' @param p The plot (its data has a `factor` column mapped to x).
#' @param focus `list(factor, response)` or `NULL`.
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_highlight_tiles <- function(p, focus) {
  if (is.null(focus) || is.null(p$data$factor)) return(p)
  d <- p$data[as.character(p$data$factor) == focus$factor, , drop = FALSE]
  if (!nrow(d)) return(p)
  p + ggplot2::geom_tile(data = d, fill = NA, colour = "#E6550D", linewidth = 1.3)
}

#' Outline the highlighted factor's bars (feasible-region chart)
#' @param p A dodged bar chart whose data has `factor` and `what` columns.
#' @param focus `list(factor, response)` or `NULL`.
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_highlight_bars <- function(p, focus) {
  if (is.null(focus) || is.null(p$data$factor)) return(p)
  d <- p$data[as.character(p$data$factor) == focus$factor, , drop = FALSE]
  if (!nrow(d)) return(p)
  p + ggplot2::geom_col(data = d, ggplot2::aes(group = .data$what), position = ggplot2::position_dodge(width = 0.7),
                        width = 0.7, fill = NA, colour = "#E6550D", linewidth = 1.1)
}

#' Ring the highlighted factor on a point plot (intervals or bootstrap ranks)
#' @param p The plot.
#' @param focus `list(factor, response)` or `NULL`.
#' @param x Name of the x column of the points.
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_highlight_points <- function(p, focus, x) {
  if (is.null(focus)) return(p)
  d <- p$data[as.character(p$data$factor) == focus$factor, , drop = FALSE]
  if (!nrow(d)) return(p)
  p + ggplot2::geom_point(data = d, ggplot2::aes(x = .data[[x]]), shape = 21, size = 6.5, stroke = 1.6,
                          colour = "#E6550D", fill = NA, inherit.aes = TRUE)
}

#' Server part: clicks set the highlight, hovers show details
#' @param input,output,session Shiny objects.
#' @param st Shared state (all stages).
#' @return Nothing useful (called for its side effects).
#' @keywords internal
#' @noRd
explorer_focus_server <- function(input, output, session, st) {
  focus <- st$focus
  factors <- shiny::reactive(st$analysis()$factors)
  opt <- function(expr) tryCatch(expr, error = function(e) NULL)
  boot_ok <- shiny::reactive({
    s <- st$boot_state()
    if (is.null(s) || !is.null(s$error)) NULL else list(value = s$value, response = s$params$response)
  })
  facts <- function(cell) {
    if (is.null(cell)) return(character())
    b <- boot_ok()
    explorer_factor_facts(cell$factor, cell$response, shown = st$shown_mpi(), base = st$base_mpi(),
                          unc = opt(st$unc_mpi()), boot = b$value, boot_response = b$response,
                          cmp = opt(st$compare()), std = input$std, flags = opt(st$int_flags()))
  }
  rank_response <- shiny::reactive(boot_ok()$response)
  cell_of <- function(id, kind) {
    function(ev) {
      if (!is.null(ev) && is.null(ev$domain$discrete_limits)) {
        # Shiny could not read the plot's coordinates: use the recorded panel geometry
        ev <- explorer_geometry_event(ev, st$geom[[id]],
                                      session$clientData[[paste0("output_", id, "_width")]],
                                      session$clientData[[paste0("output_", id, "_height")]])
      }
      explorer_event_cell(ev, kind, factors(), if (kind == "rank") rank_response())
    }
  }
  plots <- list(heatmap = "heatmap", interval_plot = "interval", rank_plot = "rank")
  for (id in names(plots)) local({
    pid <- id
    to_cell <- cell_of(pid, plots[[pid]])
    output[[paste0(pid, "_tip")]] <- shiny::renderUI({
      ev <- input[[paste0(pid, "_hover")]]
      explorer_tip_ui(ev, opt(facts(to_cell(ev))))
    })
    shiny::observeEvent(input[[paste0(pid, "_click")]], {
      cell <- to_cell(input[[paste0(pid, "_click")]])
      if (is.null(cell)) return()
      cur <- focus()
      # a second click on the same cell clears the highlight
      focus(if (identical(cur, cell)) NULL else cell)
    })
  })
  shiny::observeEvent(input$focus_clear, focus(NULL))
  # the highlight survives every re-run and setting change; it is reset only
  # when the set of factors changes (or the factor disappears)
  last_factors <- NULL
  shiny::observe({
    fs <- tryCatch(factors(), error = function(e) NULL)
    if (is.null(fs)) return()
    f <- shiny::isolate(focus())
    changed <- !is.null(last_factors) && !identical(sort(fs), sort(last_factors))
    last_factors <<- fs
    if (!is.null(f) && (changed || !(f$factor %in% fs))) focus(NULL)
  })

  output$focus_bar <- shiny::renderUI({
    f <- focus()
    if (is.null(f)) return(NULL)
    ff <- opt(facts(f))
    shiny::div(class = "focus-bar",
      shiny::span(class = "focus-dot"), shiny::strong(sprintf("Highlighted: %s", f$factor)),
      if (length(ff) > 1) shiny::span(paste(paste(names(ff)[-1], ff[-1]), collapse = " \u00b7 ")),
      shiny::actionLink("focus_clear", "Clear"))
  })

  # the highlight and the snapshot travel with a bookmark link
  shiny::onBookmark(function(state) {
    state$values$focus <- focus()
  })
  shiny::onRestored(function(state) {
    f <- state$values$focus
    if (length(f$factor) == 1) focus(list(factor = as.character(f$factor), response = as.character(f$response)))
  })
  invisible(NULL)
}

#' CSS for tooltips, highlights and the comparison
#' @return A character string.
#' @keywords internal
#' @noRd
explorer_focus_css <- function() {
  ".herminR-plotwrap{position:relative}
   .herminR-tip{position:absolute;z-index:20;pointer-events:none;background:rgba(255,255,255,.97);
     border:1px solid #bbb;border-left:4px solid #E6550D;border-radius:4px;padding:6px 9px;font-size:12.5px;
     box-shadow:0 2px 8px rgba(0,0,0,.15);max-width:340px}
   .herminR-tip table td{padding:1px 8px 1px 0;vertical-align:top}.herminR-tip table td:first-child{color:#666;white-space:nowrap}
   .herminR-tip .tip-hint{color:#999;font-size:11px;margin-top:3px}
   .focus-bar{margin:0 15px 10px;padding:6px 12px;background:#fff4ec;border:1px solid #f3c6a5;border-radius:4px;font-size:13px}
   .focus-bar>*{margin-right:10px}.focus-dot{display:inline-block;width:10px;height:10px;border-radius:50%;
     border:2px solid #E6550D;vertical-align:middle}
   .plot-hint{color:#595959;font-size:85%;margin:2px 0 8px}
   .snap-box{background:#f7f7f7;border-radius:4px;padding:6px 9px;margin:6px 0;font-size:90%}"
}
