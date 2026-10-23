# Snapshot comparison: does the picture depend on our choices? -----------------------

#' Compare two MPI results cell by cell
#'
#' Ranks are by absolute value within each response (1 = most influential,
#' ties share the smallest rank), so they are comparable even when the two
#' results use different norms or standardisations. When both results carry
#' interval bounds (`lower`, `upper`), they are compared too.
#'
#' @param before,after `herminR_mpi` objects (or data frames with `response`,
#'   `factor`, `estimate` and optionally `lower`, `upper`).
#' @return A tibble: `response`, `factor`, `before`, `after`, `change`,
#'   `rank_before`, `rank_after`, `rank_shift` (positive = moved up), and,
#'   when available, `lower_before`, `upper_before`, `lower_after`,
#'   `upper_after`, `width_change`; attributes `n_unmatched` (cells present
#'   in only one of the two) and `n_absent`.
#' @keywords internal
#' @noRd
explorer_compare <- function(before, after) {
  prep <- function(x) {
    x <- as.data.frame(tibble::as_tibble(x))
    cols <- intersect(c("response", "factor", "estimate", "lower", "upper"), names(x))
    d <- x[, cols, drop = FALSE]
    d$rank <- stats::ave(d$estimate, d$response, FUN = .rank_abs)
    d
  }
  b <- prep(before)
  a <- prep(after)
  m <- merge(b, a, by = c("response", "factor"), suffixes = c("_before", "_after"), sort = FALSE)
  resp_order <- unique(a$response)
  m <- m[order(match(m$response, resp_order), m$rank_after, m$factor), , drop = FALSE]
  # a factor with no term in a response (zero in both) carries no ranking information
  absent <- m$estimate_before == 0 & m$estimate_after == 0
  absent[is.na(absent)] <- FALSE
  m <- m[!absent, , drop = FALSE]
  out <- tibble::tibble(response = m$response, factor = m$factor,
                        before = m$estimate_before, after = m$estimate_after,
                        change = m$estimate_after - m$estimate_before,
                        rank_before = as.integer(m$rank_before), rank_after = as.integer(m$rank_after),
                        rank_shift = as.integer(m$rank_before - m$rank_after))
  if (all(c("lower_before", "upper_before", "lower_after", "upper_after") %in% names(m)) &&
      any(!is.na(m$lower_before)) && any(!is.na(m$lower_after))) {
    out$lower_before <- m$lower_before
    out$upper_before <- m$upper_before
    out$lower_after <- m$lower_after
    out$upper_after <- m$upper_after
    out$width_change <- (m$upper_after - m$lower_after) - (m$upper_before - m$lower_before)
  }
  attr(out, "n_unmatched") <- nrow(a) + nrow(b) - 2L * (nrow(m) + sum(absent))
  attr(out, "n_absent") <- sum(absent)
  out
}

#' Direction of each value change
#' @param cmp Output of `explorer_compare()`.
#' @param tol Changes smaller than this (relative to the largest value) count
#'   as unchanged.
#' @return A character vector: `"up"`, `"down"` or `"none"` (absolute values).
#' @keywords internal
#' @noRd
explorer_change_arrows <- function(cmp, tol = 1e-9) {
  d <- abs(cmp$after) - abs(cmp$before)
  scale <- max(c(abs(cmp$before), abs(cmp$after), 1e-12), na.rm = TRUE)
  ifelse(is.na(d), "none", ifelse(d > tol * scale, "up", ifelse(d < -tol * scale, "down", "none")))
}

#' Rank-change strings such as "x3: 2 -> 1" (arrow character), for moved factors
#' @param cmp Output of `explorer_compare()`.
#' @param response_prefix Prefix each string with the response when there are
#'   several.
#' @return A character vector (empty when no rank changed).
#' @keywords internal
#' @noRd
explorer_rank_strings <- function(cmp, response_prefix = length(unique(cmp$response)) > 1) {
  mv <- cmp[!is.na(cmp$rank_shift) & cmp$rank_shift != 0, , drop = FALSE]
  if (!nrow(mv)) return(character())
  paste0(if (response_prefix) paste0(mv$response, " ") else "", mv$factor, ": ", mv$rank_before, " \u2192 ", mv$rank_after)
}

#' Plain-language summary of a comparison
#' @param cmp Output of `explorer_compare()`.
#' @param before_label,after_label Descriptions of the two settings.
#' @return A character vector of sentences.
#' @keywords internal
#' @noRd
explorer_compare_text <- function(cmp, before_label, after_label) {
  if (!nrow(cmp)) return("The snapshot and the current results share no factor and response: nothing to compare.")
  moved <- cmp[cmp$rank_shift != 0 & !is.na(cmp$rank_shift), , drop = FALSE]
  resp <- unique(cmp$response)
  top_b <- vapply(resp, function(r) paste(sort(cmp$factor[cmp$response == r & cmp$rank_before %in% 1]), collapse = "/"), "")
  top_a <- vapply(resp, function(r) paste(sort(cmp$factor[cmp$response == r & cmp$rank_after %in% 1]), collapse = "/"), "")
  top_changed <- resp[top_b != top_a]
  s1 <- sprintf("Snapshot: %s. Now: %s.", before_label, after_label)
  arrow <- "\u2192"   # pasted, not sprintf()-ed: sprintf() drops it in a non-UTF-8 locale
  s2 <- if (!nrow(moved)) {
    sprintf("No factor changes rank in any of the %d response%s: the ranking does not depend on this choice.",
            length(resp), if (length(resp) > 1) "s" else "")
  } else {
    detail <- if (nrow(moved) <= 4) {
      paste0(": ", paste0(moved$factor, " in ", moved$response, " ", ifelse(moved$rank_shift > 0, "up ", "down "),
                          moved$rank_before, arrow, moved$rank_after, collapse = "; "))
    } else ""
    paste0(sprintf("%d of %d factor ranks change (in %d of %d responses)", nrow(moved), nrow(cmp),
                   length(unique(moved$response)), length(resp)), detail, ".")
  }
  s3 <- if (!length(top_changed)) {
    "The most influential factor is the same in every response."
  } else if (length(top_changed) <= 4) {
    paste0("The most influential factor changes in ",
           paste0(top_changed, " (", top_b[top_changed], " ", arrow, " ", top_a[top_changed], ")", collapse = ", "), ".")
  } else {
    sprintf("The most influential factor changes in %d of %d responses (%s and %d more).", length(top_changed),
            length(resp), paste(utils::head(top_changed, 3), collapse = ", "), length(top_changed) - 3L)
  }
  s_int <- if ("width_change" %in% names(cmp)) {
    w <- cmp$width_change
    tol <- 1e-9 * max(c(abs(cmp$upper_before), abs(cmp$upper_after), 1), na.rm = TRUE)
    nw <- sum(w > tol, na.rm = TRUE); nn <- sum(w < -tol, na.rm = TRUE)
    if (nw + nn == 0) "The intervals are unchanged." else
      paste0("Intervals: wider for ", nw, " and narrower for ", nn, " of ", sum(!is.na(w)), " factor-response pairs.")
  } else NULL
  vals <- explorer_change_arrows(cmp)
  s_val <- if (all(vals == "none")) "The values themselves are unchanged." else NULL
  n_un <- attr(cmp, "n_unmatched")
  n_ab <- attr(cmp, "n_absent")
  c(s1, s2, s3, s_val, s_int, if (isTRUE(n_un > 0)) sprintf("%d cell%s exist only in one of the two and %s left out.",
                                               n_un, if (n_un > 1) "s" else "", if (n_un > 1) "are" else "is"),
    if (isTRUE(n_ab > 0)) sprintf("%d factor-response pair%s with no term (zero in both) %s not ranked.",
                                  n_ab, if (n_ab > 1) "s" else "", if (n_ab > 1) "are" else "is"))
}

#' Rank slope chart: snapshot versus now
#' @param cmp Output of `explorer_compare()`.
#' @param focus Highlighted factor (`list(factor, response)`) or `NULL`.
#' @return A ggplot.
#' @keywords internal
#' @noRd
explorer_compare_plot <- function(cmp, focus = NULL, max_panels = 9L) {
  resp <- unique(cmp$response)
  note <- NULL
  if (length(resp) > max_panels) {
    moves <- vapply(resp, function(r) sum(abs(cmp$rank_shift[cmp$response == r]), na.rm = TRUE), numeric(1))
    keep <- resp[order(-moves)][seq_len(max_panels)]
    resp <- resp[resp %in% keep]
    note <- sprintf("The %d responses with the largest rank changes (of %d); the table lists all.",
                    max_panels, length(unique(cmp$response)))
    cmp <- cmp[cmp$response %in% resp, , drop = FALSE]
  }
  d <- rbind(data.frame(response = cmp$response, factor = cmp$factor, when = "Snapshot", rank = cmp$rank_before,
                        moved = cmp$rank_shift != 0),
             data.frame(response = cmp$response, factor = cmp$factor, when = "Now", rank = cmp$rank_after,
                        moved = cmp$rank_shift != 0))
  d$when <- factor(d$when, levels = c("Snapshot", "Now"))
  d$response <- factor(d$response, levels = resp)
  d$status <- ifelse(d$moved, "rank changed", "same rank")
  if (!is.null(focus)) d$status[d$factor == focus$factor] <- "highlighted"
  d$status <- factor(d$status, levels = c("same rank", "rank changed", "highlighted"))
  lab <- d[d$when == "Now" & d$status != "same rank", , drop = FALSE]
  # nothing moved: name every factor, otherwise the chart has no labels at all
  if (!nrow(lab)) lab <- d[d$when == "Now", , drop = FALSE]
  # tied factors share one label ("Factor_2/Factor_8")
  if (nrow(lab)) {
    key <- paste(lab$response, lab$rank)
    lab$factor <- stats::ave(lab$factor, key, FUN = function(v) paste(v, collapse = "/"))
    lab$status[stats::ave(as.integer(lab$status == "highlighted"), key, FUN = max) == 1] <- "highlighted"
    lab <- lab[!duplicated(key), , drop = FALSE]
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$when, y = .data$rank, group = .data$factor, colour = .data$status)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = .data$factor), hjust = -0.25, size = 3.4, show.legend = FALSE) +
    ggplot2::scale_y_reverse(breaks = function(l) seq(1, max(1, ceiling(max(l))))) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.25, 0.9))) +
    ggplot2::scale_colour_manual(values = c("same rank" = "grey70", "rank changed" = "#3366AA", "highlighted" = "#E6550D"),
                                 drop = TRUE) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::labs(x = NULL, y = "Rank (1 = most influential)", colour = NULL,
                  title = "Ranking: snapshot versus now", subtitle = note,
                  caption = if (any(d$moved)) "Labels: factors whose rank changed." else "No rank changed.")
  if (length(resp) > 1) p <- p + ggplot2::facet_wrap(~response)
  p
}

#' Mark changes since the snapshot on the heatmap
#' @param p Heatmap from `plot.herminR_mpi()`.
#' @param cmp Output of `explorer_compare()` or `NULL`.
#' @param kind `"rank"` (triangles with the number of places moved) or
#'   `"value"` (triangles with the size of the change in |value|, a dash when
#'   unchanged).
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_mark_changes <- function(p, cmp, kind = c("rank", "value")) {
  kind <- match.arg(kind)
  if (is.null(cmp) || is.null(p$data$xv) || !nrow(cmp)) return(p)
  if (kind == "rank") {
    ch <- cmp[!is.na(cmp$rank_shift) & cmp$rank_shift != 0, , drop = FALSE]
    if (!nrow(ch)) return(p)
    ch$dir <- ifelse(ch$rank_shift > 0, "up", "down")
    ch$lab <- as.character(abs(ch$rank_shift))
    note <- "Blue triangles: rank up/down by n since the snapshot."
  } else {
    ch <- cmp
    ch$dir <- explorer_change_arrows(cmp)
    ch$lab <- ifelse(ch$dir == "none", "", formatC(abs(abs(ch$after) - abs(ch$before)), digits = 2, format = "g"))
    note <- "Blue triangles: |value| up/down by the amount shown since the snapshot; dash: unchanged."
  }
  d <- merge(as.data.frame(p$data[, c("response", "factor", "xv", "yv")]), as.data.frame(ch), by = c("response", "factor"))
  if (!nrow(d)) return(p)
  tri <- d[d$dir != "none", , drop = FALSE]
  flat <- d[d$dir == "none", , drop = FALSE]
  tri$up <- tri$dir == "up"
  if (nrow(tri)) {
    p <- p + ggplot2::geom_point(data = tri, ggplot2::aes(x = .data$xv, y = .data$yv, shape = .data$up), inherit.aes = FALSE,
                                 size = 2.3, colour = "#3366AA", fill = "#3366AA", show.legend = FALSE,
                                 position = ggplot2::position_nudge(x = -0.36, y = 0.3)) +
      ggplot2::geom_text(data = tri, ggplot2::aes(x = .data$xv, y = .data$yv, label = .data$lab),
                         inherit.aes = FALSE, size = 3, colour = "#3366AA", fontface = "bold", hjust = 0,
                         position = ggplot2::position_nudge(x = -0.28, y = 0.3)) +
      ggplot2::scale_shape_manual(values = c(`TRUE` = 24, `FALSE` = 25))
  }
  if (nrow(flat) && kind == "value") {
    p <- p + ggplot2::geom_point(data = flat, ggplot2::aes(x = .data$xv, y = .data$yv), inherit.aes = FALSE,
                                 shape = 45, size = 6, colour = "#3366AA",
                                 position = ggplot2::position_nudge(x = -0.36, y = 0.3))
  }
  p + ggplot2::labs(caption = paste(c(p$labels$caption, note), collapse = "\n"))
}

#' Short description of the MPI settings
#' @param norm,signed,std Input values.
#' @return A string.
#' @keywords internal
#' @noRd
explorer_settings_label <- function(norm, signed, std, bounds = NULL, level = NULL) {
  paste0(sprintf("%s, %s, %s", c(l1 = "L1 MPI", l2 = "L2 score", linf = "L-infinity score")[[norm]],
                 if (identical(signed, "abs")) "absolute" else "signed", .std_label(std)),
         if (!is.null(bounds) && !is.null(level)) sprintf(", %s intervals at %s", bounds, level) else "")
}

#' Server part of the snapshot comparison (on the MPI stage)
#' @param input,output,session Shiny objects.
#' @param st Shared state (needs `shown_mpi`, `snapshot`, `focus`).
#' @return A list: `compare` (reactive, `NULL` without snapshot), `compare_text`.
#' @keywords internal
#' @noRd
explorer_compare_server <- function(input, output, session, st) {
  snapshot <- st$snapshot
  has_int <- shiny::reactive(isTRUE(tryCatch(st$capabilities()$intervals, error = function(e) FALSE)))
  current_label <- shiny::reactive(explorer_settings_label(input$norm, input$signed, input$std,
                                                           if (has_int()) input$bounds, if (has_int()) input$level))
  # the cells on screen: shown values plus, when available, the intervals of the Uncertainty stage
  current_cells <- shiny::reactive({
    d <- as.data.frame(tibble::as_tibble(st$shown_mpi())[, c("response", "factor", "estimate")])
    if (has_int()) {
      u <- tryCatch(as.data.frame(tibble::as_tibble(st$unc_mpi())), error = function(e) NULL)
      if (!is.null(u)) {
        k <- match(paste(d$response, d$factor), paste(u$response, u$factor))
        d$lower <- u$lower[k]
        d$upper <- u$upper[k]
      }
    }
    d
  })
  study_key <- shiny::reactive(tryCatch({
    a <- st$analysis()
    paste(input$source, paste(st$responses(), collapse = ","), paste(a$factors, collapse = ","), sep = "|")
  }, error = function(e) NULL))
  shiny::observeEvent(input$snap_take, {
    d <- tryCatch(current_cells(), error = function(e) NULL)
    if (is.null(d)) {
      shiny::showNotification("Nothing to keep yet: complete step 1 first.", type = "warning")
      return()
    }
    snapshot(list(data = d, label = current_label(), time = format(Sys.time(), "%H:%M:%S"), key = study_key()))
  })
  # another study (data set, responses or factors): the snapshot no longer applies
  shiny::observe({
    k <- study_key()
    s <- shiny::isolate(snapshot())
    if (!is.null(k) && !is.null(s) && !is.null(s$key) && !identical(k, s$key)) snapshot(NULL)
  })
  shiny::observeEvent(input$snap_clear, snapshot(NULL))

  compare <- shiny::reactive({
    s <- snapshot()
    if (is.null(s)) return(NULL)
    explorer_compare(s$data, current_cells())
  })
  compare_text <- shiny::reactive({
    s <- snapshot()
    if (is.null(s)) return(NULL)
    explorer_compare_text(compare(), s$label, current_label())
  })
  output$has_snapshot <- shiny::reactive(!is.null(snapshot()))
  shiny::outputOptions(output, "has_snapshot", suspendWhenHidden = FALSE)
  output$snap_info <- shiny::renderUI({
    s <- snapshot()
    if (is.null(s)) {
      return(shiny::p(class = "note", "Keep the current results, then change the aggregation, values or standardisation to see what moves."))
    }
    shiny::div(class = "snap-box", shiny::strong("Snapshot "), sprintf("(%s): %s. ", s$time, s$label),
               shiny::actionLink("snap_clear", "Clear"))
  })
  output$compare_text <- shiny::renderUI(shiny::tags$ul(lapply(compare_text(), shiny::tags$li)))
  output$compare_plot <- shiny::renderPlot({
    cmp <- compare()
    shiny::validate(shiny::need(!is.null(cmp) && nrow(cmp) > 0, "Nothing to compare."))
    explorer_fit_width(explorer_compare_plot(cmp, st$focus()) + explorer_theme(), explorer_width(session, "compare_plot"))
  }, res = 96, alt = "Rank of each factor in the snapshot and now")
  output$compare_table <- shiny::renderTable({
    cmp <- compare()
    shiny::req(!is.null(cmp))   # (req() on a zero-row table would fail silently)
    shiny::validate(shiny::need(nrow(cmp) > 0, "The snapshot and the current results share no factor and response."))
    tab <- data.frame(Response = cmp$response, Factor = cmp$factor, Snapshot = cmp$before, Now = cmp$after,
               Change = cmp$change,
               Rank = paste0(cmp$rank_before, " \u2192 ", cmp$rank_after,
                             ifelse(cmp$rank_shift > 0, "  \u25b2", ifelse(cmp$rank_shift < 0, "  \u25bc", ""))),
               check.names = FALSE)
    if ("width_change" %in% names(cmp)) {
      tab[["Interval (snapshot)"]] <- sprintf("%.3g to %.3g", cmp$lower_before, cmp$upper_before)
      tab[["Interval (now)"]] <- sprintf("%.3g to %.3g", cmp$lower_after, cmp$upper_after)
      tab[["Width change"]] <- cmp$width_change
    }
    tab
  }, digits = 3)

  shiny::onBookmark(function(state) {
    s <- snapshot()
    # columns as plain vectors so the JSON in the link restores unambiguously
    if (!is.null(s)) state$values$snapshot <- list(data = .snapshot_encode(s$data), label = s$label, time = s$time, key = s$key)
  })
  shiny::onRestored(function(state) {
    s <- state$values$snapshot
    if (!is.null(s$data)) {
      snapshot(list(data = .snapshot_decode(s$data), label = s$label, time = s$time, key = s$key))
    }
  })
  list(compare = compare, compare_text = compare_text, study_key = study_key)
}

# Snapshot columns travel through the link's JSON as strings ("NA" for
# missing; full precision for numbers) so missing intervals survive.
.snapshot_encode <- function(d) {
  lapply(d, function(v) if (is.numeric(v)) ifelse(is.na(v), "NA", sprintf("%.17g", v)) else as.character(v))
}
.snapshot_decode <- function(x) {
  d <- as.data.frame(lapply(x, function(v) as.character(unlist(v))), stringsAsFactors = FALSE)
  for (k in setdiff(names(d), c("response", "factor"))) d[[k]] <- suppressWarnings(as.numeric(ifelse(d[[k]] == "NA", NA, d[[k]])))
  d
}
