# Shared explorer helpers ---------------------------------------------------------------

#' What each kind of analysis input supports
#'
#' @param kind `"model"` (fitted models), `"table"` (coefficients with standard
#'   errors) or `"coef"` (coefficients only).
#' @return A named logical list: `intervals`, `bootstrap`, `selection`,
#'   `residual_sd`, `followup`, `feasible`.
#' @keywords internal
#' @noRd
explorer_capabilities <- function(kind) {
  list(intervals = kind %in% c("model", "table"),
       bootstrap = identical(kind, "model"),
       selection = identical(kind, "model"),
       residual_sd = identical(kind, "model"),
       followup = TRUE,
       feasible = TRUE)
}

#' Message for a stage (or part of a stage) the current data cannot support
#' @param text What is needed.
#' @param button_id Id of a "back to data" button, or `NULL`.
#' @return A Shiny tag.
#' @keywords internal
#' @noRd
explorer_unavailable_ui <- function(text, button_id = NULL) {
  shiny::div(class = "unavailable",
    shiny::strong("Not available for this data. "), text,
    if (!is.null(button_id)) shiny::div(style = "margin-top:8px",
      shiny::actionButton(button_id, "\u2190 Go to Data and change the source", class = "btn-sm")))
}

#' Run an expensive computation only on a button click, and remember how
#'
#' The computation runs eagerly in an observer (so the button is always
#' re-enabled), errors are captured, and the parameters used are stored with
#' the result so the UI can show "last run" details and detect stale results.
#'
#' @param input,session Shiny objects.
#' @param id Button id.
#' @param params A function returning the current parameters (a list).
#' @param compute A function computing the result from the parameters.
#' @return A `reactiveVal` holding `NULL` or a list with `value`, `error`,
#'   `params`, `time`, `runs`.
#' @keywords internal
#' @noRd
explorer_run_on_click <- function(input, session, id, params, compute) {
  rv <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input[[id]], {
    on.exit(session$sendCustomMessage("herminR-enable", id), add = TRUE)
    t0 <- Sys.time()
    p <- tryCatch(params(), error = function(e) NULL)
    res <- if (is.null(p)) {
      list(value = NULL, error = "Complete the data and model choices first.")
    } else {
      tryCatch(list(value = compute(p), error = NULL),
               error = function(e) list(value = NULL, error = conditionMessage(e)))
    }
    prev <- shiny::isolate(rv())
    rv(list(value = res$value, error = res$error, params = p, time = Sys.time(),
            seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
            runs = (if (is.null(prev)) 0L else prev$runs) + 1L))
  })
  rv
}

#' Result of a run-on-click computation, with friendly messages
#' @param state The `reactiveVal` from `explorer_run_on_click()` (its value).
#' @param press Message when it has not been run yet.
#' @return The computed value (or a validation error).
#' @keywords internal
#' @noRd
explorer_run_value <- function(state, press) {
  shiny::validate(shiny::need(!is.null(state), press))
  shiny::validate(shiny::need(is.null(state$error), state$error))
  state$value
}

#' "Last run" line, with a warning when the settings changed since
#' @param state Value of the run-on-click `reactiveVal`.
#' @param current Current parameters (list) or `NULL`.
#' @param describe Function turning parameters into a short description.
#' @param button_label Label of the button, for the stale message.
#' @return A Shiny tag or `NULL`.
#' @keywords internal
#' @noRd
explorer_last_run_ui <- function(state, current, describe, button_label) {
  if (is.null(state) || is.null(state$params)) return(NULL)
  stale <- !is.null(current) && !identical(current, state$params)
  shiny::div(class = if (stale) "last-run stale" else "last-run",
    sprintf("Last run %s%s: %s.", format(state$time, "%H:%M:%S"),
            if (!is.null(state$seconds)) sprintf(" (took %s)", explorer_duration(state$seconds)) else "",
            describe(state$params)),
    if (!is.null(state$error)) shiny::span(" It failed: ", state$error),
    if (stale) shiny::strong(sprintf(" Settings changed since - press '%s' to update.", button_label)))
}

#' Human-readable duration
#' @param sec Seconds.
#' @return A string such as "0.4 s" or "1 min 12 s".
#' @keywords internal
#' @noRd
explorer_duration <- function(sec) {
  if (!is.finite(sec)) return("?")
  if (sec < 0.1) return("< 0.1 s")
  if (sec < 60) sprintf("%.1f s", sec) else sprintf("%d min %d s", as.integer(sec %/% 60), as.integer(round(sec %% 60)))
}

#' A plot output that follows the window height
#'
#' The height is a share of the window (`vh`), kept between `min` and `max`
#' pixels, so plots fit small screens without becoming unreadable on large
#' ones.
#' @param id Output id.
#' @param vh Height as a percentage of the window height.
#' @param min,max Bounds in pixels.
#' @param ... Passed to [shiny::plotOutput()] (e.g. `click`, `hover`).
#' @return A Shiny tag.
#' @keywords internal
#' @noRd
explorer_plot_output <- function(id, vh = 60, min = 320, max = 620, ...) {
  out <- shiny::plotOutput(id, height = paste0(vh, "vh"), ...)
  shiny::tagAppendAttributes(out, style = sprintf("min-height:%dpx;max-height:%dpx;", min, max))
}

#' A collapsed section (tables and diagnostics behind the main plot)
#' @param label Summary text.
#' @param ... Content.
#' @param open Start open?
#' @return A Shiny tag.
#' @keywords internal
#' @noRd
explorer_details <- function(label, ..., open = FALSE) {
  shiny::tags$details(class = "more", open = if (open) NA else NULL, shiny::tags$summary(label), ...)
}

#' Adapt a plot to the width it is drawn at
#'
#' Wraps titles, subtitles and captions to the plot width, and on narrow
#' screens (below 600 px) moves legends under the plot and uses smaller text,
#' so nothing is cut off on a phone.
#' @param p A ggplot.
#' @param width Rendered width in CSS pixels (`NULL`: unchanged).
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_fit_width <- function(p, width) {
  if (is.null(width) || !is.finite(width) || width <= 0) return(p)
  wrap <- function(x, chars) {
    if (is.null(x) || !is.character(x) || !length(x)) return(x)
    paste(strwrap(paste(gsub("\n", " ", x), collapse = " "), width = max(20, chars)), collapse = "\n")
  }
  narrow <- width < 600
  p$labels$title <- wrap(p$labels$title, floor(width / if (narrow) 10 else 11))
  p$labels$subtitle <- wrap(p$labels$subtitle, floor(width / if (narrow) 7.5 else 8.5))
  p$labels$caption <- wrap(p$labels$caption, floor(width / 8.5))
  if (narrow) {
    p <- p + ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                            legend.key.width = ggplot2::unit(0.9, "cm"),
                            text = ggplot2::element_text(size = 11),
                            plot.title = ggplot2::element_text(size = 13),
                            plot.subtitle = ggplot2::element_text(size = 10),
                            plot.title.position = "plot", plot.caption.position = "plot")
  }
  p
}

#' Keep a heatmap readable at its drawn width
#'
#' When the cells become too narrow for their printed values (less than
#' about 28 px), the values are dropped (the values table has them) and the
#' column labels are turned upright and made smaller.
#' @param p A heatmap from `plot.herminR_mpi()`.
#' @param width Rendered width in CSS pixels (`NULL`: unchanged).
#' @return The plot.
#' @keywords internal
#' @noRd
explorer_fit_heatmap <- function(p, width) {
  if (is.null(width) || !is.finite(width) || is.null(p$data$xv)) return(p)
  nx <- length(unique(p$data$xv))
  cell <- (width * 0.72 - 90) / max(1, nx)
  if (cell < 28) {
    is_text <- vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))
    if (any(is_text)) {
      p$layers <- p$layers[!is_text]
      # the text colour scale has nothing left to colour (ggplot2 >= 4 warns about it)
      p$scales$scales <- Filter(function(sc) !any(c("colour", "color") %in% sc$aesthetics), p$scales$scales)
      p <- p + ggplot2::labs(caption = paste(c(p$labels$caption, "Values hidden at this width: open the values table below."),
                                             collapse = "\n"))
    }
  }
  if (cell < 40) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5,
                                                                size = if (cell < 20) 7 else 9))
  }
  p
}

#' Width of a plot output, as reported by the browser
#' @param session Shiny session.
#' @param id Output id.
#' @return Width in CSS pixels, or `NULL`.
#' @keywords internal
#' @noRd
explorer_width <- function(session, id) {
  w <- tryCatch(session$clientData[[paste0("output_", id, "_width")]], error = function(e) NULL)
  if (is.numeric(w) && length(w) == 1 && is.finite(w)) w else NULL
}

#' Theme used for plots inside the app (larger text)
#' @return A ggplot2 theme.
#' @keywords internal
#' @noRd
explorer_theme <- function() {
  ggplot2::theme(text = ggplot2::element_text(size = 14), axis.text = ggplot2::element_text(size = 12))
}

#' Theme for diagrams without axes (Sankey, network) inside the app
#' @return A ggplot2 theme.
#' @keywords internal
#' @noRd
explorer_theme_diagram <- function() {
  ggplot2::theme(text = ggplot2::element_text(size = 13), axis.text.y = ggplot2::element_blank())
}

#' JavaScript and CSS shared by the explorer
#' @return A `shiny::tags$head` element.
#' @keywords internal
#' @noRd
explorer_head <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(guide_css(), explorer_focus_css(),
      ".note{color:#555;font-size:90%;margin:4px 0 10px}.warn{color:#8a5a00}
       .tag{display:inline-block;padding:1px 6px;border-radius:3px;background:#eee;font-size:85%}
       .tag.exact{background:#e3f2e3;color:#1b5e20}.tag.approx{background:#fff3cd;color:#7a5800}
       details.adv-settings>summary{cursor:pointer;font-weight:bold;margin:6px 0}
       details.adv-settings[open]{border-top:1px solid #ddd;padding-top:6px}
       .unavailable{background:#f4f4f4;border:1px dashed #bbb;border-radius:4px;padding:10px 14px;margin:8px 0 14px;color:#444}
       .last-run{font-size:88%;color:#555;margin:6px 0}.last-run.stale{color:#8a5a00}
       details.more{margin:6px 0 12px}details.more>summary{cursor:pointer;color:#1f4e79;font-size:92%}
       details.more>summary:focus-visible,.guide-pill:focus-visible,a:focus-visible,button:focus-visible{outline:3px solid #E6550D;outline-offset:2px}
       .stage-question{font-weight:600;color:#1f4e79;margin:4px 0 6px}
       .unreliable{background:#fff4e5;border-left:4px solid #B35900;padding:8px 12px;margin:6px 0 10px;color:#5c3300}
       .unreliable ul{margin:4px 0 4px 18px;padding:0}
       .verdict{display:inline-block;padding:1px 8px;border-radius:3px;font-weight:bold;font-size:88%}
       .verdict.ok{background:#e3f2e3;color:#1b5e20}.verdict.no{background:#fff4e5;color:#7a3d00}.verdict.na{background:#eee;color:#444}
       button.running{opacity:.6;cursor:progress}
       .view-controls{display:flex;gap:24px;align-items:flex-end;flex-wrap:wrap}
       .guide-pill.unavailable{opacity:.5}.guide-pill.limited .dot{border-style:dashed}")),
    shiny::tags$script(shiny::HTML(
      "$(document).on('click', 'button.herminR-run', function(){
         var b = $(this);
         if (!b.data('label')) b.data('label', b.html());
         var t0 = Date.now();
         setTimeout(function(){
           b.prop('disabled', true).addClass('running').attr('aria-busy', 'true');
           var tick = function(){ b.html('Running\u2026 ' + Math.round((Date.now() - t0) / 1000) + ' s'); };
           tick(); b.data('timer', setInterval(tick, 1000));
         }, 0);
       });
       $(document).on('shiny:connected', function(){
         Shiny.addCustomMessageHandler('herminR-enable', function(id){
           var b = $('#' + id);
           clearInterval(b.data('timer'));
           if (b.data('label')) b.html(b.data('label'));
           b.prop('disabled', false).removeClass('running').removeAttr('aria-busy');
         });
       });"))
  )
}
