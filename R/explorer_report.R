# Self-contained HTML report ------------------------------------------------------------
#
# Written without rmarkdown/pandoc so the report works on any installation:
# plots are embedded as base64 PNGs.

#' Base64-encode raw bytes
#' @param x A raw vector.
#' @return A single character string.
#' @keywords internal
#' @noRd
.base64 <- function(x) {
  if (!length(x)) return("")
  tbl <- c(LETTERS, letters, 0:9, "+", "/")
  pad <- (3 - length(x) %% 3) %% 3
  v <- as.integer(c(x, as.raw(rep(0L, pad))))
  m <- matrix(v, nrow = 3)
  idx <- rbind(m[1, ] %/% 4L,
               (m[1, ] %% 4L) * 16L + m[2, ] %/% 16L,
               (m[2, ] %% 16L) * 4L + m[3, ] %/% 64L,
               m[3, ] %% 64L)
  out <- tbl[as.vector(idx) + 1L]
  if (pad) out[(length(out) - pad + 1L):length(out)] <- "="
  paste(out, collapse = "")
}

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

#' A ggplot as an inline <img> tag
#' @param p A ggplot object.
#' @param width,height Size in inches.
#' @return An HTML string (empty if the plot cannot be drawn).
#' @keywords internal
#' @noRd
.plot_img <- function(p, width = 8, height = 5) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  ok <- tryCatch({
    ggplot2::ggsave(f, p, width = width, height = height, dpi = 110)
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(f)) return("")
  sprintf('<img alt="plot" src="data:image/png;base64,%s"/>', .base64(readBin(f, "raw", file.info(f)$size)))
}

#' Build the explorer report as a self-contained HTML string
#'
#' @param info A two-column data frame (`item`, `value`) describing the run.
#' @param sections A list of sections, each a list with `title`, `question`,
#'   `findings` (a named list of character vectors: heading -> sentences) and
#'   `plots` (a list of ggplot objects).
#' @param script The reproducible R script (character string).
#' @param title Report title.
#' @return A single character string of HTML.
#' @keywords internal
#' @noRd
explorer_report_html <- function(info, sections, script, title = "herminR analysis report") {
  css <- "body{font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif;max-width:960px;margin:24px auto;padding:0 16px;color:#222;line-height:1.45}
h1{color:#1f4e79;margin-bottom:4px}h2{color:#1f4e79;border-bottom:1px solid #d9e2ec;padding-bottom:4px;margin-top:32px}
.q{font-style:italic;color:#444}.find{background:#eef6ee;border-radius:4px;padding:6px 14px;margin:8px 0}
.find h4{margin:6px 0;color:#2e7d32}table{border-collapse:collapse;margin:8px 0}td,th{border-bottom:1px solid #e3e3e3;padding:3px 10px;text-align:left}
img{max-width:100%;margin:8px 0}pre{background:#f6f6f6;padding:12px;overflow-x:auto;font-size:85%}.small{color:#666;font-size:85%}"
  sec <- vapply(sections, function(s) {
    finds <- paste(vapply(names(s$findings), function(h) {
      sprintf("<h4>%s</h4><ul>%s</ul>", .html_escape(h),
              paste(sprintf("<li>%s</li>", .html_escape(s$findings[[h]])), collapse = ""))
    }, character(1)), collapse = "")
    plots <- paste(vapply(s$plots, .plot_img, character(1)), collapse = "\n")
    sprintf('<h2>%s</h2><p class="q">%s</p><div class="find">%s</div>%s',
            .html_escape(s$title), .html_escape(s$question), finds, plots)
  }, character(1))
  info_rows <- paste(sprintf("<tr><th>%s</th><td>%s</td></tr>", .html_escape(info$item), .html_escape(info$value)),
                     collapse = "")
  paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>', .html_escape(title),
         '</title><style>', css, '</style></head><body>',
         '<h1>', .html_escape(title), '</h1>',
         '<p class="small">Maximal Possible Influence (MPI): the largest change in a response caused by ',
         'increasing one factor by one coded unit, with the other factors set where they amplify the effect most.</p>',
         '<table>', info_rows, '</table>',
         paste(sec, collapse = "\n"),
         '<h2>Reproduce in R</h2><pre>', .html_escape(script), '</pre>',
         '</body></html>')
}
