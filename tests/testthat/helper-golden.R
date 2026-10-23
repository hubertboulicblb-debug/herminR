# Golden-value capture for the explorer's default outputs.
#
# The same function is run by tools/golden/make-golden.R against herminR
# 0.3.0 (to write tests/testthat/fixtures/golden-030.rds) and by
# test-e2e-03x.R against the current version, so "defaults unchanged" is
# checked against the last release before the interaction features.

golden_inputs <- function(source = "pastrydough") {
  base <- list(
    tabs = "start", source = source, order = "SO", code = TRUE, norm = "l1", signed = "abs", std = "none",
    palette = "ermine", transpose = TRUE, annotate = TRUE, reorder = FALSE, bounds = "ellipsoid",
    level = 0.95, unc_response = ".all", boot_n = 30, boot_type = "residual", boot_seed = 1,
    alphas = "0.05, 0.1", top = 2, others = "centre", constraint = "", feas_method = "exact",
    feas_size = 2000, feas_seed = 1)
  if (source == "pastrydough") {
    utils::modifyList(base, list(
      responses = "y", factors = c("x1", "x2", "x3"), block = "block", rob_response = "y", feas_response = "y",
      keep = c("x1", "x2", "x3"),
      rng_x1_lo = -1, rng_x1_hi = 1, rng_x2_lo = -1, rng_x2_hi = 1, rng_x3_lo = -1, rng_x3_hi = 1,
      fea_x1_lo = -1, fea_x1_hi = 1, fea_x2_lo = -1, fea_x2_hi = 1, fea_x3_lo = -1, fea_x3_hi = 1))
  } else {
    utils::modifyList(base, list(
      responses = c("y1", "y2", "y3", "y4"), rob_response = "y1", feas_response = "y1",
      keep = c("x1", "x2", "x3"),
      rng_x1_lo = -1, rng_x1_hi = 1, rng_x2_lo = -1, rng_x2_hi = 1, rng_x3_lo = -1, rng_x3_hi = 1,
      fea_x1_lo = -1, fea_x1_hi = 1, fea_x2_lo = -1, fea_x2_hi = 1, fea_x3_lo = -1, fea_x3_hi = 1))
  }
}

# Report text without what legitimately changes between runs and versions:
# embedded images, the date and the version lines of the session table.
golden_mask_report <- function(html) {
  html <- gsub("data:image/png;base64,[A-Za-z0-9+/=]+", "data:image/png;base64,IMG", html)
  html <- gsub("(<th>Date</th><td>)[^<]*", "\\1DATE", html)
  html <- gsub("(<th>herminR</th><td>)[^<]*", "\\1VERSION", html)
  html <- gsub("(<th>R</th><td>)[^<]*", "\\1RVERSION", html)
  # section added in 0.5.1 (assumptions and limitations), checked in test-v051.R
  html <- sub("\\n?<h2>Methods and limitations</h2>.*?(<h2>Reproduce in R</h2>)", "\\1", html, perl = TRUE)
  html
}

# What each plot layer shows (positions, fills, labels, shapes), not the
# defaults a given ggplot2 version adds (line widths, nudges, fonts).
golden_layers <- function(p) {
  keep <- c("x", "y", "xmin", "xmax", "ymin", "ymax", "xend", "yend", "fill", "label", "shape", "PANEL", "group")
  lapply(seq_along(p$layers), function(i) {
    d <- as.data.frame(ggplot2::layer_data(p, i))
    d <- d[, intersect(keep, names(d)), drop = FALSE]
    d[] <- lapply(d, function(v) if (is.factor(v)) as.character(v) else v)
    d
  })
}

# Run the explorer server with the default settings and record its outputs.
# `server` is explorer_server of the version under test.
golden_capture <- function(server, source = "pastrydough", run_boot = TRUE) {
  out <- NULL
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  shiny::testServer(server, {
    do.call(session$setInputs, golden_inputs(source))
    if (run_boot && source == "pastrydough") session$setInputs(run_boot = 1)
    safe <- function(expr) tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
    out <<- list(
      shown_mpi = as.data.frame(shown_mpi()),
      mpi_table = output$mpi_table,
      interval_table = safe(output$interval_table),
      rank_table = safe(output$rank_table),
      code = code_text(),
      findings = lapply(1:6, function(k) guide$findings(k)),
      report = golden_mask_report(report_html()),
      heatmap_layers = golden_layers(heatmap_plot()),
      interval_layers = safe(golden_layers(mpi_interval_plot(unc_mpi()))),
      rank_layers = if (run_boot && source == "pastrydough") golden_layers(plot(boot_res())) else NULL
    )
  })
  out
}
