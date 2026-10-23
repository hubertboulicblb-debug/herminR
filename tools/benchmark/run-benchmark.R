# Response times and memory of the explorer's actions.
#
# Every action is run through the explorer's own server code (shiny::testServer),
# so the times include what the app does around the computation (fitting,
# formatting, plotting), but not the browser's drawing (see browser_timing.py).
# Plots are rendered to a 900 x 560 PNG as in the app.
#
#   Rscript tools/benchmark/run-benchmark.R
#
# Writes validation/benchmark-<version>.csv and .html.

suppressPackageStartupMessages({ library(herminR); library(shiny) })
ver <- as.character(packageVersion("herminR"))
out_dir <- "validation"
dir.create(out_dir, showWarnings = FALSE)
reps <- 3L

# a larger synthetic study: 3^6 = 729 runs, 6 factors, 4 responses
set.seed(7)
big <- expand.grid(rep(list(c(-1, 0, 1)), 6))
names(big) <- paste0("A", 1:6)
X <- as.matrix(big)
for (r in paste0("R", 1:4)) {
  b <- rnorm(6); q <- rnorm(6, sd = 0.5); I <- matrix(rnorm(36, sd = 0.3), 6) * (runif(36) < 0.5)
  big[[r]] <- drop(X %*% b + (X^2) %*% q + rowSums((X %*% I) * X)) + rnorm(nrow(X), sd = 0.5)
}
big_file <- tempfile(fileext = ".csv")
utils::write.csv(big, big_file, row.names = FALSE)

render <- function(p) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 900, height = 560, res = 96)
  on.exit({ grDevices::dev.off(); unlink(f) })
  print(p)
  invisible(NULL)
}

time_it <- function(expr_fn) {
  invisible(gc(reset = TRUE))
  t <- system.time(expr_fn())[["elapsed"]]
  g <- gc()
  mem <- sum(g[, ncol(g)])                    # "max used" (Mb) since the reset, Ncells + Vcells
  c(seconds = t, max_mb = mem)
}

study_inputs <- function(study) {
  base <- list(tabs = "start", order = "SO", code = TRUE, norm = "l1", signed = "abs", std = "none",
               palette = "ermine", transpose = TRUE, annotate = TRUE, reorder = FALSE, bounds = "ellipsoid",
               level = 0.95, unc_response = ".all", boot_type = "residual", boot_seed = 1, alphas = "0.05, 0.1",
               top = 2, others = "centre", constraint = "", feas_method = "exact", feas_seed = 1,
               mpi_view = "heatmap", flow_scale = TRUE, flow_min = 0)
  switch(study,
    pastry = c(base, list(source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"), block = "block",
                          rob_response = "y", feas_response = "y", view_response = "y")),
    big = c(base, list(source = "upload_data", file = list(name = "big.csv", datapath = big_file),
                       responses = paste0("R", 1:4), factors = paste0("A", 1:6), block = "",
                       rob_response = "R1", feas_response = "R1", view_response = "R1")),
    sim = c(base, list(source = "dss_sim", responses = paste0("Response_", 1:20), rob_response = "Response_1",
                       feas_response = "Response_1", view_response = "Response_1")))
}

# The models, inputs and objects are those the app builds (taken from its server
# code); each computation is then timed by calling it directly, so the numbers
# are what the app computes for one action, without the rendering of other
# outputs. Response times as a user sees them come from browser_timing.py.
data(pastrydough, package = "herminR")
results <- list()
add <- function(study, action, rep, fn) {
  r <- tryCatch(time_it(fn), error = function(e) c(seconds = NA, max_mb = NA))
  results[[length(results) + 1]] <<- data.frame(study = study, action = action, rep = rep,
                                               seconds = r[["seconds"]], max_mb = r[["max_mb"]])
}
for (study in c("pastry", "big", "sim")) {
  objs <- NULL
  testServer(explorer_server, {
    do.call(session$setInputs, study_inputs(study))
    objs <<- list(a = analysis(), input = mpi_input(analysis()), report = report_html, factors = analysis()$factors)
    for (rep in seq_len(reps)) add(study, "HTML report (all stages, plots embedded)", rep, function() report_html())
  })
  a <- objs$a; inp <- objs$input; fs <- objs$factors
  model1 <- inp[[1]]
  m <- suppressWarnings(mpi_multi(inp, abs = TRUE, factors = fs))
  for (rep in seq_len(reps)) {
    if (study != "sim") {
      raw <- if (study == "pastry") pastrydough else big
      blk <- if (study == "pastry") "block" else ""
      add(study, "Fit the models", rep, function() herminR:::.fit_models(raw, names(inp), fs, "SO", block = blk, code = TRUE))
    }
    add(study, "MPI of all factors and responses", rep, function() suppressWarnings(mpi_multi(inp, abs = TRUE, factors = fs)))
    add(study, "MPI heatmap (build + draw)", rep, function() render(plot(m, transpose = TRUE)))
    if (study != "sim") {
      add(study, "Intervals, ellipsoid, all responses", rep, function() suppressWarnings(mpi_multi(inp, bounds = "ellipsoid", abs = TRUE, factors = fs)))
      add(study, "Bootstrap, 200 replicates (one response)", rep, function() bootstrap_ranks(model1, n = 200, seed = rep))
      add(study, "Selection paths (one response)", rep, function() mpi_selection_paths(model1, alphas = c(0.05, 0.1), top = 2))
      add(study, "Feasible region, LHS 5000 samples", rep, function() mpi_feasible(model1, method = "lhs", size = 5000, seed = rep, factors = fs))
      add(study, "Convergence check (12 runs, up to 5000)", rep, function()
        mpi_feasible_convergence(model1, region = stats::setNames(list(c(-0.5, 0.5)), fs[1]), sizes = c(625, 1250, 2500, 5000), seeds = 1:3, factors = fs))
      add(study, "Contour view (build + draw)", rep, function() render(mpi_contour(m, factor = fs[1], response = names(inp)[1])))
    }
    if (study == "big") {
      add(study, "Bootstrap, 1000 replicates (worst case)", rep, function() bootstrap_ranks(model1, n = 1000, seed = rep))
      add(study, "Feasible region, LHS 20000 samples", rep, function() mpi_feasible(model1, method = "lhs", size = 20000, seed = rep, factors = fs))
    }
    add(study, "Feasible region, exact", rep, function() mpi_feasible(model1, region = stats::setNames(list(c(-0.5, 0.5)), fs[1]), factors = fs))
    add(study, "Term Sankey (build + draw)", rep, function() render(mpi_sankey(m, response = names(inp)[1])))
    add(study, "Interaction network (build + draw)", rep, function() render(mpi_network(m, response = names(inp)[1])))
    if (study != "pastry") add(study, "Factors-to-responses Sankey (build + draw)", rep, function() render(mpi_sankey(m, view = "responses")))
  }
  cat("done:", study, "\n")
}
`%||%` <- function(a, b) if (is.null(a)) b else a
res <- do.call(rbind, results)
summ <- do.call(rbind, lapply(split(res, list(res$study, res$action), drop = TRUE), function(d) {
  data.frame(study = d$study[1], action = d$action[1], first_run_s = d$seconds[d$rep == 1],
             median_s = stats::median(d$seconds), max_mb = max(d$max_mb))
}))
labels <- c(pastry = "pastry dough (1 response, 3 factors, 30 runs)",
            big = "synthetic (4 responses, 6 factors, 729 runs)",
            sim = "paper simulation (20 responses, 11 factors, coefficients)")
summ$study <- labels[summ$study]
summ <- summ[order(match(summ$action, unique(res$action)), summ$study), ]
rownames(summ) <- NULL
utils::write.csv(summ, file.path(out_dir, paste0("benchmark-", ver, ".csv")), row.names = FALSE)

fmt <- function(x) ifelse(is.na(x), "error", sprintf("%.2f", x))
rows <- paste0("<tr><td>", summ$action, "</td><td>", summ$study, "</td><td>", fmt(summ$first_run_s), "</td><td>",
               fmt(summ$median_s), "</td><td>", sprintf("%.0f", summ$max_mb), "</td></tr>", collapse = "")
env_line <- sprintf("%s; %s; herminR %s, shiny %s, ggplot2 %s; %d repetitions per action.", R.version.string,
                    R.version$platform, ver, packageVersion("shiny"), packageVersion("ggplot2"), reps)
html <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>herminR ', ver, ' response times</title>',
  '<style>body{font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif;max-width:1000px;margin:24px auto;padding:0 16px}',
  'table{border-collapse:collapse}td,th{border-bottom:1px solid #e3e3e3;padding:3px 9px;text-align:left}</style></head><body>',
  '<h1>herminR ', ver, ': response times of the explorer\'s actions</h1><p>', env_line,
  ' Computation times (R side, including drawing a 900 x 560 plot where stated), in seconds, for the objects the app builds; memory is the most R used during the computation (Mb). The time a user waits in the browser is in the second table.</p>',
  '<table><tr><th>Action</th><th>Data</th><th>First run (s)</th><th>Median (s)</th><th>Max memory (Mb)</th></tr>', rows,
  '</table></body></html>')
bt_file <- file.path(out_dir, paste0("browser-timing-", ver, ".csv"))
if (file.exists(bt_file)) {
  bt <- utils::read.csv(bt_file, stringsAsFactors = FALSE)
  bt_rows <- paste0("<tr><td>", bt$action, "</td><td>", bt$study, "</td><td>", sprintf("%.2f", bt$seconds), "</td></tr>", collapse = "")
  html <- sub("</body></html>", paste0(
    "<h2>Response times in the browser</h2><p>From the action to the page being idle again, all visible outputs redrawn ",
    "(tools/benchmark/browser_timing.py, Chromium, 1440 x 900, app and browser on the same machine).</p>",
    "<table><tr><th>Action</th><th>Data</th><th>Seconds</th></tr>", bt_rows, "</table></body></html>"), html, fixed = TRUE)
}
writeLines(html, file.path(out_dir, paste0("benchmark-", ver, ".html")))
print(summ, digits = 3)
