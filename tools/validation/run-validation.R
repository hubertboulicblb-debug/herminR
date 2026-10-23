# Release validation report for herminR.
#
# Produces validation/validation-report-<version>.html (self-contained) and
# validation/validation-results-<version>.rds with every number in it.
# Run from the package root, with herminR installed:
#   Rscript tools/validation/run-validation.R            # full (about 5-10 min)
#   Rscript tools/validation/run-validation.R --quick    # smaller simulations
#
# Sections: environment; reproduction of the paper's 1,520 heatmap cells;
# MPI against brute force on random models; interval coverage by simulation;
# bootstrap rank stability in known scenarios; feasible-region convergence;
# contour step against predict(); package-check and test-suite evidence.

suppressPackageStartupMessages({ library(herminR); library(ggplot2) })
args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args
ver <- as.character(packageVersion("herminR"))
out_dir <- "validation"
dir.create(out_dir, showWarnings = FALSE)
set.seed(20261022)
t_start <- Sys.time()
res <- list()
esc <- herminR:::.html_escape
img <- function(p, w = 8, h = 4.5) herminR:::.plot_img(p, width = w, height = h)
tab <- function(d, digits = 4) {
  d <- as.data.frame(d)
  cells <- lapply(d, function(v) if (is.numeric(v)) formatC(v, digits = digits, format = "g") else esc(as.character(v)))
  paste0("<table><tr>", paste0("<th>", esc(names(d)), "</th>", collapse = ""), "</tr>",
         paste0("<tr>", do.call(paste0, lapply(cells, function(v) paste0("<td>", v, "</td>"))), "</tr>", collapse = ""),
         "</table>")
}
verdict <- function(ok) if (isTRUE(ok)) '<span class="pass">PASS</span>' else '<span class="fail">FAIL</span>'
sections <- list()
add <- function(title, question, verdict_ok, body) {
  sections[[length(sections) + 1]] <<- list(title = title, question = question, ok = verdict_ok, body = body)
  cat(sprintf("[%s] %s\n", if (isTRUE(verdict_ok)) "PASS" else "FAIL", title))
}

# 1. Environment -------------------------------------------------------------------
pk <- c("herminR", "ggplot2", "shiny", "rsm", "testthat", "tibble", "dplyr", "rlang")
env <- data.frame(item = c("Date", "R", "Platform", pk),
                  value = c(format(Sys.time(), "%Y-%m-%d %H:%M %Z"), R.version.string, R.version$platform,
                            vapply(pk, function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "not installed"), "")))
res$environment <- env

# 2. Paper reproduction ---------------------------------------------------------------
ref <- utils::read.csv(system.file("extdata", "paper_reference.csv", package = "herminR"), stringsAsFactors = FALSE)
data(pastry_table1, package = "herminR"); data(dss_sim, package = "herminR")
studies <- list(
  pastry = list(cf = pastry_table1, f = paste0("x", 1:3), keep = c("x1", "x2"), rng = list(x3 = c(-0.1, 0.1))),
  sim = list(cf = dss_sim$coefs, f = paste0("Factor_", 1:11), keep = paste0("Factor_", c(1, 2, 4, 5, 6, 11)),
             rng = list(Factor_3 = c(-0.1, 0.1))))
repro <- do.call(rbind, lapply(names(studies), function(stn) {
  x <- studies[[stn]]
  s_base <- mpi_multi(x$cf, factors = x$f); a_base <- mpi_multi(x$cf, factors = x$f, abs = TRUE)
  only <- function(keep_term) lapply(x$cf, function(b) b[vapply(names(b), keep_term, logical(1))])
  mine <- list(main = mpi_multi(only(function(t) !grepl("[:^]", t)), factors = x$f),
               main_quad = mpi_multi(only(function(t) !grepl(":", t)), factors = x$f),
               all = s_base, all_perResp = standardize_mpi(s_base, "colmax"),
               abs_perResp = standardize_mpi(a_base, "colmax"),
               subset = standardize_mpi(mpi_subset(a_base, keep = x$keep), "colmax"),
               range = standardize_mpi(mpi_adjust_range(a_base, x$rng), "colmax"))
  do.call(rbind, lapply(names(mine), function(fg) {
    r <- ref[ref$study == stn & ref$figure == fg, ]
    m <- mine[[fg]]
    got <- m$estimate[match(paste(r$response, r$factor), paste(m$response, m$factor))]
    data.frame(study = stn, figure = fg, cells = nrow(r), missing = sum(is.na(got)),
               max_abs_diff = max(abs(got - r$expected), na.rm = TRUE))
  }))
}))
res$reproduction <- repro
ok_repro <- sum(repro$cells) == 1520 && all(repro$missing == 0) && max(repro$max_abs_diff) < 1e-10
add("Reproduction of the paper's heatmaps",
    "Does herminR give the values of the paper's own code for all 14 supplement heatmaps?", ok_repro,
    paste0("<p>", sum(repro$cells), " cells compared with the output of the supplement's <code>heatmapVisualization()</code>; ",
           "largest absolute difference ", formatC(max(repro$max_abs_diff), format = "e", digits = 1), ".</p>", tab(repro)))

# 3. MPI against brute force ----------------------------------------------------------
n_models <- if (quick) 60 else 300
rand_model <- function(p) {
  fs <- paste0("x", seq_len(p))
  b <- c(stats::setNames(rnorm(p), fs), stats::setNames(rnorm(p, sd = 0.6), paste0(fs, "^2")))
  pr <- utils::combn(fs, 2)
  inter <- stats::setNames(rnorm(ncol(pr), sd = 0.5) * (runif(ncol(pr)) < 0.7), paste(pr[1, ], pr[2, ], sep = ":"))
  c(b, inter)
}
surface <- function(b, X) {
  v <- numeric(nrow(X))
  for (t in names(b)) {
    if (grepl(":", t)) { u <- strsplit(t, ":")[[1]]; v <- v + b[[t]] * X[, u[1]] * X[, u[2]] }
    else if (grepl("\\^2", t)) { u <- sub("\\^2", "", t); v <- v + b[[t]] * X[, u]^2 }
    else v <- v + b[[t]] * X[, t]
  }
  v
}
brute <- do.call(rbind, lapply(seq_len(n_models), function(i) {
  p <- sample(2:5, 1)
  b <- rand_model(p)
  m <- mpi(b, abs = TRUE)
  fs <- paste0("x", seq_len(p))
  do.call(rbind, lapply(fs, function(f) {
    others <- setdiff(fs, f)
    grid <- expand.grid(c(list(seq(-1, 0, by = 0.05)), rep(list(c(-1, -0.5, 0, 0.5, 1)), length(others))))
    names(grid) <- c(f, others)
    X0 <- as.matrix(grid); X1 <- X0; X1[, f] <- X1[, f] + 1
    bf <- max(abs(surface(b, X1) - surface(b, X0)))
    data.frame(model = i, factors = p, factor = f, mpi = m$estimate[m$factor == f], brute = bf)
  }))
}))
brute$diff <- brute$mpi - brute$brute
res$brute_force <- brute
ok_brute <- max(abs(brute$diff)) < 1e-10
add("MPI against brute force",
    "Is MPI the largest one-unit change of the fitted surface, as defined?", ok_brute,
    paste0("<p>", n_models, " random second-order models (2-5 factors, random main, quadratic and interaction terms), ",
           nrow(brute), " factor values. The brute force evaluates the polynomial over a grid of starting points ",
           "(the factor from -1 to 0 in steps of 0.05, the others at -1, -0.5, 0, 0.5, 1) and takes the largest one-unit change. ",
           "Largest |MPI - brute force| = ", formatC(max(abs(brute$diff)), format = "e", digits = 1), ".</p>"))

# 4. Interval coverage --------------------------------------------------------------
n_rep <- if (quick) 150 else 600
d0 <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), x3 = c(-1, 0, 1))
fml <- y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2)
scen <- list(
  `Clear effect (MPI 1.5)` = list(b = c(x1 = 0.6, x1sq = 0.4, x12 = -0.5), sd = 0.15),
  `Small effect (MPI 0.3)` = list(b = c(x1 = 0.15, x1sq = 0.1, x12 = -0.05), sd = 0.15),
  `One term near zero (MPI 0.62)` = list(b = c(x1 = 0.6, x1sq = 0.02, x12 = 0), sd = 0.15))
wilson <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n
  c <- (p + z^2 / (2 * n)) / (1 + z^2 / n); h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(c - h, c + h)
}
cov <- do.call(rbind, lapply(names(scen), function(sn) {
  s <- scen[[sn]]
  true <- sum(abs(s$b))
  mu <- with(d0, s$b[["x1"]] * x1 + s$b[["x1sq"]] * x1^2 + s$b[["x12"]] * x1 * x2 + 0.3 * x2 + 0.2 * x3)
  hits <- matrix(FALSE, n_rep, 3, dimnames = list(NULL, c("delta", "ellipsoid", "se")))
  for (i in seq_len(n_rep)) {
    d <- d0; d$y <- mu + rnorm(nrow(d), sd = s$sd)
    m <- lm(fml, data = d)
    for (meth in colnames(hits)) {
      r <- suppressWarnings(switch(meth, delta = mpi_delta(m), ellipsoid = mpi_ellipsoid(m), se = mpi_se(m)))
      k <- which(r$factor == "x1")
      hits[i, meth] <- !is.na(r$lower[k]) && r$lower[k] <= true && true <= r$upper[k]
    }
  }
  do.call(rbind, lapply(colnames(hits), function(meth) {
    w <- wilson(sum(hits[, meth]), n_rep)
    data.frame(scenario = sn, method = meth, true_mpi = true, coverage = mean(hits[, meth]), wilson_low = w[1], wilson_high = w[2])
  }))
}))
res$coverage <- cov
# expectations: ellipsoid not significantly below 95%; delta near nominal for the clear effect
ok_cov <- all(cov$wilson_high[cov$method == "ellipsoid"] >= 0.95) &&
  with(cov[cov$method == "delta" & grepl("Clear", cov$scenario), ], wilson_high >= 0.93 && wilson_low <= 0.97)
p_cov <- ggplot(cov, aes(x = method, y = coverage, ymin = wilson_low, ymax = wilson_high)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", colour = "#E6550D") +
  geom_pointrange() + facet_wrap(~scenario) + coord_cartesian(ylim = c(min(0.5, min(cov$wilson_low)), 1)) +
  theme_minimal() + labs(y = "Empirical coverage (95% Wilson interval)", x = NULL,
                         title = "Coverage of 95% MPI intervals for x1",
                         subtitle = sprintf("%d simulated experiments per scenario (3^3 design, second-order model)", n_rep))
add("Interval coverage by simulation",
    "Do the intervals cover the true MPI as often as they claim?", ok_cov,
    paste0("<p>Expectations: the ellipsoid interval is conservative (not significantly below 95%); the delta method is close to 95% ",
           "when the effect is clearly away from zero; the SE soft-threshold has no coverage guarantee and is shown for information.</p>",
           "<p>Observed for the delta method: ", paste(sprintf("%s %.1f%%", cov$scenario[cov$method == "delta"],
                                                        100 * cov$coverage[cov$method == "delta"]), collapse = "; "),
           ". Coverage below nominal when a term is close to zero is the known weakness of the delta method, ",
           "and the explorer says so next to the intervals.</p>",
           img(p_cov, 9, 3.8), tab(cov, 3)))

# 5. Bootstrap rank stability -------------------------------------------------------
n_boot <- if (quick) 100 else 300
mk <- function(b2) {
  d <- d0; d$y <- with(d, 1.2 * x1 + b2 * x2 + 0.1 * x3) + rnorm(nrow(d), sd = 0.2)
  lm(y ~ x1 + x2 + x3, data = d)
}
clear <- bootstrap_ranks(mk(0.3), n = n_boot, seed = 1)
tie <- bootstrap_ranks(mk(1.2), n = n_boot, seed = 1)
rb <- rbind(data.frame(scenario = "clear leader (1.2 vs 0.3)", as.data.frame(clear)[, c("factor", "top1_frac", "rank_lower", "rank_upper")]),
            data.frame(scenario = "near tie (1.2 vs 1.2)", as.data.frame(tie)[, c("factor", "top1_frac", "rank_lower", "rank_upper")]))
res$bootstrap <- rb
tie12 <- tie[tie$factor %in% c("x1", "x2"), ]
ok_boot <- clear$top1_frac[clear$factor == "x1"] > 0.95 && clear$rank_upper[clear$factor == "x1"] == 1 &&
  all(tie12$top1_frac < 0.95) && all(tie12$rank_lower == 1 & tie12$rank_upper == 2)
add("Bootstrap rank stability in known scenarios",
    "Does the bootstrap report a stable leader when there is one, and an unstable ranking for a near tie?", ok_boot,
    paste0("<p>", n_boot, " residual-bootstrap replays each. Expected: with a clear leader, x1 first in (almost) every replay ",
           "and a rank interval of 1-1; with equal true effects, neither x1 nor x2 first in more than 95% of replays and both ",
           "rank intervals spanning 1-2. (The bootstrap varies the data around the <em>fitted</em> model, so a true tie shows up ",
           "as an unstable ranking, not as an exact 50/50 split.)</p>", tab(rb, 3)))

# 6. Feasible-region convergence ------------------------------------------------------
fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
sizes <- if (quick) c(250, 1000, 4000) else c(250, 1000, 4000, 16000, 64000)
cv <- mpi_feasible_convergence(fit, region = list(x3 = c(-0.5, 0.5)), sizes = sizes, seeds = 1:5)
cv_c <- mpi_feasible_convergence(fit, constraint = function(d) d$x1 + d$x2 <= 0, sizes = sizes, seeds = 1:5)
res$convergence <- list(box = cv, constraint = cv_c)
ok_conv <- all(cv$estimate <= cv$exact + 1e-12) && all(diff(tapply(cv$exact - cv$estimate, cv$size, max)) <= 1e-12)
add("Feasible-region sampling converges from below",
    "Is a sampled feasible-region MPI a lower estimate that approaches the exact value as samples increase?", ok_conv,
    paste0("<p>Box region x3 in [-0.5, 0.5] (exact value known), LHS, ", length(sizes), " sizes x 5 seeds. ",
           "Every sampled value is at or below the exact value, and the worst gap shrinks with the sample size.</p>",
           img(plot(cv), 9, 3.4), tab(attr(cv, "summary"), 3),
           "<p>Constraint x1 + x2 &lt;= 0 (no exact value): look for a flat median.</p>",
           img(plot(cv_c), 9, 3.4), tab(attr(cv_c, "summary"), 3)))

# 7. Contour step against predict() -----------------------------------------------------
m <- mpi(fit)
ct <- do.call(rbind, lapply(c("x1", "x2", "x3"), function(f) {
  s <- attr(mpi_contour(m, f), "mpi_step")
  nd <- as.data.frame(rbind(s$start, s$end))
  nd$block <- factor(levels(factor(pastrydough$block))[1], levels = levels(factor(pastrydough$block)))
  pr <- predict(fit, newdata = nd)
  data.frame(factor = f, mpi = abs(m$estimate[m$factor == f]), step_change = s$change, predict_change = unname(pr[2] - pr[1]))
}))
d <- mpi_multi(dss_sim$coefs); tms <- attr(d, "term_maps")
mism <- 0; n_cells <- 0
for (r in names(tms)) for (f in tms[[r]]$factors) {
  st <- tryCatch(herminR:::.mpi_step(tms[[r]], f), error = function(e) NULL)
  if (!is.null(st)) {
    n_cells <- n_cells + 1
    if (abs(abs(st$change) - abs(d$estimate[d$response == r & d$factor == f])) > 1e-9) mism <- mism + 1
  }
}
res$contour <- list(pastry = ct, sim_cells = n_cells, sim_mismatches = mism)
ok_ct <- max(abs(abs(ct$step_change) - ct$mpi)) < 1e-10 && max(abs(ct$step_change - ct$predict_change)) < 1e-10 && mism == 0
add("Contour view: the drawn step is the MPI",
    "Is the arrow in the contour view the one-unit change that defines MPI?", ok_ct,
    paste0("<p>Pastry fit: the change along the arrow equals the MPI and the change predicted by <code>predict()</code>. ",
           "Paper simulation: ", n_cells, " factor-response cells, ", mism, " mismatches.</p>", tab(ct, 6)))

# 8a. Edge cases: unusable data and unsafe constraints are refused -------------------
g3 <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), x3 = c(-1, 0, 1))
ec <- list()
chk <- function(name, d, order = "SO") {
  r <- herminR:::explorer_check_data(d, "y", c("x1", "x2", "x3"), "", order)
  if (!length(r$errors)) {
    m <- suppressWarnings(lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d))
    r2 <- herminR:::explorer_check_fit(list(y = m), order)
    r$errors <- c(r$errors, r2$errors); r$notes <- c(r$notes, r2$notes)
  }
  ec[[length(ec) + 1]] <<- data.frame(case = name, outcome = if (length(r$errors)) "refused" else if (length(r$notes)) "noted" else "accepted",
                                      message = substr(paste(c(r$errors, r$notes), collapse = " | "), 1, 160))
}
d <- g3; d$y <- rnorm(27); chk("valid 3^3 design", d)
d <- g3; d$y <- rnorm(27); d$x3 <- rep(c("lo", "mid", "hi"), 9); chk("text in a factor", d)
d <- g3; d$y <- rep(c("good", "bad", "ok"), 9); chk("text in the response", d)
d <- g3; d$y <- rnorm(27); d$x3 <- 5; chk("factor with a single value", d)
d <- g3[c(1, 5, 9, 11, 15, 19, 23, 27), ]; d$y <- rnorm(8); chk("8 runs for a 10-term model", d)
d <- g3; d$y <- rnorm(27); d$y[c(2, 5)] <- NA; chk("missing responses", d)
d <- g3; d$x3 <- ifelse(d$x3 > 0, 1, -1); d$y <- rnorm(27); chk("two-level factor, second-order model", d)
cons <- c("x1 + x2 <= 0", "system('ls')", "x1 +", "x9 > 0", "rep(TRUE, 3)")
for (cn in cons) {
  r <- tryCatch({ herminR:::.safe_constraint(cn, c("x1", "x2", "x3")); "accepted" }, error = function(e) paste("refused:", conditionMessage(e)))
  ec[[length(ec) + 1]] <- data.frame(case = paste("constraint", cn), outcome = sub(":.*", "", r), message = substr(sub("^refused: ", "", r), 1, 160))
}
ec <- do.call(rbind, ec)
res$edge_cases <- ec
expected <- c("accepted", "refused", "refused", "refused", "refused", "noted", "noted", "accepted", "refused", "refused", "refused", "refused")
ok_edge <- identical(ec$outcome, expected)
add("Edge cases: unusable data and unsafe constraints",
    "Are unusable data and unsafe constraints refused with a plain explanation, and are data problems reported?", ok_edge,
    paste0("<p>The data checks run before any model is fitted; constraints are parsed and only arithmetic and comparisons on ",
           "factor names are allowed.</p>", tab(ec)))

# 8b. Response times ----------------------------------------------------------------------
bench <- file.path(out_dir, paste0("benchmark-", ver, ".csv"))
btime <- file.path(out_dir, paste0("browser-timing-", ver, ".csv"))
if (file.exists(btime)) {
  bt <- utils::read.csv(btime, stringsAsFactors = FALSE)
  slow <- bt[bt$seconds > 5, , drop = FALSE]
  res$browser_timing <- bt
  ok_time <- !nrow(slow)
  add("Response times in the browser",
      "Does every action respond within 5 seconds on the reference data sets, including a 729-run, 4-response study?", ok_time,
      paste0("<p>Slowest action: ", bt$action[which.max(bt$seconds)], " (", bt$study[which.max(bt$seconds)], "), ",
             sprintf("%.2f", max(bt$seconds)), " s. Full tables, including computation times and memory, in <code>benchmark-",
             ver, ".html</code>.</p>", tab(bt, 3)))
} else {
  add("Response times in the browser", "Were the response times measured for this release?", FALSE,
      "<p>No browser timing file: run tools/benchmark/browser_timing.py.</p>")
}

# 8. Package checks and tests ---------------------------------------------------------
logs <- list.files(out_dir, pattern = paste0("^(R-CMD-check|full-tests).*", gsub("\\.", "\\\\.", ver), ".*\\.log$"), full.names = TRUE)
ev <- do.call(rbind, lapply(logs, function(f) {
  l <- readLines(f, warn = FALSE)
  line <- c(grep("^Status:", l, value = TRUE), grep("^files: ", l, value = TRUE))
  data.frame(log = basename(f), result = if (length(line)) line[length(line)] else "(no summary line)")
}))
if (is.null(ev)) ev <- data.frame(log = "(none found)", result = "run tools/run-all-tests.R and R CMD check first")
res$evidence <- ev
ok_ev <- nrow(ev) > 0 && all(grepl("Status: OK|failed: 0 +warnings: 0 +skipped: 0 +errors: 0", ev$result))
e2e <- file.path(out_dir, paste0("e2e-", ver, ".log"))
e2e_txt <- if (file.exists(e2e)) paste0("<pre>", esc(paste(readLines(e2e, warn = FALSE), collapse = "\n")), "</pre>") else
  "<p>No browser-test log for this version (tools/e2e/*.py).</p>"
add("Package check, test suite and browser tests",
    "Does the released tarball pass R CMD check and the full test suite without failures, warnings or skips?", ok_ev,
    paste0(tab(ev), e2e_txt))

# Write ------------------------------------------------------------------------------
elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
n_ok <- sum(vapply(sections, function(s) isTRUE(s$ok), logical(1)))
html <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>herminR ', ver, ' validation report</title><style>',
  'body{font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif;max-width:980px;margin:24px auto;padding:0 16px;color:#222;line-height:1.45}',
  'h1{color:#1f4e79}h2{color:#1f4e79;border-bottom:1px solid #d9e2ec;padding-bottom:4px;margin-top:30px}',
  'table{border-collapse:collapse;margin:8px 0;font-size:90%}td,th{border-bottom:1px solid #e3e3e3;padding:3px 9px;text-align:left}',
  '.q{font-style:italic;color:#444}.pass{background:#e3f2e3;color:#1b5e20;padding:1px 7px;border-radius:3px;font-weight:bold}',
  '.fail{background:#fde2e1;color:#8b1a1a;padding:1px 7px;border-radius:3px;font-weight:bold}img{max-width:100%}',
  'pre{background:#f6f6f6;padding:10px;overflow-x:auto;font-size:85%}</style></head><body>',
  '<h1>herminR ', ver, ': validation report</h1>',
  '<p>', n_ok, ' of ', length(sections), ' checks passed', if (quick) ' (quick mode: smaller simulations)' else '',
  '; generated in ', sprintf("%.1f", elapsed), ' min by <code>tools/validation/run-validation.R</code>. ',
  'All numbers are in <code>validation-results-', ver, '.rds</code>.</p>', tab(env),
  '<h2>Summary</h2>', tab(data.frame(check = vapply(sections, `[[`, "", "title"),
                                    result = ifelse(vapply(sections, function(s) isTRUE(s$ok), logical(1)), "PASS", "FAIL"))),
  paste0(vapply(seq_along(sections), function(i) {
    s <- sections[[i]]
    paste0('<h2>', i, '. ', esc(s$title), ' ', verdict(s$ok), '</h2><p class="q">', esc(s$question), '</p>', s$body)
  }, ""), collapse = ""),
  '</body></html>')
f_html <- file.path(out_dir, paste0("validation-report-", ver, ".html"))
writeLines(html, f_html, useBytes = TRUE)
saveRDS(res, file.path(out_dir, paste0("validation-results-", ver, ".rds")))
cat(sprintf("\n%d/%d checks passed; report: %s (%.1f min)\n", n_ok, length(sections), f_html, elapsed))
if (n_ok < length(sections)) quit(status = 1)
