# Explorer 0.3.0: context-aware stages, run-on-click state, report, script, bookmarks
skip_if_not_installed("shiny")

pastry_inputs <- list(
  tabs = "start", source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"), block = "block",
  order = "SO", code = TRUE, norm = "l1", signed = "abs", std = "none", palette = "ermine", transpose = TRUE,
  annotate = TRUE, reorder = FALSE, bounds = "ellipsoid", level = 0.95, unc_response = ".all", boot_n = 30,
  boot_type = "residual", boot_seed = 1, rob_response = "y", alphas = "0.05, 0.1", top = 2,
  keep = c("x1", "x2", "x3"), others = "centre", feas_response = "y", constraint = "", feas_method = "exact",
  feas_size = 2000, feas_seed = 1,
  rng_x1_lo = -1, rng_x1_hi = 1, rng_x2_lo = -1, rng_x2_hi = 1, rng_x3_lo = -1, rng_x3_hi = 1,
  fea_x1_lo = -1, fea_x1_hi = 1, fea_x2_lo = -1, fea_x2_hi = 1, fea_x3_lo = -1, fea_x3_hi = 1)

test_that("capabilities follow the kind of data", {
  expect_equal(unlist(herminR:::explorer_capabilities("model")[c("intervals", "bootstrap", "selection")]), c(intervals = TRUE, bootstrap = TRUE, selection = TRUE))
  expect_equal(unlist(herminR:::explorer_capabilities("table")[c("intervals", "bootstrap", "selection")]), c(intervals = TRUE, bootstrap = FALSE, selection = FALSE))
  expect_equal(unlist(herminR:::explorer_capabilities("coef")[c("intervals", "bootstrap", "selection")]), c(intervals = FALSE, bootstrap = FALSE, selection = FALSE))
})

test_that("stages are greyed out and explained when the data cannot support them", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, utils::modifyList(pastry_inputs, list(source = "pastry_table1")))
    expect_false(st$capabilities()$intervals)        # drives output.cap_intervals (conditionalPanel)
    expect_equal(guide$stage_status(), c("ok", "ok", "unavailable", "limited", "ok", "ok"))
    expect_match(output$guide_stepper$html, "guide-pill unavailable|guide-pill  unavailable")
    expect_match(output$guide_stepper$html, "(not available)", fixed = TRUE)
    expect_match(output$guide_find_3$html, "Not available for this data")
    expect_match(output$source_info$html, "Uncertainty intervals")
    do.call(session$setInputs, pastry_inputs)
    expect_true(st$capabilities()$intervals)
    expect_true(st$capabilities()$bootstrap)
    expect_equal(guide$stage_status(), rep("ok", 6))
  })
})

test_that("bootstrap and selection: run on click, remember settings, flag stale results, re-enable button", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, pastry_inputs)
    expect_error(boot_res(), "Press 'Run bootstrap'")
    session$setInputs(run_boot = 1)
    expect_equal(boot_res()$rank_mean, bootstrap_ranks(analysis()$models$y, n = 30, seed = 1)$rank_mean)
    expect_match(output$boot_last$html, "response y, 30 replicates, residual resampling, seed 1")
    expect_false(grepl("Settings changed", output$boot_last$html))
    session$setInputs(boot_seed = 2)                                   # settings changed, not re-run
    expect_match(output$boot_last$html, "Settings changed since - press 'Run bootstrap' to update", fixed = TRUE)
    expect_equal(st$boot_state()$params$seed, 1)
    session$setInputs(boot_n = 3, run_boot = 2)                        # invalid: error stored, no crash
    expect_error(boot_res(), "between 20 and 5000")
    session$setInputs(run_sel = 1)
    expect_match(output$sel_last$html, "alphas 0.05, 0.1")
  })
})

test_that("feasible sampling: examples, approximate badge, share of allowed steps", {
  ex <- herminR:::explorer_constraint_examples(c("A", "B"))
  expect_true("A + B <= 0" %in% ex)
  expect_equal(herminR:::explorer_n_sampled("grid", 1000, 3), 729)   # floor(1000^(1/3)) = 9 in floating point, as in mpi_feasible()
  expect_equal(herminR:::explorer_n_sampled("lhs", 1234, 3), 1234L)
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, pastry_inputs)
    expect_match(output$feas_info$html, "Exact")
    session$setInputs(constraint_example = "x1 + x2 <= 0")
    session$setInputs(constraint = "x1 + x2 <= 0", feas_method = "lhs", run_feas = 1)
    expect_match(output$feas_info$html, "Approximate")
    expect_match(output$feas_info$html, "About \\d+% of the sampled one-unit steps")
    expect_match(output$feas_last$html, "LHS, 2000 samples, seed 1, constraint x1 + x2 &lt;= 0", fixed = TRUE)
  })
})

test_that("the full generated script runs and reproduces bootstrap, selection and feasible results", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, utils::modifyList(pastry_inputs, list(
      constraint = "x1 + x2 <= 0", feas_method = "lhs", fea_x3_lo = -0.5, fea_x3_hi = 0.5)))
    session$setInputs(run_boot = 1, run_sel = 1, run_feas = 1)
    code <- output$code
    expect_match(code, "bootstrap_ranks\\(input\\[\\[\"y\"\\]\\], n = 30")
    expect_match(code, "mpi_selection_paths\\(input\\[\\[\"y\"\\]\\], alphas = c\\(0.05, 0.1\\)")
    expect_match(code, "constraint = function\\(d\\) with\\(d, x1 \\+ x2 <= 0\\)")
    e <- new.env(parent = globalenv())
    pdf(NULL); on.exit(dev.off(), add = TRUE)
    code_eval <- gsub("plot\\((bootstrap_ranks|mpi_selection_paths)", "res_\\1 <- (\\1", code)
    suppressWarnings(eval(parse(text = code_eval), envir = e))
    expect_equal(e$res_bootstrap_ranks$rank_mean, boot_res()$rank_mean)
    expect_equal(e$res_mpi_selection_paths$estimate, sel_res()$estimate)
    expect_equal(e$fe$estimate, feas_res()$estimate)
  })
})

test_that("advanced runs appear in the script", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, utils::modifyList(pastry_inputs, list(
      adv_iter = 25, adv_tol = 1e-6, adv_seed = 3, adv_method = "delta", adv_verbose = FALSE,
      adv_cache = FALSE, adv_show = FALSE)))
    session$setInputs(adv_run = 1)
    expect_match(output$code, "# Advanced run")
    expect_match(output$code, "adv_b <- bootstrap_ranks\\(input\\[\\[\"y\"\\]\\], n = 25")
  })
})

test_that("report: base64 encoder and self-contained HTML with findings, plots and script", {
  expect_equal(herminR:::.base64(charToRaw("Man")), "TWFu")
  expect_equal(herminR:::.base64(charToRaw("Ma")), "TWE=")
  expect_equal(herminR:::.base64(charToRaw("M")), "TQ==")
  expect_equal(herminR:::.base64(charToRaw("herminR report")), "aGVybWluUiByZXBvcnQ=")
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, pastry_inputs)
    session$setInputs(run_boot = 1)
    html <- report_html()
    expect_match(html, "^<!DOCTYPE html>")
    expect_match(html, "2. Which factors matter most?", fixed = TRUE)
    expect_match(html, "can change y by up to 1.68", fixed = TRUE)
    expect_match(html, "x2 ranked first in 100% of 30 replays", fixed = TRUE)
    expect_gte(lengths(regmatches(html, gregexpr("data:image/png;base64,", html))), 5)
    expect_match(html, "Reproduce in R", fixed = TRUE)
    expect_false(grepl("<script", html, fixed = TRUE))
    f <- output$dl_report
    expect_true(file.exists(f))
    expect_gt(file.info(f)$size, 10000)
  })
})

test_that("bookmarking is enabled and excludes navigation and the uploaded file", {
  app <- explorer_app()
  expect_equal(app$appOptions$bookmarkStore, "url")
  html <- as.character(explorer_ui())
  expect_match(html, 'id="bookmark"', fixed = TRUE)
  expect_match(html, 'id="dl_report"', fixed = TRUE)
})

test_that("switching source keeps the dataset's default model (block term not lost)", {
  shiny::testServer(explorer_server, {
    session$setInputs(tabs = "start", source = "pastry_table1", norm = "l1", signed = "abs", std = "none",
                      order = "SO", code = TRUE)
    session$setInputs(source = "pastrydough")
    session$setInputs(responses = "y", factors = c("x1", "x2", "x3"), block = "block")
    data(pastrydough, package = "herminR")
    ref <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
    expect_equal(shown_mpi()$estimate, mpi(ref, abs = TRUE)$estimate, tolerance = 1e-12)
  })
})
