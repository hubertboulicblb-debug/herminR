# 0.5.0: feasible-region convergence, interpretation aids, app usability

pastry_fit <- function() {
  lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
}

test_that("sampled feasible MPI never exceeds the exact value and approaches it", {
  fit <- pastry_fit()
  cv <- mpi_feasible_convergence(fit, region = list(x3 = c(-0.5, 0.5)), sizes = c(200, 800, 3200), seeds = 1:3)
  expect_s3_class(cv, "herminR_convergence")
  expect_equal(nrow(cv), 3 * 3 * 3)
  expect_true(all(cv$estimate <= cv$exact + 1e-12))
  ex <- mpi_feasible(fit, region = list(x3 = c(-0.5, 0.5)))
  expect_equal(unique(cv$exact[cv$factor == "x2"]), ex$estimate[ex$factor == "x2"])
  s <- attr(cv, "summary")
  expect_named(s, c("factor", "estimate", "spread", "last_change", "gap_to_exact", "min_feasible", "stable"))
  # the median of the largest size is closer to the exact value than the smallest size's
  med <- function(n) tapply(cv$estimate[cv$size == n], cv$factor[cv$size == n], stats::median)
  gap_small <- ex$estimate - med(200)[ex$factor]
  gap_large <- ex$estimate - med(3200)[ex$factor]
  expect_true(all(gap_large <= gap_small + 1e-12))
  expect_true(all(s$gap_to_exact >= 0))
  expect_s3_class(plot(cv), "ggplot")
  expect_output(print(cv), "Convergence of sampled feasible-region MPI")
})

test_that("with a constraint there is no exact value; summary flags stability with the tolerance", {
  b <- c(x1 = 1, x2 = 0.5, "x1:x2" = 0.3, "x1^2" = -0.2)
  cv <- mpi_feasible_convergence(b, constraint = function(d) d$x1 + d$x2 <= 0, sizes = c(200, 400), seeds = 1:2)
  expect_true(all(is.na(cv$exact)))
  s <- attr(cv, "summary")
  expect_true(all(is.na(s$gap_to_exact)))
  loose <- herminR:::.convergence_summary(cv, tol = 10)
  expect_true(all(loose$stable))
  txt <- explorer_convergence_text(loose)
  expect_match(txt[length(txt)], "All factors have levelled off")
  expect_error(mpi_feasible_convergence(b, sizes = 100), "at least two increasing")
  expect_error(mpi_feasible_convergence(b, sizes = c(400, 200)), "at least two increasing")
})

test_that("the explorer runs the convergence check and reports it in plain words", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, utils::modifyList(golden_inputs("pastrydough"),
                                                 list(feas_method = "lhs", feas_size = 800, fea_x3_lo = -0.5, fea_x3_hi = 0.5)))
    session$setInputs(run_conv = 1)
    s <- attr(st$conv_res(), "summary")
    expect_equal(sort(unique(st$conv_res()$size)), c(100, 200, 400, 800))
    expect_equal(sort(unique(st$conv_res()$seed)), 1:3)
    expect_match(output$conv_info$html, "below the exact value")
    expect_match(output$conv_last$html, "LHS up to 800 samples, seeds 1-3")
    expect_match(output$conv_last$html, "\\(took (&lt; )?[0-9.]+ s\\)")
  })
})

test_that("interval notes explain coverage per method and separate it from rank stability", {
  e <- as.character(explorer_bounds_note("ellipsoid", 0.9))
  expect_match(e, "at least 90%")
  expect_match(e, "per factor")
  expect_match(as.character(explorer_bounds_note("delta", 0.95)), "Approximately 95%")
  expect_match(as.character(explorer_bounds_note("se", 0.95)), "no exact joint coverage")
  for (b in c("ellipsoid", "delta", "se")) expect_match(as.character(explorer_bounds_note(b, 0.95)), "bootstrap below")
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(bounds = "delta")
    expect_match(output$bounds_note$html, "Delta method")
    expect_match(output$rank_note$html, "not a confidence interval")
  })
})

test_that("a select gives a keyboard alternative to clicking, both ways", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(highlight_select = "x3")
    expect_equal(focus()$factor, "x3")
    session$setInputs(highlight_select = "")
    expect_null(focus())
    # a click sets the same state the select reflects
    session$setInputs(heatmap_click = list(x = 1, y = 2, domain = list(discrete_limits = list(x = "y", y = c("x1", "x2", "x3")))))
    expect_equal(focus()$factor, "x2")
  })
})

test_that("run timing, durations, responsive outputs and collapsible tables", {
  expect_equal(explorer_duration(0.42), "0.4 s")
  expect_equal(explorer_duration(72), "1 min 12 s")
  po <- as.character(explorer_plot_output("p1", vh = 50, min = 300, max = 500))
  expect_match(po, "height:50vh")
  expect_match(po, "min-height:300px;max-height:500px")
  d <- as.character(explorer_details("Table", shiny::p("x")))
  expect_match(d, "<details class=\"more\">")
  expect_match(as.character(explorer_details("T", open = TRUE)), "open")
  st <- list(value = 1, error = NULL, params = list(a = 1), time = Sys.time(), seconds = 3.21, runs = 1L)
  expect_match(as.character(explorer_last_run_ui(st, list(a = 1), function(p) "a=1", "Run")), "\\(took 3.2 s\\)")
  ui <- as.character(explorer_ui())
  expect_match(ui, "Advanced View")              # still reachable, under "More"
  expect_match(ui, "data-toggle=\"dropdown\"|dropdown-toggle")
  expect_match(ui, "Running\\\\u2026|Running")
})

test_that("printing a column subset of an MPI table gives no warning", {
  u <- mpi(pastry_fit(), bounds = "ellipsoid", abs = TRUE)
  expect_silent(utils::capture.output(print(u[, c("factor", "estimate", "lower", "upper")])))
})
