# 0.5.1: reliability flags, convergence verdicts, limitations, data checks,
# width-adapted plots

pastry_fit <- function() {
  lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
}
grid3 <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), x3 = c(-1, 0, 1))
upload <- function(d, resp = "y", factors = c("x1", "x2", "x3"), order = "SO") {
  f <- tempfile(fileext = ".csv"); utils::write.csv(d, f, row.names = FALSE)
  list(tabs = "start", source = "upload_data", file = list(name = "d.csv", datapath = f), responses = resp,
       factors = factors, block = "", order = order, code = TRUE, norm = "l1", signed = "abs", std = "none",
       palette = "ermine", transpose = TRUE, annotate = TRUE, reorder = FALSE, bounds = "ellipsoid", level = 0.95,
       unc_response = ".all", feas_response = resp, rob_response = resp, constraint = "", feas_method = "exact")
}

test_that("delta intervals are flagged when a term is not clearly non-zero; ellipsoid never", {
  fit <- pastry_fit()
  u <- mpi_multi(list(y = fit), bounds = "delta", abs = TRUE)
  fl <- explorer_interval_flags(u, "delta", 0.95)
  tm <- attr(u, "term_maps")$y
  tval <- abs(tm$coef) / sqrt(diag(tm$vcov))[names(tm$coef)]
  crit <- stats::qt(0.975, tm$df)
  for (f in fl$factor) {
    terms <- names(tm$powers)[vapply(tm$powers, function(p) f %in% names(p), logical(1))]
    expect_equal(fl$reliable[fl$factor == f], all(tval[terms] >= crit), info = f)
  }
  expect_match(fl$note[!fl$reliable][1], "not clearly non-zero; prefer the ellipsoid interval")
  e <- explorer_interval_flags(mpi_multi(list(y = fit), bounds = "ellipsoid", abs = TRUE), "ellipsoid")
  expect_true(all(e$reliable))
  s <- explorer_interval_flags(mpi_multi(list(y = fit), bounds = "se", abs = TRUE), "se")
  expect_false(any(s$reliable))
  # a model whose terms are all strongly significant gets no flag
  d <- grid3; set.seed(3); d$y <- with(d, 3 * x1 + 2 * x2) + rnorm(27, sd = 0.05)
  strong <- lm(y ~ x1 + x2, data = d)
  expect_true(all(explorer_interval_flags(mpi_multi(list(y = strong), bounds = "delta", abs = TRUE), "delta")$reliable))
})

test_that("the flags reach the warning box, the table, the plot, the tooltip and the guide", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    expect_false(grepl("may be too narrow", output$bounds_note$html))        # ellipsoid: no warning
    session$setInputs(bounds = "delta")
    expect_match(output$bounds_note$html, "intervals may be too narrow")
    expect_match(output$bounds_note$html, "Switch to &#39;Ellipsoid&#39;|Switch to 'Ellipsoid'")
    expect_match(output$interval_table, "reliability")
    p <- explorer_flag_intervals(mpi_interval_plot(unc_mpi()), st$int_flags())
    expect_equal(p$layers[[length(p$layers)]]$aes_params$label, "  may be too narrow")
    expect_true(any(grepl("Caution: the delta-method interval may be too narrow", unlist(guide$findings(3)))))
    session$setInputs(interval_plot_hover = list(x = 1, y = 1, coords_css = list(x = 5, y = 5),
                                                 domain = list(discrete_limits = list(y = paste0("y\r", c("x1", "x3", "x2"))))))
    expect_match(output$interval_plot_tip$html, "may be too narrow")
    session$setInputs(bounds = "se")
    expect_match(output$bounds_note$html, "not confidence intervals")
  })
})

test_that("feasible results carry the convergence verdict for the current settings", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, utils::modifyList(golden_inputs("pastrydough"),
      list(feas_method = "lhs", feas_size = 800, constraint = "x1 + x2 <= 0")))
    session$setInputs(run_feas = 1)
    expect_match(output$feas_info$html, "Convergence not checked")
    expect_true(any(grepl("Convergence not checked", unlist(guide$findings(5)))))
    session$setInputs(run_conv = 1)
    cs <- st$conv_status()
    expect_true(cs$checked)
    expect_match(output$feas_info$html, if (length(cs$unstable)) "Not yet stable" else "Stable under this sampling")
    # changing a setting invalidates the verdict
    session$setInputs(feas_seed = 2)
    expect_false(st$conv_status()$checked)
    expect_match(output$feas_info$html, "Convergence not checked")
  })
  s <- data.frame(factor = "x1", estimate = 1, spread = 0, last_change = 0, gap_to_exact = NA, min_feasible = 100, stable = TRUE)
  txt <- explorer_convergence_text(s)
  expect_match(txt[2], "does not show that every feasible setting was searched")
})

test_that("the report states the methods and limitations of what is on screen", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(bounds = "delta")
    session$setInputs(run_boot = 1)
    html <- report_html()
    expect_match(html, "<h2>Methods and limitations</h2>")
    expect_match(html, "MPI is a property of the fitted model")
    expect_match(html, "Flagged in this analysis: x")
    expect_match(html, "not a confidence interval")
    lim <- explorer_limitations(st, input)
    expect_named(lim, c("Model", "Intervals", "Rank stability"))
  })
})

test_that("data checks refuse unusable data with a plain explanation", {
  bad_text <- grid3; bad_text$y <- 1; bad_text$x3 <- rep(c("lo", "mid", "hi"), 9)
  chk <- explorer_check_data(bad_text, "y", c("x1", "x2", "x3"))
  expect_match(chk$errors, "Factor 'x3' must contain numbers, but it contains text such as 'lo'")
  resp_text <- grid3; resp_text$y <- rep(c("good", "bad", "ok"), 9)
  expect_match(explorer_check_data(resp_text, "y", c("x1", "x2", "x3"))$errors, "Response 'y' must contain numbers")
  const <- grid3; const$y <- 1; const$x3 <- 5
  expect_match(explorer_check_data(const, "y", c("x1", "x2", "x3"))$errors, "single value \\(5\\)")
  na <- grid3; na$y <- seq_len(27); na$y[c(2, 5)] <- NA
  expect_match(explorer_check_data(na, "y", c("x1", "x2", "x3"))$notes, "2 runs with missing values \\(in y\\) are left out: 25 of 27")
  two <- grid3; two$x3 <- ifelse(two$x3 > 0, 1, -1); two$y <- 1
  expect_match(explorer_check_data(two, "y", c("x1", "x2", "x3"), order = "SO")$notes, "only two levels")
  expect_length(explorer_check_data(two, "y", c("x1", "x2", "x3"), order = "FO")$notes, 0)
  expect_match(explorer_check_data(grid3, "zz", "x1")$errors, "not found")
})

test_that("too few runs for the model are refused; small residual df is noted", {
  set.seed(4)
  few <- grid3[c(1, 5, 9, 11, 15, 19, 23, 27), ]          # 8 runs, all factors vary
  few$y <- rnorm(8)
  m <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = few)   # 10 terms
  expect_match(explorer_check_fit(list(y = m))$errors, "8 runs for a model with 10 terms .* At least 11 runs are needed")
  d <- grid3; d$y <- rnorm(27)
  m2 <- lm(y ~ x1 + x2 + x3, data = d[c(1, 5, 9, 14, 20, 27), ])   # 6 runs, 4 terms: 2 residual df
  expect_equal(stats::df.residual(m2), 2)
  expect_length(explorer_check_fit(list(y = m2))$errors, 0)
  expect_match(explorer_check_fit(list(y = m2))$notes, "only 2 residual degrees of freedom")
  # in the app: no result, an explanation, and no 'reliable leader' from a meaningless bootstrap
  shiny::testServer(explorer_server, {
    suppressWarnings(do.call(session$setInputs, upload(few)))
    expect_error(shown_mpi(), "At least 11 runs are needed")
    expect_match(paste(unlist(guide$findings(2)), collapse = " "), "At least 11 runs are needed")
    session$setInputs(run_boot = 1)
    expect_error(boot_res())
  })
  shiny::testServer(explorer_server, {
    two <- grid3; two$x3 <- ifelse(two$x3 > 0, 1, -1); set.seed(5); two$y <- rnorm(27)
    do.call(session$setInputs, upload(two))
    expect_match(output$source_info$html, "Data checks")
    expect_true("Data checks" %in% names(guide$findings(1)))
  })
})

test_that("near-ties are named in the guide instead of a confident ranking", {
  b <- list(y = c(x1 = 1, x2 = 0.995, x3 = 0.2))
  expect_match(guide_mpi_text(mpi_multi(b, abs = TRUE))[1], "x2 is practically tied with it")
  b2 <- list(y = c(x1 = 1, x2 = 0.97, x3 = 0.2))
  expect_match(guide_mpi_text(mpi_multi(b2, abs = TRUE))[1], "x2 is close behind")
  b3 <- list(y = c(x1 = 1, x2 = 0.5))
  expect_match(guide_mpi_text(mpi_multi(b3, abs = TRUE))[1], "Next: x2")
})

test_that("plots adapt to narrow widths", {
  m <- mpi_multi(dss_sim$coefs, abs = TRUE)
  p <- plot(m, transpose = TRUE)
  narrow <- explorer_fit_heatmap(p, 390)
  expect_false(any(vapply(narrow$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
  expect_match(narrow$labels$caption, "Values hidden at this width")
  wide <- explorer_fit_heatmap(p, 1400)
  expect_true(any(vapply(wide$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
  q <- explorer_fit_width(mpi_contour(mpi(pastry_fit()), "x2"), 390)
  expect_true(all(nchar(strsplit(q$labels$subtitle, "\n")[[1]]) <= 52))
  expect_equal(q$theme$legend.position, "bottom")
  expect_identical(explorer_fit_width(p, NULL), p)
  s <- mpi_sankey(m, response = "Response_1", compact = TRUE)
  lab <- s$layers[[3]]
  expect_true(inherits(lab$geom, "GeomLabel"))
  expect_true(all(lab$data$hj == 0))
  expect_match(s$labels$caption, "very small node")
  expect_s3_class(plot(bootstrap_ranks(pastry_fit(), n = 20, seed = 1), type = "flow", compact = TRUE), "ggplot")
})

test_that("the MPI table is built in one step (fast) and is unchanged", {
  m <- mpi_multi(dss_sim$coefs)
  expect_s3_class(m, "tbl_df")
  expect_named(tibble::as_tibble(m)[, 1:9], c("factor", "response", "estimate", "se", "lower", "upper", "norm",
                                              "terms_contributed", "direction"))
  t0 <- system.time(for (i in 1:20) mpi(pastry_fit()))[["elapsed"]]
  expect_lt(t0, 5)
})
