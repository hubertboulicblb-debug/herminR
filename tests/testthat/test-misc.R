test_that("response_cor aligns observations by row name", {
  d <- make_ccd()
  d$y2 <- d$y + rnorm(nrow(d), 0, 0.01)
  d$y2[1:5] <- NA
  expect_message(C <- response_cor(list(a = lm(y ~ x1, d), b = lm(y2 ~ x1, d))), "excluded")
  expect_equal(C[1, 2], cor(d$y, d$y2, use = "complete.obs"))
  expect_true(all(attr(C, "n_obs") == nrow(d) - 5))
})

test_that("plots build in all modes, including all-NA panels", {
  d <- make_ccd()
  ms <- list(A = lm(y ~ x1 + x2 + x3 + I(x1^2), d), B = lm(-y ~ x1 + x2 + x3, d), C = lm(y ~ x1 * x2, d))
  m <- mpi(ms, bounds = "delta")
  for (pal in c("ermine", "colour")) {
    expect_s3_class(plot(m, palette = pal), "ggplot")
    expect_s3_class(plot(m, palette = pal, signed = FALSE, method = "colmax"), "ggplot")
  }
  expect_s3_class(plot(m, panel = "significance"), "ggplot")
  m0 <- mpi(ms)
  expect_warning(p <- plot(m0, panel = "lower"), "missing")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_s3_class(ggplot2::autoplot(m), "ggplot")
  expect_s3_class(plot(mpi_compare_norms(ms$A)), "ggplot")
  expect_s3_class(mpi_waterfall(ms$A), "ggplot")
  for (pp in list(plot(m, palette = "ermine"), plot(m, panel = "significance"))) {
    expect_s3_class(ggplot2::ggplot_build(pp), "ggplot_built")
  }
})

test_that("paper figures run and record provenance", {
  out <- mpi_paper_figures()
  expect_named(out, c("fig1", "fig2", "fig3", "fig4"))
  expect_named(attr(out, "provenance"), c("pastry", "dss"))
  expect_equal(unique(out$fig4$factor), paste0("Factor_", c(1, 2, 4, 5, 6, 11)))
  ct <- list(Y4 = c(x3 = 0.2294, "x3^2" = 0.4185, "x2:x3" = 0.3706))
  o2 <- suppressMessages(mpi_paper_figures(which = 1, coef_table = ct))
  expect_equal(o2$fig1$estimate_raw[o2$fig1$factor == "x3"], 1.0185, tolerance = 1e-10)
  expect_equal(attr(o2, "provenance")[["pastry"]], "user-supplied coefficient table")
})

test_that("factors are ordered naturally", {
  r <- mpi(c(Factor_10 = 1, Factor_2 = 1, Factor_1 = 1))
  expect_equal(r$factor, c("Factor_1", "Factor_2", "Factor_10"))
})

test_that("sim_mpi recovers the true MPI on average", {
  s <- sim_mpi(n_factors = 3, active = 1:2, sigma = 0.05, n_sim = 30, seed = 3)
  avg <- tapply(s$mpi$estimate, s$mpi$factor, mean)
  expect_equal(as.numeric(avg[names(s$true_mpi)]), unname(s$true_mpi), tolerance = 0.1)
  expect_equal(nrow(s$estimates), 30)
})

test_that("mpi_paper_figures(plot = TRUE) returns ggplots", {
  p <- mpi_paper_figures(plot = TRUE)
  expect_true(all(vapply(p, inherits, logical(1), "ggplot")))
})

test_that("mpi_paper_figures validates `which`", {
  expect_error(mpi_paper_figures(which = 5), "subset of 1:4")
  expect_error(mpi_paper_figures(which = integer(0)), "subset of 1:4")
  expect_error(mpi_paper_figures(which = "1"), "subset of 1:4")
  expect_named(mpi_paper_figures(which = c(4, 4)), "fig4")
})

test_that("response_cor reports pairwise observation counts", {
  df <- data.frame(a = c(1, 2, 3, 4, NA), b = c(2, 1, 4, 3, 5), c = c(NA, NA, 1, 2, 3))
  C <- response_cor(df)
  n <- attr(C, "n_obs")
  expect_equal(n["a", "b"], 4L)
  expect_equal(n["a", "c"], 2L)
  expect_equal(n["b", "b"], 5L)
})

test_that("interval plot shows bounds, flags imprecise factors, and refuses missing bounds", {
  d <- make_ccd(sigma = 0.4)
  fit <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  m <- mpi(fit, bounds = "ellipsoid")
  p <- mpi_interval_plot(m)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_match(p$labels$subtitle, "ellipsoid bounds, level 0.95")
  expect_match(p$labels$caption, "not simultaneous")
  expect_equal(as.character(p$data$precise), ifelse(m$lower > 0, "lower bound > 0", "interval reaches 0"))
  expect_error(mpi_interval_plot(mpi(fit)), "No bounds")
  mm <- mpi(list(A = fit, B = lm(-y ~ x1 + x2 + x3, d)), bounds = "delta")
  expect_s3_class(ggplot2::ggplot_build(mpi_interval_plot(mm)), "ggplot_built")
  expect_match(mpi_interval_plot(mpi(fit, bounds = "delta", norm = "l2"))$labels$title, "L2 score")
})

test_that("bootstrap rank plot", {
  d <- make_ccd()
  fit <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  br <- bootstrap_ranks(fit, n = 25, seed = 1)
  expect_s3_class(br, "herminR_ranks")
  p <- plot(br)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_match(p$labels$subtitle, "residual bootstrap, 25 replicates")
  expect_equal(levels(p$data$factor)[nlevels(p$data$factor)], br$factor[which.min(br$rank_mean)])
})

test_that("norm-comparison plot states its per-panel scaling", {
  p <- plot(mpi_compare_norms(c(x1 = 1, x2 = 0.5, "x1:x2" = 0.3)))
  expect_match(p$labels$caption, "own maximum")
})
