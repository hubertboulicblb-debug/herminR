# Bootstrap validation: independent recomputation and known distributions ----

test_that("residual bootstrap equals an independent hand recomputation", {
  d <- make_ccd(sigma = 0.3)
  fit <- lm(y ~ x1 + x2 + x3 + I(x1^2) + x1:x2, data = d)
  n <- 60
  br <- bootstrap_ranks(fit, n = n, seed = 11)
  # hand recomputation: same seed, plain lm(), L1 MPI from explicit groups
  groups <- list(x1 = c("x1", "I(x1^2)", "x1:x2"), x2 = c("x2", "x1:x2"), x3 = "x3")
  set.seed(11)
  R <- t(vapply(seq_len(n), function(i) {
    ys <- fitted(fit) + sample(residuals(fit), nrow(d), replace = TRUE)
    b <- coef(lm(update(formula(fit), ys ~ .), data = cbind(d, ys = ys)))
    est <- vapply(groups, function(g) sum(abs(b[g])), numeric(1))
    rank(-est, ties.method = "min")
  }, numeric(3)))
  expect_equal(br$rank_mean, unname(colMeans(R)), tolerance = 1e-12)
  expect_equal(br$top1_frac, unname(colMeans(R == 1)), tolerance = 1e-12)
  expect_equal(br$rank_lower, unname(apply(R, 2, quantile, 0.025, type = 1)))
  expect_equal(br$rank_upper, unname(apply(R, 2, quantile, 0.975, type = 1)))
  expect_equal(attr(br, "n_ok") + attr(br, "n_failed") + attr(br, "n_aliased"), n)
})

test_that("known distribution: a dominant factor always ranks first", {
  d <- make_ccd(sigma = 0.05)
  d$y <- 10 * d$x1 + 0.1 * d$x2 + rnorm(nrow(d), sd = 0.05)
  br <- bootstrap_ranks(lm(y ~ x1 + x2 + x3, data = d), n = 200, seed = 1)
  expect_equal(br$top1_frac[br$factor == "x1"], 1)
  expect_equal(c(br$rank_lower[1], br$rank_upper[1]), c(1, 1))
})

test_that("known distribution: exchangeable factors split first place evenly", {
  set.seed(5)
  d <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), rep = 1:3)
  d$x3 <- rep(c(-1, 1), length.out = nrow(d))
  d$y <- d$x1 + d$x2 + rnorm(nrow(d), sd = 0.5)
  br <- bootstrap_ranks(lm(y ~ x1 + x2 + x3, data = d), n = 400, seed = 2)
  p1 <- br$top1_frac[br$factor == "x1"]; p2 <- br$top1_frac[br$factor == "x2"]
  expect_equal(p1 + p2, 1)                                  # x3 never first; no ties
  z <- qnorm(0.995); se <- sqrt(0.25 / 400)
  expect_lt(abs(p1 - 0.5), z * se + 0.1)                    # ~1/2 up to the data's own asymmetry
})

test_that("ties share the smallest rank", {
  expect_equal(herminR:::.rank_desc(c(a = 2, b = 0, c = 0, d = 5)), c(a = 2, b = 3, c = 3, d = 1))
  expect_equal(herminR:::.rank_desc(c(a = 1, b = 1)), c(a = 1, b = 1))
})

test_that("failed and aliased replicates are counted, excluded and reported", {
  d <- data.frame(x1 = c(-1, 1, -1, 1, 0, 0), x2 = c(-1, -1, 1, 1, 0, 0))
  set.seed(3); d$y <- d$x1 + 0.5 * d$x2 + 0.3 * d$x1 * d$x2 + rnorm(6, sd = 0.1)
  fit <- lm(y ~ x1 * x2, data = d)
  expect_warning(br <- bootstrap_ranks(fit, n = 100, type = "case", seed = 4), "aliased")
  expect_gt(attr(br, "n_aliased"), 0)
  expect_equal(attr(br, "n_ok") + attr(br, "n_failed") + attr(br, "n_aliased"), 100)
  expect_true(all(br$top1_frac >= 0 & br$top1_frac <= 1))
})

test_that("invalid arguments are rejected", {
  fit <- lm(y ~ x1 + x2, data = make_ccd())
  expect_error(bootstrap_ranks(fit, n = 1), "whole number")
  expect_error(bootstrap_ranks(fit, n = 10.5), "whole number")
  expect_error(bootstrap_ranks(fit, n = c(5, 6)), "whole number")
  expect_error(bootstrap_ranks(fit, level = 1), "between 0 and 1")
  expect_error(bootstrap_ranks(fit, level = 0), "between 0 and 1")
  expect_error(bootstrap_ranks(fit, level = NA), "between 0 and 1")
  expect_error(bootstrap_ranks(fit, norm = "l3"))
})

# Interval-plot edge cases -----------------------------------------------------

builds_clean <- function(p) {
  expect_no_warning(b <- ggplot2::ggplot_build(p))
  b
}

test_that("interval plot: partial missing bounds", {
  fit <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2), data = make_ccd())
  m <- mpi(fit, bounds = "delta")
  m$lower[2] <- NA; m$upper[2] <- NA
  p <- mpi_interval_plot(m)
  builds_clean(p)
  expect_equal(as.character(p$data$precise[2]), "no interval")
})

test_that("interval plot: all-zero influences and non-L1 scores near zero", {
  z <- data.frame(term = c("x1", "x2", "x1:x2"), estimate = 0, se = 0.1)
  builds_clean(mpi_interval_plot(mpi(z, bounds = "delta")))
  tiny <- data.frame(term = c("x1", "x2", "x1^2"), estimate = c(1e-6, -2e-6, 1e-7), se = 0.05)
  expect_warning(m_tiny <- mpi(tiny, bounds = "ellipsoid", norm = "l2"), "uncorrelated")
  p <- mpi_interval_plot(m_tiny)
  builds_clean(p)
  expect_match(p$labels$title, "L2 score")
  expect_true(all(p$data$precise == "interval reaches 0"))
})

test_that("interval plot: responses with different factor sets", {
  d <- make_ccd()
  m <- mpi(list(A = lm(y ~ x1 + x2, d), B = lm(y ~ x3 + I(x3^2), d)), bounds = "delta")
  p <- mpi_interval_plot(m)
  b <- builds_clean(p)
  expect_equal(length(unique(b$layout$layout$PANEL)), 2)
  expect_equal(sum(m$estimate == 0), 3)   # absent factors are shown at zero
})
