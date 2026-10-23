test_that("colmax gives each response max |MPI| = 1 and keeps ratios", {
  m <- mpi_multi(list(A = c(x1 = 2, x2 = -4, "x1^2" = 1), B = c(x1 = 1, x2 = 0.5)))
  s <- standardize_mpi(m, "colmax")
  expect_equal(as.numeric(tapply(abs(s$estimate), s$response, max)), c(1, 1))
  expect_equal(s$estimate[s$response == "A"], c(0.75, -1))
  sf <- standardize_mpi(m, "colmax", by = "factor")
  expect_equal(as.numeric(tapply(abs(sf$estimate), sf$factor, max)), c(1, 1))
})

test_that("residual_sd uses each response's model", {
  d <- make_ccd()
  d$y2 <- 3 * d$y + rnorm(nrow(d), 0, 0.5)
  ms <- list(A = lm(y ~ x1 + x2, d), B = lm(y2 ~ x1 + x2, d))
  s <- standardize_mpi(mpi(ms), "residual_sd")
  expect_equal(s$estimate[s$response == "B"], s$estimate_raw[s$response == "B"] / sigma(ms$B))
})

test_that("rank gives within-response ranks of |MPI|", {
  s <- standardize_mpi(mpi(c(x1 = 1, x2 = -3, x3 = 2)), "rank")
  expect_equal(s$estimate, c(1, 3, 2))
})
