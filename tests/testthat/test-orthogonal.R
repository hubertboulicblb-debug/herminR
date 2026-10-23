test_that("|MPI| is invariant when a coded factor is reversed and refitted", {
  d <- make_ccd()
  f <- y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2)
  m1 <- lm(f, data = d)
  d2 <- d; d2$x1 <- -d2$x1
  m2 <- lm(f, data = d2)
  r1 <- mpi(m1); r2 <- mpi(m2)
  expect_equal(abs(r1$estimate), abs(r2$estimate), tolerance = 1e-10)
  expect_equal(sign(r1$estimate[1]), -sign(r2$estimate[1]))
  expect_equal(r1$se, r2$se, tolerance = 1e-10)
})

test_that("well-separated signals: all standardizations agree on ranks", {
  b <- c(x1 = 3, x2 = 1, x3 = 0.2, "x1:x2" = 0.1)
  m <- mpi(b, abs = TRUE)
  r_col <- rank(standardize_mpi(m, "colmax")$estimate)
  r_rank <- standardize_mpi(m, "rank")$estimate
  expect_equal(r_col, r_rank)
})
