test_that("exact feasible MPI matches brute force on a box", {
  b <- c(x1 = 1, x2 = 0.5, "x1:x2" = 0.3, "x1^2" = -0.2, "x2^2" = 0.4)
  res <- mpi_feasible(b, region = list(x2 = c(-0.5, 0.5)))
  expect_equal(res$estimate[res$factor == "x1"], 1 + 0.15 + 0.2)
  # x2 in [-0.5, 0.5]: the only unit step is -0.5 -> 0.5, so the quadratic
  # change is 0.4 * (0.5^2 - 0.5^2) = 0 and the change is 0.5 + 0.3 * x1
  expect_equal(res$estimate[res$factor == "x2"], 0.8, tolerance = 1e-12)
  expect_true(all(res$feas_ratio <= 1 + 1e-12))
})

test_that("sampling approaches the exact value from below; seed and size matter", {
  b <- c(x1 = 1, x2 = 0.5, "x1:x2" = 0.3, "x1^2" = -0.2, x3 = 0.1, "x2:x3" = 0.2)
  ex <- mpi_feasible(b, region = list(x2 = c(-0.5, 1)))
  s1 <- mpi_feasible(b, region = list(x2 = c(-0.5, 1)), method = "lhs", size = 20000, seed = 1)
  s2 <- mpi_feasible(b, region = list(x2 = c(-0.5, 1)), method = "lhs", size = 20000, seed = 1)
  s3 <- mpi_feasible(b, region = list(x2 = c(-0.5, 1)), method = "lhs", size = 50, seed = 2)
  expect_identical(s1$estimate, s2$estimate)
  expect_false(identical(s1$estimate, s3$estimate))
  expect_true(all(s1$estimate <= ex$estimate + 1e-12))
  expect_true(all(ex$estimate - s1$estimate < 0.05 * ex$estimate))
  g <- mpi_feasible(b, region = list(x2 = c(-0.5, 1)), method = "grid", size = 30^3)
  expect_true(all(abs(g$estimate - ex$estimate) < 1e-8))  # grid hits the vertices
})

test_that("non-rectangular constraints reduce attainable influence", {
  b <- c(x1 = 1, x2 = 1, "x1:x2" = 1)
  free <- mpi_feasible(b, method = "grid", size = 41^2)
  con <- mpi_feasible(b, constraint = function(d) d$x1 + d$x2 <= 0, method = "grid", size = 41^2)
  expect_true(all(con$estimate < free$estimate))
  expect_true(all(con$n_feasible > 0))
  expect_error(mpi_feasible(b, constraint = function(d) TRUE), "needs")
})

test_that("factor-name prefixes are not confused", {
  set.seed(1)
  dd <- data.frame(x1 = runif(40, -1, 1), x10 = runif(40, -1, 1), x11 = runif(40, -1, 1))
  dd$y <- dd$x1 + dd$x10 + dd$x11 + rnorm(40, 0, .05)
  m <- lm(y ~ x1 + x10 + x11, data = dd)
  res <- mpi_feasible(m, region = list(x1 = c(-0.25, 0.25)))
  expect_equal(res$feas_ratio[res$factor == "x10"], 1)
  expect_equal(res$feas_ratio[res$factor == "x1"], 0.5)
})

test_that("feasible MPI is L1 only", {
  expect_error(mpi_feasible(c(x1 = 1), norm = "l2"), "attainable")
})

test_that("constraint output is validated", {
  b <- c(x1 = 1, x2 = 1)
  expect_error(mpi_feasible(b, constraint = function(d) TRUE, method = "lhs", size = 50, seed = 1),
               "one logical value per row")
  expect_error(mpi_feasible(b, constraint = function(d) d$x1 + d$x2, method = "lhs", size = 50, seed = 1),
               "one logical value per row")
  expect_error(mpi_feasible(b, constraint = function(d) ifelse(d$x1 > 0, NA, TRUE), method = "lhs", size = 50, seed = 1),
               "NA")
})
