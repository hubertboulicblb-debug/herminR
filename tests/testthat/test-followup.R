test_that("mpi_adjust_range scales partners' interactions, not own effects", {
  b <- c(x1 = 0.5, x2 = -0.3, x3 = 0.2, "x3^2" = 0.1, "x2:x3" = 0.4, "x1:x3" = 0.1)
  base <- mpi(b, abs = TRUE)
  adj <- mpi_adjust_range(base, list(x3 = c(-0.1, 0.1)))
  get <- function(r, f) r$estimate[r$factor == f]
  expect_equal(get(adj, "x3"), get(base, "x3"))
  expect_equal(get(adj, "x2"), 0.3 + 0.1 * 0.4)
  expect_equal(get(adj, "x1"), 0.5 + 0.1 * 0.1)
})

test_that("mpi_adjust_range matches brute force for asymmetric ranges", {
  for (s in 1:10) {
    b <- rand_coefs(s, p = 3)
    rng <- list(x3 = c(0.2, 0.9))
    adj <- mpi_adjust_range(mpi(b, abs = TRUE), rng)
    for (f in c("x1", "x2")) {
      expect_equal(adj$estimate[adj$factor == f], brute_mpi(b, f, rng), tolerance = 1e-8)
    }
  }
})

test_that("mpi_adjust_range warns on extrapolation", {
  expect_warning(mpi_adjust_range(mpi(c(x1 = 1, "x1:x2" = 1)), list(x2 = c(-1.5, 1.5))), "extrapolates")
})

test_that("mpi_subset with all factors is the identity; centre removes partners", {
  b <- c(x1 = 0.5, x2 = -0.3, x3 = 0.2, "x2^2" = 0.1, "x2:x3" = 0.4, "x1:x2" = 0.1)
  base <- mpi(b)
  expect_equal(mpi_subset(base, keep = c("x1", "x2", "x3"))$estimate, base$estimate)
  s <- mpi_subset(base, keep = c("x1", "x2"))
  expect_equal(s$factor, c("x1", "x2"))
  expect_equal(abs(s$estimate[s$factor == "x2"]), 0.3 + 0.1 + 0.1)
  fav <- mpi_subset(base, keep = 1:2, others = "favourable")
  expect_equal(fav$estimate, base$estimate[1:2])
  expect_error(mpi_subset(base, keep = "x9"), "Unknown")
})

test_that("follow-ups stack and work on multi-response objects", {
  r <- mpi_multi(list(A = c(x1 = 1, "x1:x2" = 0.5, x3 = 0.2), B = c(x2 = 2, "x2:x3" = 1)))
  s <- mpi_subset(mpi_adjust_range(r, list(x3 = c(-0.5, 0.5))), keep = c("x1", "x2"))
  expect_equal(s$estimate[s$response == "B" & s$factor == "x2"], 2)  # x2:x3 removed by centring x3
  expect_equal(s$estimate[s$response == "A" & s$factor == "x1"], 1.5)
  s2 <- mpi_adjust_range(mpi_subset(r, keep = c("x2", "x3")), list(x3 = c(-0.5, 0.5)))
  expect_equal(s2$estimate[s2$response == "B" & s2$factor == "x2"], 2.5)
})
