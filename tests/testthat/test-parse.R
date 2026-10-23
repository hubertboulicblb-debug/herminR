test_that("rsm SO(), FO/TWI/PQ and lm I() parametrisations give identical MPI", {
  skip_if_not_installed("rsm")
  d <- make_ccd()
  m_lm <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  m_so <- rsm::rsm(y ~ SO(x1, x2, x3), data = d)
  m_sep <- rsm::rsm(y ~ FO(x1, x2, x3) + TWI(x1, x2, x3) + PQ(x1, x2, x3), data = d)
  a <- mpi(m_lm); b <- mpi(m_so); c <- mpi(m_sep)
  expect_equal(a$factor, c("x1", "x2", "x3"))
  expect_equal(a$estimate, b$estimate, tolerance = 1e-10)
  expect_equal(a$estimate, c$estimate, tolerance = 1e-10)
  expect_equal(a$se, b$se, tolerance = 1e-10)
  expect_equal(b$terms_contributed, c(4L, 4L, 4L))
})

test_that("raw poly() is parsed; orthogonal poly() is a hard error", {
  d <- make_ccd()
  m_raw <- lm(y ~ poly(x1, 2, raw = TRUE) + x2 * x3, data = d)
  m_i <- lm(y ~ x1 + I(x1^2) + x2 * x3, data = d)
  expect_equal(mpi(m_raw)$estimate, mpi(m_i)$estimate, tolerance = 1e-10)
  m_orth <- lm(y ~ poly(x1, 2) + x2, data = d)
  expect_error(mpi(m_orth), "Orthogonal")
})

test_that("multi-variable raw poly() maps column suffixes to exponents", {
  d <- make_ccd()
  m_p <- lm(y ~ poly(x2, x3, degree = 2, raw = TRUE) + x1 + I(x1^2), data = d)
  m_i <- lm(y ~ x2 * x3 + I(x2^2) + I(x3^2) + x1 + I(x1^2), data = d)
  expect_equal(mpi(m_p)$estimate, mpi(m_i)$estimate, tolerance = 1e-10)
})

test_that("factor names ending in 2 or sharing prefixes are not confused", {
  tm <- herminR:::.term_map(c(x1 = 1, x2 = -2, x12 = 3, "x1:x12" = 0.5, "x2^2" = 1))
  expect_equal(tm$powers[["x2"]], c(x2 = 1L))
  expect_equal(tm$powers[["x12"]], c(x12 = 1L))
  expect_equal(tm$powers[["x2^2"]], c(x2 = 2L))
  expect_equal(tm$powers[["x1:x12"]], c(x1 = 1L, x12 = 1L))
  r <- mpi(c(x1 = -1, x2 = -1, "x1^2" = 0.2, "x2^2" = 0.2))
  expect_equal(r$estimate, c(-1.2, -1.2))
})

test_that("block / categorical terms are nuisance, not factors", {
  data(pastrydough, package = "herminR")
  m <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  r <- mpi(m)
  expect_equal(r$factor, c("x1", "x2", "x3"))
  expect_length(attr(r, "terms")$nuisance, 6)
})

test_that("non-monomial terms error; higher order needs max_order", {
  expect_error(mpi(c("log(x1)" = 1)), "not a monomial")
  expect_error(mpi(c(x1 = 1, "x1:x2:x3" = 1)), "max_order")
  expect_warning(r <- mpi(c(x1 = 1, "x1:x2:x3" = 1), max_order = 3), "order > 2")
  expect_true(is.na(r$direction[r$factor == "x1"]))
})

test_that("aliased coefficients are reported and ignored", {
  d <- make_ccd()
  d$x4 <- d$x1  # aliased with x1
  m <- lm(y ~ x1 + x4 + x2, data = d)
  expect_warning(r <- mpi(m), "Aliased")
  expect_equal(r$estimate[r$factor == "x4"], 0)
})

test_that("uncoded data triggers a warning", {
  d <- make_ccd()
  d$temp <- 150 + 25 * d$x1
  m <- lm(y ~ temp + x2, data = d)
  expect_warning(mpi(m), "coded units")
})
