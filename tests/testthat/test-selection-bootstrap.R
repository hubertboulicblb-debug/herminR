test_that("selection paths work on rsm SO() and respect hierarchy", {
  skip_if_not_installed("rsm")
  d <- make_ccd(sigma = 0.3)
  m <- rsm::rsm(y ~ SO(x1, x2, x3), data = d)
  sp <- mpi_selection_paths(m, alphas = c(0.01, 0.1), top = 2)
  info <- attr(sp, "paths")
  expect_true(all(vapply(info, `[[`, character(1), "status") == "ok"))
  for (p in info) {
    tr <- p$terms
    if ("I(x1^2)" %in% tr || "x1:x2" %in% tr || "x1:x3" %in% tr) expect_true("x1" %in% tr)
    if ("x2:x3" %in% tr) expect_true(all(c("x2", "x3") %in% tr))
  }
  expect_equal(sort(unique(sp$factor)), c("x1", "x2", "x3"))
  expect_equal(nrow(sp), 3 * 3)
})

test_that("factors removed on a path stay in the output with MPI 0", {
  set.seed(4)
  d <- make_ccd()
  d$y <- 1 + 2 * d$x1 + rnorm(nrow(d), 0, 0.3)
  sp <- suppressWarnings(mpi_selection_paths(lm(y ~ x1 + x2 + x3, d), alphas = 0.01, top = 1))
  expect_true(all(sp$estimate[!sp$in_model] == 0))
  expect_equal(unique(sp$stability[sp$factor == "x1"]), 1)
})

test_that("keep terms are never dropped", {
  d <- make_ccd()
  sp <- suppressWarnings(mpi_selection_paths(lm(y ~ x1 + x2 + x3 + I(x3^2), d), keep = "I(x3^2)", alphas = 0.5))
  expect_true(all(vapply(attr(sp, "paths"), function(p) "I(x3^2)" %in% p$terms, logical(1))))
  expect_error(mpi_selection_paths(lm(y ~ x1, d), keep = "x9"), "Unknown")
})

test_that("bootstrap_ranks works on rsm models and is reproducible", {
  skip_if_not_installed("rsm")
  d <- make_ccd()
  m <- rsm::rsm(y ~ SO(x1, x2, x3), data = d)
  a <- bootstrap_ranks(m, n = 30, seed = 1)
  b <- bootstrap_ranks(m, n = 30, seed = 1)
  expect_equal(a, b)
  expect_equal(attr(a, "n_ok"), 30)
  expect_true(all(a$rank_lower <= a$rank_mean & a$rank_mean <= a$rank_upper))
  expect_message(p <- bootstrap_ranks(m, n = 10, type = "pairs", seed = 1), "case")
  expect_equal(attr(p, "type"), "case")
})

test_that("blocked models bootstrap with blocks kept as nuisance", {
  data(pastrydough, package = "herminR")
  m <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  r <- bootstrap_ranks(m, n = 20, seed = 2)
  expect_equal(r$factor, c("x1", "x2", "x3"))
})

test_that("models fitted inside a function can be bootstrapped and re-selected", {
  fit_in_function <- function() {
    dd <- make_ccd()
    lm(y ~ x1 + x2 + I(x1^2) + x1:x2, data = dd)
  }
  m <- fit_in_function()
  expect_equal(attr(bootstrap_ranks(m, n = 10, seed = 1), "n_ok"), 10)
  expect_s3_class(suppressWarnings(mpi_selection_paths(m, alphas = 0.05)), "herminR_selection")
})
