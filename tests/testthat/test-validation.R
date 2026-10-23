# Independent validation ------------------------------------------------------
#
# Every expected value in this file is obtained WITHOUT herminR's internals:
# closed-form results, stats::predict() on the fitted model, explicit
# sigma^2 (X'X)^-1 algebra, or Monte Carlo sampling of the confidence region.

# A. Ellipsoid bounds with known closed-form answers ---------------------------

test_that("one coefficient: interval is [max(0, |b| - r), |b| + r] for every norm", {
  for (b in c(0.8, -0.8, 0.1)) {
    v <- 0.04; cc <- qchisq(0.95, 1); r <- sqrt(cc * v)
    for (n in c("l1", "l2", "linf")) {
      ex <- herminR:::.ellipsoid_norm_range(b, matrix(v), cc, n)
      expect_equal(ex, c(max(0, abs(b) - r), abs(b) + r), tolerance = 1e-10, info = paste(b, n))
    }
  }
})

test_that("spherical region: closed-form L2, L1 and Linf extrema", {
  # V = s2 * I  =>  region is a ball of radius r around b0
  b0 <- c(0.9, -0.7, 0.6, 0.8)
  g <- length(b0); s2 <- 0.01; cc <- qchisq(0.95, g); r <- sqrt(cc * s2)
  V <- s2 * diag(g)
  # L2: ||b0|| -/+ r
  expect_equal(herminR:::.ellipsoid_norm_range(b0, V, cc, "l2"),
               c(sqrt(sum(b0^2)) - r, sqrt(sum(b0^2)) + r), tolerance = 1e-10)
  # L1 max: ||b0||_1 + r sqrt(g) (at b0 + r sign(b0)/sqrt(g))
  # L1 min: ||b0||_1 - r sqrt(g), valid because min |b0_i| >= r / sqrt(g)
  stopifnot(min(abs(b0)) >= r / sqrt(g))
  expect_equal(herminR:::.ellipsoid_norm_range(b0, V, cc, "l1"),
               c(sum(abs(b0)) - r * sqrt(g), sum(abs(b0)) + r * sqrt(g)), tolerance = 1e-8)
  # Linf max: max |b0_i| + r
  expect_equal(herminR:::.ellipsoid_norm_range(b0, V, cc, "linf")[2], max(abs(b0)) + r, tolerance = 1e-10)
})

test_that("axis-aligned region: closed-form L-infinity minimum", {
  # b0 = (a, 0, 0), diagonal V: every point has |b_1| >= a - r1, and
  # (a - r1, 0, 0) attains it, so min ||b||_inf = a - r1.
  a <- 1; V <- diag(c(0.04, 0.09, 0.01)); cc <- qchisq(0.95, 3)
  expect_equal(herminR:::.ellipsoid_norm_range(c(a, 0, 0), V, cc, "linf")[1],
               a - sqrt(cc * 0.04), tolerance = 1e-7)
})

# B. Ill-conditioned and higher-dimensional models: Monte Carlo of the region ---

mc_norm_range <- function(b0, V, cc, norm, n = 4e5, seed = 3) {
  set.seed(seed)
  g <- length(b0)
  U <- matrix(rnorm(n * g), n); U <- U / sqrt(rowSums(U^2))
  L <- t(chol(V + diag(1e-14, g)))
  B <- sweep(sqrt(cc) * U %*% t(L), 2, b0, "+")
  v <- switch(norm, l1 = rowSums(abs(B)), l2 = sqrt(rowSums(B^2)), linf = apply(abs(B), 1, max))
  range(v)
}

test_that("near-collinear design: bounds contain every sampled point and are tight", {
  skip_on_cran()
  set.seed(8)
  n <- 30
  x1 <- runif(n, -1, 1)
  x2 <- x1 + rnorm(n, sd = 0.02)          # cor(x1, x2) ~ 0.9997
  x3 <- runif(n, -1, 1)
  y <- 1 + x1 + 0.5 * x2 + 0.8 * x1 * x3 + 0.4 * x1^2 + rnorm(n, sd = 0.2)
  m <- lm(y ~ x1 + x2 + x3 + x1:x3 + I(x1^2))
  expect_gt(kappa(model.matrix(m)), 50)
  V <- vcov(m)
  t1 <- c("x1", "I(x1^2)", "x1:x3")
  cc <- 3 * qf(0.95, 3, df.residual(m))
  for (nm in c("l1", "l2", "linf")) {
    ex <- mpi_ellipsoid(m, norm = nm)
    lo_hi <- c(ex$lower[ex$factor == "x1"], ex$upper[ex$factor == "x1"])
    mc <- mc_norm_range(coef(m)[t1], V[t1, t1], cc, nm)
    expect_lte(lo_hi[1], mc[1] + 1e-9)        # lower bound below every sampled point
    expect_gte(lo_hi[2], mc[2] - 1e-9)        # upper bound above every sampled point
    expect_lt(mc[1] - lo_hi[1], 0.02 * mc[2])  # ...and tight
    expect_lt(lo_hi[2] - mc[2], 0.02 * mc[2])
  }
})

test_that("12-term factor group (11-factor model): L1 maximum vs Monte Carlo", {
  skip_on_cran()
  set.seed(9)
  g <- 12
  b0 <- rnorm(g, sd = 0.4)
  A <- matrix(rnorm(g * g), g); V <- 0.004 * crossprod(A) / g + 0.0005 * diag(g)
  cc <- qchisq(0.95, g)
  ex <- herminR:::.ellipsoid_norm_range(b0, V, cc, "l1")
  mc <- mc_norm_range(b0, V, cc, "l1", n = 2e5)
  expect_gte(ex[2], mc[2] - 1e-9)
  expect_lte(ex[1], mc[1] + 1e-9)
  # independent exact L1 maximum: max over sign vectors of s'b0 + sqrt(cc s'Vs)
  S <- as.matrix(expand.grid(rep(list(c(-1, 1)), g)))
  expect_equal(ex[2], max(S %*% b0 + sqrt(cc * rowSums((S %*% V) * S))), tolerance = 1e-12)
})

test_that("aliased coefficients: bounds use only estimable terms", {
  d <- make_ccd()
  d$x4 <- d$x1
  m <- lm(y ~ x1 + x4 + x2 + I(x1^2), data = d)
  expect_warning(r <- mpi(m, bounds = "ellipsoid"), "Aliased")
  expect_false(anyNA(r$lower[r$factor %in% c("x1", "x2")]))
  expect_equal(r$upper[r$factor == "x4"], 0)
})

# C. Delta-method SE from explicit sigma^2 (X'X)^-1 ----------------------------

test_that("delta SE equals the explicit linear-model formula", {
  d <- make_ccd()
  X <- with(d, cbind(1, x1, x2, x3, x1^2, x2^2, x3^2, x1 * x2, x1 * x3, x2 * x3))
  beta <- solve(crossprod(X), crossprod(X, d$y))
  res <- d$y - X %*% beta
  s2 <- sum(res^2) / (nrow(X) - ncol(X))
  V <- s2 * solve(crossprod(X))
  # x2 group: x2 (col 3), x2^2 (6), x1:x2 (8), x2:x3 (10); L1 gradient = signs
  j <- c(3, 6, 8, 10)
  se_manual <- sqrt(drop(t(sign(beta[j])) %*% V[j, j] %*% sign(beta[j])))
  m <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  r <- mpi_delta(m)
  expect_equal(r$se[r$factor == "x2"], se_manual, tolerance = 1e-10)
  expect_equal(r$estimate[r$factor == "x2"], sum(abs(beta[j])), tolerance = 1e-10)
  # interval uses t with residual df
  tq <- qt(0.975, nrow(X) - ncol(X))
  expect_equal(r$upper[r$factor == "x2"], sum(abs(beta[j])) + tq * se_manual, tolerance = 1e-10)
})

# D. MPI and feasible MPI against stats::predict() ------------------------------

predict_max_change <- function(model, f, factors, ranges = list(), constraint = NULL,
                               grid = 21, nuisance = list()) {
  rr <- lapply(setNames(factors, factors), function(k) if (!is.null(ranges[[k]])) ranges[[k]] else c(-1, 1))
  d_step <- min(1, diff(rr[[f]]))
  axes <- lapply(rr, function(r) seq(r[1], r[2], length.out = grid))
  axes[[f]] <- seq(rr[[f]][1], rr[[f]][2] - d_step, length.out = 4 * grid + 1)
  G <- expand.grid(axes)
  G1 <- G; G1[[f]] <- G[[f]] + d_step
  for (k in names(nuisance)) { G[[k]] <- nuisance[[k]]; G1[[k]] <- nuisance[[k]] }
  ok <- if (is.null(constraint)) rep(TRUE, nrow(G)) else constraint(G) & constraint(G1)
  dlt <- predict(model, G1[ok, ]) - predict(model, G[ok, ])
  max(abs(dlt))
}

test_that("L1 MPI equals the largest one-unit change of predict() (lm, rsm, blocked)", {
  skip_on_cran()
  d <- make_ccd()
  models <- list(
    lm = lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d),
    rsm = rsm::rsm(y ~ SO(x1, x2, x3), data = d)
  )
  data(pastrydough, package = "herminR")
  models$blocked <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  for (nm in names(models)) {
    m <- models[[nm]]
    r <- mpi(m, abs = TRUE)
    nuis <- if (nm == "blocked") list(block = factor("1", levels = levels(pastrydough$block))) else list()
    for (f in r$factor) {
      expect_equal(r$estimate[r$factor == f],
                   predict_max_change(m, f, r$factor, nuisance = nuis), tolerance = 1e-8, info = paste(nm, f))
    }
  }
})

test_that("feasible MPI (box and constraint) agrees with predict() on a grid", {
  skip_on_cran()
  d <- make_ccd()
  m <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  region <- list(x2 = c(-0.2, 0.9), x3 = c(-1, 0.3))
  ex <- mpi_feasible(m, region = region)
  for (f in ex$factor) {
    expect_equal(ex$estimate[ex$factor == f],
                 predict_max_change(m, f, ex$factor, region), tolerance = 1e-8, info = f)
  }
  con <- function(g) g$x1 + g$x2 <= 0.5
  gr <- mpi_feasible(m, constraint = con, method = "grid", size = 31^3)
  for (f in gr$factor) {
    ref <- predict_max_change(m, f, gr$factor, constraint = con, grid = 31)
    expect_lte(gr$estimate[gr$factor == f], ref + 1e-8)       # never above the supremum
    expect_gt(gr$estimate[gr$factor == f], 0.97 * ref)
  }
})

# E. More closed forms: diagonal covariance (incl. near-singular), zero centre --

test_that("diagonal covariance: closed-form L1 extrema, including near-singular", {
  cc <- qchisq(0.95, 4)
  for (V in list(diag(c(0.02, 0.01, 0.03, 0.005)), diag(c(0.02, 1e-12, 0.03, 1e-10)))) {
    for (b0 in list(c(0.9, -0.8, 1.1, 0.7), c(1e-3, -2e-3, 0.9, 5e-4))) {
      # max over sign vectors: s'b0 + sqrt(cc * tr V) (s'Vs = tr V for diagonal V)
      ex <- herminR:::.ellipsoid_norm_range(b0, V, cc, "l1")
      expect_equal(ex[2], sum(abs(b0)) + sqrt(cc * sum(diag(V))), tolerance = 1e-10)
      # min: ||b0||_1 - sqrt(cc tr V), valid when the minimiser stays in b0's orthant
      if (all(abs(b0) >= sqrt(cc) * diag(V) / sqrt(sum(diag(V))))) {
        expect_equal(ex[1], sum(abs(b0)) - sqrt(cc * sum(diag(V))), tolerance = 1e-8)
      }
    }
  }
})

test_that("region centred on zero: minima 0, closed-form maxima", {
  g <- 5; s2 <- 0.02; cc <- qchisq(0.95, g); r <- sqrt(cc * s2)
  V <- s2 * diag(g)
  expect_equal(herminR:::.ellipsoid_norm_range(rep(0, g), V, cc, "l2"), c(0, r), tolerance = 1e-10)
  expect_equal(herminR:::.ellipsoid_norm_range(rep(0, g), V, cc, "l1"), c(0, r * sqrt(g)), tolerance = 1e-10)
  expect_equal(herminR:::.ellipsoid_norm_range(rep(0, g), V, cc, "linf"), c(0, r), tolerance = 1e-10)
  Vd <- diag(c(0.01, 0.05, 1e-9, 0.02, 0.03))
  expect_equal(herminR:::.ellipsoid_norm_range(rep(0, g), Vd, cc, "l2")[2], sqrt(cc * 0.05), tolerance = 1e-10)
})
