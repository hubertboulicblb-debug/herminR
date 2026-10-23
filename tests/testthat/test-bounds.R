sample_ellipsoid_boundary <- function(b0, V, cc, n = 2e5, seed = 1) {
  set.seed(seed)
  g <- length(b0)
  U <- matrix(rnorm(n * g), n)
  U <- U / sqrt(rowSums(U^2))
  e <- eigen(V, symmetric = TRUE)
  L <- e$vectors %*% diag(sqrt(e$values), g) %*% t(e$vectors)
  sweep(sqrt(cc) * U %*% L, 2, b0, "+")
}

test_that("exact ellipsoid norm range matches brute force (g = 2, 3)", {
  nf <- list(l1 = function(B) rowSums(abs(B)), l2 = function(B) sqrt(rowSums(B^2)),
             linf = function(B) apply(abs(B), 1, max))
  cases <- list(
    list(b0 = c(0.8, -0.5), V = matrix(c(0.04, 0.015, 0.015, 0.03), 2)),
    list(b0 = c(0.3, 0.6, -0.4), V = 0.01 * (diag(3) + 0.5)),
    list(b0 = c(0.05, -0.02), V = diag(c(0.01, 0.02)))  # origin inside
  )
  for (cs in cases) {
    cc <- qchisq(0.95, length(cs$b0))
    B <- sample_ellipsoid_boundary(cs$b0, cs$V, cc)
    inside0 <- drop(t(cs$b0) %*% solve(cs$V) %*% cs$b0) <= cc
    for (n in names(nf)) {
      ex <- herminR:::.ellipsoid_norm_range(cs$b0, cs$V, cc, n)
      v <- nf[[n]](B)
      expect_gte(ex[2], max(v) - 1e-9)
      expect_lt(ex[2] - max(v), 2e-3)
      if (inside0) {
        expect_equal(ex[1], 0)
      } else {
        expect_lte(ex[1], min(v) + 1e-9)
        expect_lt(min(v) - ex[1], 2e-3)
      }
    }
  }
})

test_that("bound functions return ordered intervals around the estimate", {
  d <- make_ccd()
  m <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  for (fun in list(mpi_se, mpi_delta, mpi_ellipsoid)) {
    for (n in c("l1", "l2", "linf")) {
      b <- fun(m, norm = n)
      expect_true(all(b$lower <= b$estimate + 1e-12))
      expect_true(all(b$upper >= b$estimate - 1e-12))
      expect_true(all(b$lower >= 0))
    }
  }
  r <- mpi(m, bounds = "ellipsoid")
  expect_false(anyNA(r$lower))
})

test_that("k_adj applies a Bonferroni t quantile", {
  d <- make_ccd()
  m <- lm(y ~ x1 + I(x1^2) + x2, data = d)
  b <- mpi_se(m, k_adj = TRUE)
  se <- sqrt(diag(vcov(m)))
  k <- qt(1 - 0.05 / (2 * 2), df.residual(m))
  up <- sum(abs(coef(m)[c("x1", "I(x1^2)")]) + k * se[c("x1", "I(x1^2)")])
  expect_equal(b$upper[b$factor == "x1"], unname(up), tolerance = 1e-12)
})

test_that("missing covariance gives NA bounds with a warning", {
  expect_warning(b <- mpi_delta(c(x1 = 1, x2 = 2)), "vcov")
  expect_true(all(is.na(b$lower)))
})

# Independent check: multistart local optimisation on the ellipsoid boundary.
indep_range <- function(b0, V, cc, norm, starts = 25) {
  nf <- switch(norm, l1 = function(b) sum(abs(b)), l2 = function(b) sqrt(sum(b^2)),
               linf = function(b) max(abs(b)))
  g <- length(b0); e <- eigen(V, symmetric = TRUE)
  L <- e$vectors %*% diag(sqrt(pmax(e$values, 0)), g) %*% t(e$vectors)
  pt <- function(v) b0 + sqrt(cc) * drop(L %*% (v / sqrt(sum(v^2))))
  hi <- -Inf; lo <- Inf
  set.seed(7)
  for (s in seq_len(starts)) {
    v0 <- rnorm(g)
    h <- optim(v0, function(v) -nf(pt(v)), control = list(maxit = 4000, reltol = 1e-14))
    h <- optim(h$par, function(v) -nf(pt(v)), method = "BFGS")
    l <- optim(v0, function(v) nf(pt(v)), control = list(maxit = 4000, reltol = 1e-14))
    l <- optim(l$par, function(v) nf(pt(v)), method = "BFGS")
    hi <- max(hi, -h$value); lo <- min(lo, l$value)
  }
  if (drop(t(b0) %*% solve(V, b0)) <= cc) lo <- 0
  c(lo, hi)
}

rand_cov <- function(g, cond, seed) {
  set.seed(seed)
  Q <- qr.Q(qr(matrix(rnorm(g * g), g)))
  0.02 * Q %*% diag(exp(seq(0, -log(cond), length.out = g)), g) %*% t(Q)
}

test_that("ellipsoid solver handles hard numerical cases", {
  skip_on_cran()
  cases <- list(
    near_singular = list(b0 = c(0.5, -0.3, 0.2), V = rand_cov(3, 1e8, 1)),
    repeated_eig = list(b0 = c(0.4, 0.4, 0), V = 0.01 * diag(3)),
    trs_hard_case = list(b0 = c(0.3, 0, 0), V = diag(c(0.01, 0.04, 0.04))),
    zero_coefs = list(b0 = c(0, 0.6, 0, -0.2), V = rand_cov(4, 50, 2)),
    g6 = list(b0 = c(0.5, -0.2, 0.3, 0.05, -0.4, 0.1), V = rand_cov(6, 100, 3)),
    g8 = list(b0 = c(0.9, -0.2, 0.3, 0.05, -0.4, 0.1, 0.2, -0.6), V = rand_cov(8, 20, 4)),
    centre_zero = list(b0 = c(0, 0, 0), V = rand_cov(3, 10, 5))
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    cc <- qchisq(0.95, length(cs$b0))
    for (n in c("l1", "l2", "linf")) {
      # no duality-gap warning: certificates are tight
      expect_no_warning(ex <- herminR:::.ellipsoid_norm_range(cs$b0, cs$V, cc, n))
      ind <- indep_range(cs$b0, cs$V, cc, n)
      # exact max >= any point found, and the optimiser reaches it
      expect_gte(ex[2], ind[2] - 1e-9)
      expect_lt(ex[2] - ind[2], 1e-6)
      # exact min <= any point found (convex problem), and agrees closely
      expect_lte(ex[1], ind[1] + 1e-9)
      expect_lt(ind[1] - ex[1], 1e-4)
    }
  }
})

wilson <- function(x, n, conf = 0.99) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(centre - half, centre + half)
}

test_that("coverage: delta ~ nominal, ellipsoid not below nominal (Wilson 99%)", {
  skip_on_cran()
  set.seed(42)
  d <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), x3 = c(-1, 0, 1))
  b_true <- c(x1 = 0.6, "x1^2" = 0.4, "x1:x2" = -0.5)
  true <- sum(abs(b_true))
  f <- y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2)
  mu <- with(d, 0.6 * x1 + 0.4 * x1^2 - 0.5 * x1 * x2 + 0.3 * x2 + 0.2 * x3)
  n_rep <- 400
  hit_d <- hit_e <- logical(n_rep)
  for (i in seq_len(n_rep)) {
    d$y <- mu + rnorm(nrow(d), sd = 0.15)
    m <- lm(f, data = d)
    bd <- mpi_delta(m); be <- mpi_ellipsoid(m)
    hit_d[i] <- bd$lower[1] <= true && true <= bd$upper[1]
    hit_e[i] <- be$lower[1] <= true && true <= be$upper[1]
  }
  wd <- wilson(sum(hit_d), n_rep)
  we <- wilson(sum(hit_e), n_rep)
  # delta: the 99% Wilson interval must overlap the band [0.93, 0.97]
  expect_true(wd[2] >= 0.93 && wd[1] <= 0.97)
  # ellipsoid (simultaneous, conservative): not significantly below 0.95
  expect_gte(we[2], 0.95)
})

test_that("pseudo_se = TRUE is refused rather than silently ignored", {
  d <- make_ccd()
  m <- lm(y ~ x1 + x2, data = d)
  expect_error(mpi_se(m, pseudo_se = TRUE), "not implemented")
  expect_error(mpi_se(c(x1 = 1), pseudo_se = TRUE), "not implemented")
})

test_that("chunked L1 enumeration equals full enumeration; limit is enforced", {
  set.seed(11)
  g <- 13
  b0 <- rnorm(g, sd = 0.3)
  A <- matrix(rnorm(g * g), g); V <- 0.01 * crossprod(A) / g + 0.001 * diag(g)
  cc <- qchisq(0.95, g)
  S <- as.matrix(expand.grid(rep(list(c(-1, 1)), g)))
  full <- max(drop(S %*% b0) + sqrt(cc * rowSums((S %*% V) * S)))
  expect_equal(herminR:::.ellipsoid_norm_range(b0, V, cc, "l1")[2], full, tolerance = 1e-12)
  expect_error(herminR:::.ellipsoid_norm_range(b0, V, cc, "l1", max_terms = 12), "max_terms")
})

test_that("singular covariance blocks give NA bounds with a warning", {
  V <- matrix(c(1, 1, 1, 1), 2) * 0.01
  expect_warning(r <- herminR:::.ellipsoid_norm_range(c(0.5, 0.2), V, 6, "l2"), "singular")
  expect_true(all(is.na(r)))
})
