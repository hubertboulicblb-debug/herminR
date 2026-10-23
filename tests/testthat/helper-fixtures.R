# Shared fixtures --------------------------------------------------------------
make_ccd <- function(seed = 1, sigma = 0.1) {
  set.seed(seed)
  d <- expand.grid(x1 = c(-1, 0, 1), x2 = c(-1, 0, 1), x3 = c(-1, 0, 1))
  d$y <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + 0.4 * d$x3 + 0.3 * d$x1^2 + 0.6 * d$x2 * d$x3 +
    rnorm(nrow(d), 0, sigma)
  d
}

# Brute force: max |y(a+1, x) - y(a, x)| over a in [-1, 0] and x in the box
brute_mpi <- function(b, f, ranges = NULL, n_a = 201) {
  tm <- herminR:::.term_map(b)
  fs <- tm$factors
  rr <- herminR:::.full_ranges(fs, if (is.null(ranges)) list() else ranges)
  others <- setdiff(fs, f)
  # second-order change is linear in the other factors -> vertices suffice;
  # add random interior points as a check that nothing beats the vertices
  V <- expand.grid(lapply(rr[others], identity))
  set.seed(99)
  R <- as.data.frame(lapply(rr[others], function(r) runif(200, r[1], r[2])))
  X <- rbind(V, R)
  best <- c(hi = -Inf, lo = Inf)
  for (a in seq(-1, 0, length.out = n_a)) {
    X0 <- X; X0[[f]] <- a
    X1 <- X; X1[[f]] <- a + 1
    dlt <- herminR:::.poly_eval(tm, X1) - herminR:::.poly_eval(tm, X0)
    best <- c(hi = max(best["hi"], dlt), lo = min(best["lo"], dlt))
  }
  max(best["hi"], -best["lo"])
}

rand_coefs <- function(seed, p = 3) {
  set.seed(seed)
  f <- paste0("x", seq_len(p))
  nm <- c(f, paste0(f, "^2"), apply(combn(f, 2), 2, paste, collapse = ":"))
  setNames(round(rnorm(length(nm)), 3), nm)
}
