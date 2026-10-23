test_that("paper worked example: coefficient attribution and MPI values", {
  data(pastry_table1, package = "herminR")
  b <- pastry_table1$y4
  # attribution as in paper Table 1 (response 4)
  expect_equal(b[["x3"]], 0.2294)      # main effect beta_34
  expect_equal(b[["x3^2"]], 0.4185)    # quadratic gamma_34
  expect_equal(b[["x2:x3"]], 0.3706)   # interaction delta_234
  expect_equal(b[["x2"]], -0.2844)     # main effect beta_24
  tm <- herminR:::.term_map(b)
  expect_equal(tm$powers[["x3^2"]], c(x3 = 2L))
  expect_equal(tm$powers[["x2:x3"]], c(x2 = 1L, x3 = 1L))
  r <- mpi(b, factors = "x1")
  expect_equal(r$estimate, c(0, -(0.2844 + 0.3706), 0.2294 + 0.4185 + 0.3706), tolerance = 1e-12)
  expect_equal(r$terms_contributed, c(0L, 2L, 3L))
  # the restricted-x3 follow-up only shrinks the interaction in x2's MPI
  adj <- mpi_adjust_range(r, list(x3 = c(-0.1, 0.1)))
  expect_equal(adj$estimate, c(0, -(0.2844 + 0.1 * 0.3706), 1.0185), tolerance = 1e-12)
})

test_that("L1 MPI equals the brute-force maximal one-unit change", {
  for (s in 1:15) {
    b <- rand_coefs(s, p = 3)
    r <- mpi(b, abs = TRUE)
    for (f in r$factor) {
      expect_equal(r$estimate[r$factor == f], brute_mpi(b, f), tolerance = 1e-8)
    }
  }
})

test_that("signed MPI follows the sign of the change at the maximiser", {
  r <- mpi(c(x1 = -0.5, "x1^2" = 0.8, "x1:x2" = 0.3, x2 = 0.1))
  expect_equal(r$estimate[1], -(0.5 + 0.8 + 0.3))
  expect_equal(r$direction[1], "decrease")
  r0 <- mpi(c("x1^2" = 0.8, "x1:x2" = 0.3, x2 = 0.1))
  expect_equal(r0$direction[r0$factor == "x1"], "either")
  expect_gt(r0$estimate[r0$factor == "x1"], 0)
  expect_equal(mpi(c(x1 = -0.5), abs = TRUE)$estimate, 0.5)
})

test_that("L2 and Linf norms aggregate per-term contributions", {
  b <- c("x1" = 3, "x1^2" = -4, "x1:x2" = 0)
  expect_equal(mpi(b, norm = "l2", abs = TRUE)$estimate[1], 5)
  expect_equal(mpi(b, norm = "linf", abs = TRUE)$estimate[1], 4)
  cmp <- mpi_compare_norms(b)
  expect_s3_class(cmp, "herminR_norms")
  expect_named(cmp, c("l1", "l2", "linf"))
})

test_that("se column is the covariance-aware delta-method SE", {
  d <- make_ccd()
  m <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  r <- mpi(m)
  V <- vcov(m)
  t2 <- c("x2", "I(x2^2)", "x1:x2", "x2:x3")
  g <- sign(coef(m)[t2])
  expect_equal(r$se[r$factor == "x2"], sqrt(drop(t(g) %*% V[t2, t2] %*% g)), tolerance = 1e-12)
})

test_that("coefficient data frame input works", {
  df <- data.frame(term = c("x1", "x1^2"), estimate = c(0.5, -0.2), se = c(0.1, 0.1))
  r <- mpi(df)
  expect_equal(r$estimate, 0.7)
  expect_equal(r$se, sqrt(0.02), tolerance = 1e-12)
})

test_that("mpi_multi aligns factors across responses with zeros", {
  r <- mpi_multi(list(A = c(x1 = 1, x2 = 1), B = c(x3 = 2)))
  expect_equal(nrow(r), 6)
  expect_equal(r$estimate[r$response == "B" & r$factor == "x1"], 0)
})

test_that("empty input returns an empty table", {
  expect_equal(nrow(mpi(c("(Intercept)" = 1))), 0)
})

test_that("non-L1 norms are labelled as contribution scores", {
  b <- c(x1 = 0.5, "x1^2" = 0.2, x2 = 1)
  expect_output(print(mpi(b, norm = "l2")), "contribution scores")
  expect_no_match(capture.output(print(mpi(b))), "contribution scores")
  p <- plot(mpi(b, norm = "linf"))
  expect_match(p$labels$subtitle, "not attainable")
  expect_match(p$labels$fill, "Linf score")
  expect_null(plot(mpi(b))$labels$subtitle)
})

test_that("no non-L1 plot is titled or labelled as MPI (all heatmap options)", {
  b <- c(x1 = 0.5, "x1^2" = 0.2, x2 = 1, "x1:x2" = -0.3)
  d <- make_ccd()
  fit <- lm(y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = d)
  # "not MPI" in a title is the intended disclaimer; anything else is not
  lab_text <- function(p) gsub("not MPI", "", unlist(p$labels[c("title", "fill", "x", "y", "caption", "shape")]))
  grid <- expand.grid(norm = c("l1", "l2", "linf"), panel = c("estimate", "lower", "upper", "significance"),
                      palette = c("ermine", "colour"), signed = c(TRUE, FALSE), transpose = c(TRUE, FALSE),
                      method = c("none", "colmax"), stringsAsFactors = FALSE)
  fits <- lapply(c(l1 = "l1", l2 = "l2", linf = "linf"), function(n) mpi(fit, norm = n, bounds = "delta"))
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    p <- plot(fits[[g$norm]], panel = g$panel, palette = g$palette, signed = g$signed,
              transpose = g$transpose, method = g$method)
    txt <- lab_text(p)
    info <- paste(unlist(g), collapse = "/")
    if (g$norm == "l1") {
      expect_equal(p$labels$title, "Maximal Possible Influence", info = info)
    } else {
      expect_false(any(grepl("MPI|Maximal Possible Influence", txt)), info = info)
      expect_match(p$labels$title, "not MPI", info = info)
    }
  }
  for (n in c("l2", "linf")) {
    for (p in list(mpi_waterfall(fits[[n]]), mpi_interval_plot(fits[[n]]),
                   plot(bootstrap_ranks(fit, n = 5, norm = n, seed = 1)))) {
      expect_false(any(grepl("MPI|Maximal Possible Influence", lab_text(p))), info = n)
    }
  }
  expect_equal(mpi_waterfall(mpi(b))$labels$y, "MPI")
  np <- plot(mpi_compare_norms(b))
  expect_equal(np$labels$title, "Factor influence summaries under different norms")
  sp <- suppressWarnings(mpi_selection_paths(lm(y ~ x1 + x2 + x3, d), alphas = 0.05, norm = "l2"))
  expect_false(grepl("MPI", plot(sp)$labels$title))
})
