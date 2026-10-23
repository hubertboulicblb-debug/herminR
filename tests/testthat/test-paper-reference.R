# herminR against the PAPER's own code. inst/extdata/paper_reference.csv was
# produced by the supplement's heatmapVisualization() (data-raw/paper_reference.R)
# for all 14 heatmaps of the supplement (7 per case study); herminR must match
# it cell by cell.

ref_path <- function() {
  f <- system.file("extdata", "paper_reference.csv", package = "herminR")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "extdata", "paper_reference.csv")
  f
}

test_that("worked example from the paper", {
  data(pastry_table1, package = "herminR")
  r <- mpi(pastry_table1$y4, factors = c("x1", "x2", "x3"))
  expect_equal(r$estimate, c(0, -0.655, 1.0185), tolerance = 1e-12)  # MPI_14, MPI_24, MPI_34
  s <- standardize_mpi(r, "colmax")
  expect_equal(s$estimate, c(0, -0.655 / 1.0185, 1), tolerance = 1e-12)  # Eq. (5), per response
})

test_that("every supplement heatmap is reproduced cell by cell", {
  ref <- utils::read.csv(ref_path(), stringsAsFactors = FALSE)
  data(pastry_table1, package = "herminR"); data(dss_sim, package = "herminR")
  studies <- list(
    pastry = list(cf = pastry_table1, f = paste0("x", 1:3), keep = c("x1", "x2"), rng = list(x3 = c(-0.1, 0.1))),
    sim = list(cf = dss_sim$coefs, f = paste0("Factor_", 1:11),
               keep = paste0("Factor_", c(1, 2, 4, 5, 6, 11)), rng = list(Factor_3 = c(-0.1, 0.1))))
  n_checked <- 0
  for (st in names(studies)) {
    x <- studies[[st]]
    s_base <- mpi_multi(x$cf, factors = x$f)
    a_base <- mpi_multi(x$cf, factors = x$f, abs = TRUE)
    only <- function(keep_term) lapply(x$cf, function(b) b[vapply(names(b), keep_term, logical(1))])
    is_main <- function(t) !grepl("[:^]", t)
    is_mq <- function(t) !grepl(":", t)
    mine <- list(
      main = mpi_multi(only(is_main), factors = x$f),
      main_quad = mpi_multi(only(is_mq), factors = x$f),
      all = s_base,
      all_perResp = standardize_mpi(s_base, "colmax"),
      abs_perResp = standardize_mpi(a_base, "colmax"),
      subset = standardize_mpi(mpi_subset(a_base, keep = x$keep), "colmax"),
      range = standardize_mpi(mpi_adjust_range(a_base, x$rng), "colmax"))
    for (fg in names(mine)) {
      r <- ref[ref$study == st & ref$figure == fg, ]
      m <- mine[[fg]]
      got <- m$estimate[match(paste(r$response, r$factor), paste(m$response, m$factor))]
      expect_false(anyNA(got))
      expect_equal(got, r$expected, tolerance = 1e-10, info = paste(st, fg))
      n_checked <- n_checked + nrow(r)
    }
  }
  expect_equal(n_checked, nrow(ref))
  expect_equal(nrow(unique(ref[c("study", "figure")])), 14)
})

test_that("mpi_paper_figures() returns the paper's values by default", {
  ref <- utils::read.csv(ref_path(), stringsAsFactors = FALSE)
  figs <- mpi_paper_figures()
  key <- list(fig1 = c("pastry", "abs_perResp"), fig2 = c("pastry", "range"),
              fig3 = c("sim", "abs_perResp"), fig4 = c("sim", "subset"))
  for (k in names(key)) {
    r <- ref[ref$study == key[[k]][1] & ref$figure == key[[k]][2], ]
    m <- figs[[k]]
    got <- m$estimate[match(paste(r$response, r$factor), paste(m$response, m$factor))]
    expect_equal(got, r$expected, tolerance = 1e-10, info = k)
  }
})
