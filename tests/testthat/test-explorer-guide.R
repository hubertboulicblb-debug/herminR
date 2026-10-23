# Guided experience: stage definitions, plain-language findings, server wiring

test_that("six stages, each pointing at an existing tab", {
  st <- herminR:::guide_stages()
  expect_length(st, 6)
  expect_true(all(vapply(st, function(s) all(c("tab", "short", "title", "question", "what", "read", "get") %in% names(s)),
                         logical(1))))
  skip_if_not_installed("shiny")
  html <- as.character(explorer_ui())
  for (s in st) expect_match(html, sprintf('data-value="%s"', gsub("&", "&amp;", s$tab)), fixed = TRUE)
  expect_match(html, 'data-value="start"', fixed = TRUE)
})

test_that("MPI findings state the paper's numbers correctly", {
  m <- mpi_multi(pastry_table1, factors = c("x1", "x2", "x3"))
  txt <- herminR:::guide_mpi_text(m)
  expect_true(any(grepl("y4: x3 has the largest influence; one coded unit more of x3 can change y4 by up to 1.02 (the response goes up). Next: x2 (0.655). No influence: x1.", txt, fixed = TRUE)))
  expect_true(any(grepl("y1: x2 has the largest influence; one coded unit more of x2 can change y1 by up to 3.48 (the response goes down)", txt, fixed = TRUE)))
  l2 <- herminR:::guide_mpi_text(mpi_multi(pastry_table1, norm = "l2", factors = c("x1", "x2", "x3")))
  expect_true(any(grepl("highest L2 score", l2)))
  expect_true(any(grepl("not achievable changes", l2)))
  many <- herminR:::guide_mpi_text(mpi_multi(dss_sim$coefs, abs = TRUE))
  expect_match(many[1], "^Across 20 responses, Factor_\\d+ is the most influential factor for \\d+ of them")
  zero <- herminR:::guide_mpi_text(mpi(c(x1 = 0, x2 = 0)))
  expect_match(zero[1], "no factor has any influence")
})

test_that("interval, rank and selection findings follow the results", {
  data(pastrydough, package = "herminR")
  fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  u <- mpi(fit, bounds = "ellipsoid")
  expect_equal(herminR:::guide_interval_text(u),
               "y: x2 and x3 are clearly influential (interval above 0); for x1 the data cannot rule out no influence.")
  mk <- function(top1, lo = c(1, 2), hi = c(1, 2)) {
    structure(tibble::tibble(factor = c("a", "b"), rank_lower = lo, rank_upper = hi,
                             top1_frac = top1), n_ok = 100L)
  }
  expect_match(herminR:::guide_rank_text(mk(c(0.95, 0.05)))[1], "a ranked first in 95% of 100 replays: a reliable leader")
  expect_match(herminR:::guide_rank_text(mk(c(0.7, 0.3)))[1], "likely but not certain leader \\(next: b, 30%\\)")
  expect_match(herminR:::guide_rank_text(mk(c(0.5, 0.5), lo = c(1, 1), hi = c(2, 2)))[1], "No stable leader")
  expect_match(herminR:::guide_rank_text(mk(c(0.5, 0.5), lo = c(1, 1), hi = c(2, 2)))[2], "Rank can change for a and b")
  sp <- tibble::tibble(factor = rep(c("a", "b"), 2), path = rep(c("aic", "p0.05"), each = 2),
                       rank = c(1, 2, 2, 1), in_model = c(TRUE, TRUE, TRUE, FALSE))
  expect_match(herminR:::guide_selection_text(sp)[1], "The leader depends on the model choice: a on aic, b on p0.05")
  expect_match(herminR:::guide_selection_text(sp)[2], "b is removed entirely")
})

test_that("follow-up and feasible findings report the right changes", {
  before <- mpi_multi(pastry_table1, abs = TRUE, factors = c("x1", "x2", "x3"))
  after <- standardize_mpi(mpi_adjust_range(before, list(x3 = c(-0.1, 0.1))), "colmax")
  txt <- herminR:::guide_followup_text(before, after)
  expect_true("y4: influence of x2 goes from 0.655 to 0.321 (-51%)." %in% txt)
  sub <- herminR:::guide_followup_text(before, mpi_subset(before, keep = c("x1", "x2")))
  expect_equal(sub[1], "Dropped and held at the centre: x3.")
  expect_match(herminR:::guide_followup_text(before, before), "No influence changes")
  f <- mpi_feasible(pastry_table1$y4, region = list(x2 = c(-0.5, 0.5)), factors = c("x1", "x2", "x3"))
  ft <- herminR:::guide_feasible_text(f)
  expect_match(ft[1], "x2 keeps \\d+% and x3 keeps \\d+%")
  expect_equal(ft[2], "Exact result for these ranges.")
})

test_that("server: starts on the guide, tracks progress, renders every finding", {
  skip_if_not_installed("shiny")
  shiny::testServer(explorer_server, {
    session$setInputs(tabs = "start", source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"),
                      block = "block", order = "SO", code = TRUE, norm = "l1", signed = "abs", std = "none",
                      bounds = "ellipsoid", level = 0.95, unc_response = ".all", boot_n = 30, boot_type = "residual",
                      boot_seed = 1, rob_response = "y", alphas = "0.05", top = 2, keep = c("x1", "x2", "x3"),
                      others = "centre", feas_response = "y", constraint = "", feas_method = "exact",
                      feas_size = 1000, feas_seed = 1,
                      rng_x1_lo = -1, rng_x1_hi = 1, rng_x2_lo = -1, rng_x2_hi = 1, rng_x3_lo = -1, rng_x3_hi = 1,
                      fea_x1_lo = -1, fea_x1_hi = 1, fea_x2_lo = -1, fea_x2_hi = 1, fea_x3_lo = -1, fea_x3_hi = 1)
    expect_match(output$guide_stepper$html, "Start here")
    for (t in c("1 Data & model", "2 MPI explorer")) session$setInputs(tabs = t)
    expect_equal(guide$visited(), c("start", "1 Data & model", "2 MPI explorer"))
    expect_match(output$guide_stepper$html, "guide-pill done")
    expect_match(output$guide_find_1$html, "1 response \\(y\\) and 3 factors")
    expect_match(output$guide_find_2$html, "can change y by up to 1.68")
    expect_match(output$guide_find_3$html, "Press 'Run bootstrap'", fixed = TRUE)
    session$setInputs(run_boot = 1)
    expect_match(output$guide_find_3$html, "x2 ranked first in 100% of 30 replays")
    expect_match(output$guide_find_4$html, "Not run yet")
    session$setInputs(run_sel = 1)
    expect_match(output$guide_find_4$html, "x2 is the most influential factor on all")
    expect_match(output$guide_find_5$html, "Nothing changed yet")
    expect_match(output$guide_find_5$html, "No restriction set")
    session$setInputs(rng_x3_lo = -0.1, rng_x3_hi = 0.1, fea_x1_lo = -0.5, fea_x1_hi = 0.5)
    expect_match(output$guide_find_5$html, "influence of x2 goes from 1.68 to")
    expect_match(output$guide_find_5$html, "y: Of its unrestricted influence, x1 keeps")
    session$setInputs(guide_next_2 = 1, guide_back_3 = 1, guide_example = 1, guide_goto = "4 Robustness")
    expect_match(output$guide_find_6$html, "running it in R gives the same numbers")
  })
})
