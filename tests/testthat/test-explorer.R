# Server logic of the Shiny explorer, driven with shiny::testServer().
skip_if_not_installed("shiny")

base_inputs <- list(norm = "l1", signed = "abs", std = "colmax_response", palette = "ermine",
                    transpose = TRUE, annotate = TRUE, reorder = FALSE, bounds = "ellipsoid",
                    level = 0.95, boot_n = 30, boot_type = "residual", boot_seed = 1,
                    alphas = "0.01, 0.1", top = 2, others = "centre", constraint = "",
                    feas_method = "exact", feas_size = 2000, feas_seed = 1, order = "SO",
                    code = TRUE, block = "")

set_base <- function(session, ...) do.call(session$setInputs, utils::modifyList(base_inputs, list(...)))

test_that("paper Table 1 source reproduces Figure 1 values", {
  shiny::testServer(explorer_server, {
    set_base(session, source = "pastry_table1")
    m <- shown_mpi()
    ref <- mpi_paper_figures(which = 1)$fig1
    expect_equal(m$estimate, ref$estimate, tolerance = 1e-12)
    expect_s3_class(heatmap_plot(), "ggplot")
    expect_equal(analysis()$kind, "coef")
    expect_match(output$code, "data\\(pastry_table1\\)")
  })
})

test_that("raw data: fitted models match a direct fit, uncertainty and bootstrap run", {
  shiny::testServer(explorer_server, {
    set_base(session, source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"),
             block = "block", code = FALSE)
    a <- analysis()
    expect_equal(a$kind, "model")
    data(pastrydough, package = "herminR")
    direct <- lm(y ~ block + x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
    expect_equal(unname(coef(a$models$y)), unname(coef(direct)), tolerance = 1e-12)
    session$setInputs(std = "none")
    expect_equal(shown_mpi()$estimate, mpi(direct, abs = TRUE)$estimate, tolerance = 1e-12)
    session$setInputs(unc_response = "y")
    u <- unc_mpi()
    expect_equal(u$lower, mpi(direct, bounds = "ellipsoid")$lower, tolerance = 1e-10)
    session$setInputs(run_boot = 1)
    expect_equal(boot_res()$rank_mean, bootstrap_ranks(direct, n = 30, seed = 1)$rank_mean)
    session$setInputs(rob_response = "y", run_sel = 1)
    expect_true(all(vapply(attr(sel_res(), "paths"), `[[`, "", "status") == "ok"))
    expect_match(output$code, "lm\\(y ~ block")
  })
})

test_that("the generated script reproduces every result shown in the app", {
  for (nm in c("l1", "l2")) shiny::testServer(explorer_server, {
    set_base(session, source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"),
             block = "block", code = TRUE, norm = nm, signed = "signed", std = "none",
             unc_response = ".all", keep = c("x1", "x2"),
             rng_x1_lo = -1, rng_x1_hi = 1, rng_x2_lo = -1, rng_x2_hi = 1, rng_x3_lo = -0.1, rng_x3_hi = 0.1)
    e <- new.env(parent = globalenv())
    suppressWarnings(eval(parse(text = output$code), envir = e))
    expect_equal(e$m$estimate, shown_mpi()$estimate, tolerance = 1e-12)
    expect_equal(e$u$lower, unc_mpi()$lower, tolerance = 1e-12)
    expect_equal(e$f$estimate, followup_mpi()$estimate_raw, tolerance = 1e-12)
  })
})

test_that("MPI table is compact: one row per response", {
  shiny::testServer(explorer_server, {
    set_base(session, source = "dss_sim", std = "none")
    expect_match(output$mpi_table, "Factor_11")
    expect_equal(lengths(regmatches(output$mpi_table, gregexpr("<tr", output$mpi_table))), 21)
  })
})

test_that("coding to [-1, 1] makes uncoded uploads equivalent to coded ones", {
  set.seed(1)
  coded <- expand.grid(A = c(-1, 0, 1), B = c(-1, 0, 1), rep = 1:2)
  coded$y <- 2 + coded$A - 0.5 * coded$B + 0.7 * coded$A * coded$B + 0.3 * coded$B^2 + rnorm(nrow(coded), sd = 0.1)
  natural <- transform(coded, A = 150 + 25 * A, B = 2 + 0.5 * B)
  f <- tempfile(fileext = ".csv"); utils::write.csv(natural[, c("A", "B", "y")], f, row.names = FALSE)
  shiny::testServer(explorer_server, {
    set_base(session, source = "upload_data", file = list(datapath = f, name = "nat.csv"),
             responses = "y", factors = c("A", "B"), std = "none")
    expect_equal(analysis()$coding$low, c(125, 1.5))
    direct <- lm(y ~ A + B + A:B + I(A^2) + I(B^2), data = coded)
    expect_equal(shown_mpi()$estimate, mpi(direct, abs = TRUE)$estimate, tolerance = 1e-10)
  })
})

test_that("coefficient-table upload with SEs supports intervals; without SEs it says so", {
  tab <- data.frame(response = "R1", term = c("x1", "x2", "x1:x2", "x1^2"),
                    estimate = c(1, -0.5, 0.3, 0.2), se = 0.05)
  f <- tempfile(fileext = ".csv"); utils::write.csv(tab, f, row.names = FALSE)
  shiny::testServer(explorer_server, {
    set_base(session, source = "upload_coef", file = list(datapath = f, name = "coef.csv"), unc_response = ".all")
    expect_equal(analysis()$kind, "table")
    expect_false(anyNA(unc_mpi()$lower))
  })
  f2 <- tempfile(fileext = ".csv"); utils::write.csv(tab[, 1:3], f2, row.names = FALSE)
  shiny::testServer(explorer_server, {
    set_base(session, source = "upload_coef", file = list(datapath = f2, name = "coef.csv"), unc_response = ".all")
    expect_equal(analysis()$kind, "coef")
    expect_error(unc_mpi(), "standard errors")
  })
})

test_that("follow-up and feasible tabs call the package functions", {
  shiny::testServer(explorer_server, {
    set_base(session, source = "pastry_table1", keep = c("x1", "x2", "x3"),
             rng_x1_lo = -1, rng_x1_hi = 1, rng_x2_lo = -1, rng_x2_hi = 1, rng_x3_lo = -0.1, rng_x3_hi = 0.1)
    ref <- mpi_paper_figures(which = 2)$fig2
    expect_equal(followup_mpi()$estimate, ref$estimate, tolerance = 1e-12)
    session$setInputs(keep = c("x1", "x2"))
    expect_equal(unique(followup_mpi()$factor), c("x1", "x2"))
    session$setInputs(feas_response = "y4", fea_x1_lo = -1, fea_x1_hi = 1, fea_x2_lo = -0.5, fea_x2_hi = 0.5,
                      fea_x3_lo = -1, fea_x3_hi = 1)
    data(pastry_table1, package = "herminR")
    expect_equal(feas_res()$estimate,
                 mpi_feasible(pastry_table1$y4, region = list(x2 = c(-0.5, 0.5)), factors = c("x1", "x2", "x3"))$estimate)
    expect_equal(attr(feas_res(), "method"), "exact")
    session$setInputs(constraint = "x1 + x2 <= 0")
    expect_error(feas_res(), "sampling")
    session$setInputs(feas_method = "lhs")
    expect_error(feas_res(), "press 'Run sampling'")        # sampling never runs on its own
    session$setInputs(run_feas = 1)
    expect_equal(attr(feas_res(), "method"), "lhs")
    expect_true(all(feas_res()$n_feasible > 0))
  })
})

test_that("constraint expressions are sandboxed", {
  ok <- herminR:::.safe_constraint("x1 + abs(x2) <= 0.5 & x3 > -1", c("x1", "x2", "x3"))
  d <- data.frame(x1 = c(0, 1), x2 = c(0.2, 0), x3 = c(0, 0))
  expect_equal(ok(d), c(TRUE, FALSE))
  expect_error(herminR:::.safe_constraint("system('ls') > 0", c("x1")), "not allowed")
  expect_error(herminR:::.safe_constraint("x9 > 0", c("x1")), "Unknown name")
  expect_error(herminR:::.safe_constraint("x1 > ", c("x1")), "not a valid")
  expect_error(herminR:::.safe_constraint("x1 > 'a'", c("x1")), "Only numbers")
})

test_that("explorer_app builds", {
  expect_s3_class(explorer_app(), "shiny.appobj")
})
