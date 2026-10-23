# Advanced settings: validation, defaults-unchanged, cache invalidation ---------

pastry_fit <- function() {
  data(pastrydough, package = "herminR", envir = environment())
  lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
}
std_settings <- function(...) utils::modifyList(list(norm = "l1", abs = TRUE, std = "colmax_response",
                                                     level = 0.95, boot_type = "residual", response = "y"),
                                                list(...))
adv <- function(...) utils::modifyList(herminR:::adv_defaults(), list(...))
ctx_model <- list(kind = "model", norm = "l1", std = "none", max_group = 4L)

# 1. Validation ------------------------------------------------------------------

test_that("defaults are valid", {
  expect_length(herminR:::adv_validate(herminR:::adv_defaults(), ctx_model), 0)
  expect_length(herminR:::adv_validate(herminR:::adv_defaults(), list(kind = "table", norm = "l2")), 0)
})

test_that("out-of-range and malformed values are rejected with clear messages", {
  v <- function(...) herminR:::adv_validate(adv(...), ctx_model)
  for (bad in list(19, 5001, 20.5, NA_real_, "200", c(100, 200), Inf)) {
    expect_match(v(iterations = bad), "Bootstrap replicates must be a whole number from 20 to 5000", all = FALSE)
  }
  for (bad in list(0, 1e-13, 0.5, NA_real_, -1e-6)) {
    expect_match(v(tolerance = bad), "Solver tolerance must be a number from 1e-12 to 0.01", all = FALSE)
  }
  for (bad in list(-1, 1.5, 2^31, NA_real_)) {
    expect_match(v(seed = bad), "Random seed must be a whole number from 0", all = FALSE)
  }
  expect_match(v(method = "bayes"), "Choose an interval method", all = FALSE)
  expect_match(v(verbose = NA), "'verbose' must be TRUE or FALSE", all = FALSE)
  expect_match(v(cache = "yes"), "'cache' must be TRUE or FALSE", all = FALSE)
  expect_length(v(iterations = 20, tolerance = 1e-12, seed = 0), 0)          # boundaries are valid
  expect_length(v(iterations = 5000, tolerance = 1e-2, seed = .Machine$integer.max), 0)
})

test_that("invalid combinations with the analysis are rejected", {
  d <- herminR:::adv_defaults()
  expect_match(herminR:::adv_validate(d, list(kind = "coef", norm = "l1")), "Intervals need fitted models")
  expect_length(herminR:::adv_validate(adv(method = "none"), list(kind = "coef", norm = "l1")), 0)
  expect_match(herminR:::adv_validate(adv(method = "none"), list(kind = "coef", std = "residual_sd")),
               "Residual-SD standardisation needs fitted models")
  big <- list(kind = "model", norm = "l1", max_group = 17L)
  expect_match(herminR:::adv_validate(d, big), "limited to 16 terms")
  expect_length(herminR:::adv_validate(adv(method = "delta"), big), 0)
  expect_length(herminR:::adv_validate(d, utils::modifyList(big, list(norm = "l2"))), 0)
})

test_that("invalid settings never reach the calculation functions", {
  local_mocked_bindings(mpi_multi = function(...) stop("core reached"), .package = "herminR")
  expect_error(herminR:::adv_run(list(y = pastry_fit()), "model", c("x1", "x2", "x3"),
                                 adv(iterations = 3), std_settings()),
               "Bootstrap replicates")
  skip_if_not_installed("shiny")
  shiny::testServer(explorer_server, {
    session$setInputs(source = "pastry_table1", norm = "l1", signed = "abs", std = "none",
                      adv_iter = 200, adv_tol = 1e-6, adv_seed = 1, adv_method = "ellipsoid_F",
                      adv_verbose = FALSE, adv_cache = FALSE, adv_show = FALSE, adv_run = 1)
    expect_error(adv$result(), "Intervals need fitted models")   # coef-only source + intervals
    expect_equal(adv$runs(), 0L)
  })
})

# 2. Defaults leave results unchanged -------------------------------------------------

test_that("defaults equal the previously hard-coded behaviour", {
  d <- herminR:::adv_defaults()
  expect_identical(formals(mpi_ellipsoid)$gap_tol, d$tolerance)
  expect_identical(eval(formals(mpi_ellipsoid)$crit)[1], "F")
  expect_identical(formals(herminR:::.ellipsoid_norm_range)$gap_tol, d$tolerance)
  expect_identical(formals(herminR:::plot.herminR_mpi)$cluster_method, "complete")
  expect_identical(formals(herminR:::plot.herminR_mpi)$dist_method, "euclidean")
  skip_if_not_installed("shiny")
  html <- as.character(explorer_ui())
  # Uncertainty-tab defaults that the advanced defaults must match
  input_tag <- function(id) regmatches(html, regexpr(sprintf('<input id="%s"[^>]*>', id), html))
  expect_match(input_tag("boot_n"), sprintf('value="%d"', d$iterations), fixed = TRUE)
  expect_match(input_tag("boot_seed"), sprintf('value="%d"', d$seed), fixed = TRUE)
  expect_match(input_tag("adv_iter"), sprintf('value="%d"', d$iterations), fixed = TRUE)
  # panel collapsed by default
  expect_match(html, '<details id="adv_panel" class="adv-settings">', fixed = TRUE)
})

test_that("an advanced run with default settings reproduces the standard results bit for bit", {
  fit <- pastry_fit()
  res <- herminR:::adv_run(list(y = fit), "model", c("x1", "x2", "x3"), herminR:::adv_defaults(), std_settings())
  ref_mpi <- mpi_multi(list(y = fit), norm = "l1", abs = TRUE, factors = c("x1", "x2", "x3"))
  cols <- c("factor", "response", "estimate", "se", "direction", "terms_contributed")
  expect_identical(as.data.frame(res$mpi)[cols], as.data.frame(ref_mpi)[cols])
  expect_identical(res$standardized$estimate, standardize_mpi(ref_mpi, "colmax", by = "response")$estimate)
  ref_b <- mpi_multi(list(y = fit), bounds = "ellipsoid", level = 0.95, abs = TRUE, factors = c("x1", "x2", "x3"))
  expect_identical(res$bounds$lower, ref_b$lower)
  expect_identical(res$bounds$upper, ref_b$upper)
  ref_r <- bootstrap_ranks(fit, n = 200, seed = 1)
  expect_identical(as.data.frame(res$bootstrap), as.data.frame(ref_r))
  expect_identical(mpi_ellipsoid(fit), mpi_ellipsoid(fit, gap_tol = 1e-6))
})

test_that("standard view ignores the advanced inputs entirely", {
  skip_if_not_installed("shiny")
  shiny::testServer(explorer_server, {
    session$setInputs(source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"), block = "block",
                      order = "SO", code = TRUE, norm = "l1", signed = "abs", std = "colmax_response",
                      bounds = "ellipsoid", level = 0.95, unc_response = ".all", palette = "ermine",
                      transpose = TRUE, annotate = TRUE, reorder = FALSE)
    before <- list(m = shown_mpi(), u = unc_mpi(), h = heatmap_plot()$data)
    session$setInputs(adv_iter = 999, adv_tol = 1e-3, adv_seed = 42, adv_method = "delta",
                      adv_verbose = TRUE, adv_cache = TRUE, adv_show = TRUE)
    expect_identical(shown_mpi(), before$m)
    expect_identical(unc_mpi(), before$u)
    expect_identical(heatmap_plot()$data, before$h)
  })
})

# 3. Cache and re-run behaviour ------------------------------------------------------

test_that("cache: same key hits, any changed parameter misses", {
  cache <- herminR:::adv_cache_new()
  n <- 0
  f <- function() { n <<- n + 1; n }
  expect_false(herminR:::adv_cached(cache, list(a = 1), f)$hit)
  r <- herminR:::adv_cached(cache, list(a = 1), f)
  expect_true(r$hit); expect_equal(r$value, 1); expect_equal(n, 1)
  expect_false(herminR:::adv_cached(cache, list(a = 2), f)$hit)
  expect_false(herminR:::adv_cached(cache, list(a = 1, b = TRUE), f)$hit)
  expect_equal(herminR:::adv_cache_size(cache), 3)
  expect_false(herminR:::adv_cached(NULL, list(a = 1), f)$hit)            # disabled cache
})

test_that("cache invalidation follows each step's own parameters", {
  fit <- pastry_fit()
  cache <- herminR:::adv_cache_new()
  run <- function(st = std_settings(), s = adv(cache = TRUE, iterations = 30), inp = list(y = fit)) {
    herminR:::adv_run(inp, "model", c("x1", "x2", "x3"), s, st, cache = cache)$timings
  }
  t1 <- run()
  expect_true(all(t1$cache == "miss"))
  t2 <- run()
  expect_true(all(t2$cache == "hit"))                                    # identical parameters
  t3 <- run(st = std_settings(std = "rank"))                             # only standardisation changed
  expect_equal(t3$cache, c("hit", "hit", "miss", "hit", "hit"))
  t4 <- run(s = adv(cache = TRUE, iterations = 30, tolerance = 1e-8))    # only interval tolerance changed
  expect_equal(t4$cache, c("hit", "hit", "hit", "miss", "hit"))
  t5 <- run(s = adv(cache = TRUE, iterations = 40))                      # only bootstrap replicates
  expect_equal(t5$cache, c("hit", "hit", "hit", "hit", "miss"))
  t6 <- run(st = std_settings(norm = "l2"))                              # norm feeds steps 2-5
  expect_equal(t6$cache, c("hit", "miss", "miss", "miss", "miss"))
  d2 <- pastrydough; d2$y[1] <- d2$y[1] + 0.5                            # new data: everything
  fit2 <- lm(formula(fit), data = d2)
  expect_true(all(run(inp = list(y = fit2))$cache == "miss"))
  expect_identical(herminR:::adv_fingerprint(list(y = fit)),
                   herminR:::adv_fingerprint(list(y = lm(formula(fit), data = pastrydough))))
  t8 <- run(s = adv(cache = FALSE, iterations = 30))                     # caching off
  expect_true(all(t8$cache == "off"))
})

test_that("server: runs only on click; cached re-runs; outputs and downloads work", {
  skip_if_not_installed("shiny")
  shiny::testServer(explorer_server, {
    session$setInputs(source = "pastrydough", responses = "y", factors = c("x1", "x2", "x3"), block = "block",
                      order = "SO", code = TRUE, norm = "l1", signed = "abs", std = "colmax_response",
                      level = 0.95, boot_type = "residual", unc_response = ".all", palette = "ermine",
                      transpose = TRUE, annotate = TRUE, reorder = FALSE,
                      adv_iter = 30, adv_tol = 1e-6, adv_seed = 1, adv_method = "ellipsoid_F",
                      adv_verbose = TRUE, adv_cache = TRUE, adv_show = TRUE,
                      adv_cluster = TRUE, adv_linkage = "average", adv_dist = "manhattan")
    expect_equal(adv$runs(), 0L)
    msgs <- character(0)
    withCallingHandlers(session$setInputs(adv_run = 1),
                        message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
    expect_true(any(grepl("[herminR] 1 Term mapping: start", msgs, fixed = TRUE)))   # verbose -> console
    r1 <- adv$result()
    expect_equal(adv$runs(), 1L)
    expect_true(all(r1$timings$cache == "miss"))
    session$setInputs(adv_iter = 50, adv_seed = 7)                 # no click: nothing re-runs
    expect_identical(adv$result(), r1)
    expect_equal(adv$runs(), 1L)
    suppressMessages(session$setInputs(adv_iter = 30, adv_seed = 1, adv_run = 2))  # same parameters: all cached
    expect_true(all(adv$result()$timings$cache == "hit"))
    expect_equal(adv$result()$bootstrap, r1$bootstrap)
    suppressMessages(session$setInputs(std = "rank", adv_run = 3))
    expect_equal(adv$result()$timings$cache, c("hit", "hit", "miss", "hit", "hit"))
    expect_match(output$adv_params, "interval_method")
    expect_match(output$adv_timings, "5 Bootstrap ranks")
    expect_match(output$adv_log, "Bootstrap ranks: done")
    csv <- output$adv_dl_csv
    expect_true(file.exists(csv))
    dg <- utils::read.csv(csv)
    expect_true(all(c("estimate", "standardized", "interval_lower", "rank_mean") %in% names(dg)))
    rds <- readRDS(output$adv_dl_rds)
    expect_s3_class(rds, "herminR_advanced")
    expect_equal(rds$params$standardisation, "rank")
  })
})

test_that("advanced heatmap honours the clustering options", {
  m <- mpi_multi(dss_sim$coefs, abs = TRUE)
  p <- plot(m, reorder = TRUE, cluster_method = "average", dist_method = "manhattan")
  M <- tapply(m$estimate, list(m$factor, m$response), function(v) v[1])
  expect_equal(levels(p$data$factor),
               rownames(M)[stats::hclust(stats::dist(M, "manhattan"), "average")$order])
  p0 <- plot(m, reorder = TRUE)
  expect_equal(levels(p0$data$factor), rownames(M)[stats::hclust(stats::dist(M))$order])
})
