# Acceptance suite for the interaction line (hover and click, comparison,
# contour view): one file, one run.
skip_if_not_installed("shiny")

pastry_fit <- function() {
  lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
}
heat_lim <- list(x = "y", y = c("x1", "x2", "x3"))   # paper layout: response on x, factors on y

# 1. Defaults unchanged -----------------------------------------------------------
test_that("1. with no hover, click, snapshot or new view, outputs equal the 0.3.0 golden values", {
  golden <- readRDS(test_path("fixtures", "golden-030.rds"))
  expect_equal(golden$version, "0.3.0")
  for (src in c("pastry", "table1")) {
    cur <- golden_capture(explorer_server, if (src == "pastry") "pastrydough" else "pastry_table1",
                          run_boot = src == "pastry")
    for (k in names(golden[[src]])) {
      expect_equal(cur[[k]], golden[[src]][[k]], info = paste(src, k))
    }
  }
})

# 2. Hover ------------------------------------------------------------------------
test_that("2. a synthetic hover on a heatmap cell returns its MPI, interval, rank and rank-hold share", {
  fit <- pastry_fit()
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(run_boot = 1)
    # idle: no tooltip
    expect_null(output$heatmap_tip$html)
    session$setInputs(heatmap_hover = list(x = 1.1, y = 1.9, coords_css = list(x = 40, y = 60),
                                           domain = list(discrete_limits = heat_lim)))
    tip <- output$heatmap_tip$html
    # expected values from the package functions directly
    m <- mpi(fit, abs = TRUE)
    u <- mpi(fit, bounds = "ellipsoid", level = 0.95, abs = TRUE)
    b <- bootstrap_ranks(fit, n = 30, type = "residual", seed = 1)
    i <- which(u$factor == "x2")
    expect_match(tip, "x2 on y", fixed = TRUE)
    expect_match(tip, sprintf("%.4g", m$estimate[m$factor == "x2"]), fixed = TRUE)          # 1.679
    expect_match(tip, sprintf("%.3g to %.3g", u$lower[i], u$upper[i]), fixed = TRUE)          # interval
    expect_match(tip, "1 of 3", fixed = TRUE)                                               # rank
    hold <- mean(attr(b, "rank_matrix")[, match("x2", b$factor)] == 1)
    expect_match(tip, sprintf("rank 1 in %.0f%% of the replays", 100 * hold), fixed = TRUE)
    expect_match(tip, "left:54px;top:74px", fixed = TRUE)                                   # follows the pointer
    # outside the cells: nothing
    session$setInputs(heatmap_hover = list(x = 2.6, y = 1, coords_css = list(x = 1, y = 1),
                                           domain = list(discrete_limits = heat_lim)))
    expect_null(output$heatmap_tip$html)
  })
})

# 3. Click toggle -----------------------------------------------------------------
test_that("3. a click selects a factor on every stage; a second click clears it", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(run_boot = 1)
    session$setInputs(heatmap_click = list(x = 1, y = 3, domain = list(discrete_limits = heat_lim)))
    expect_equal(focus(), list(factor = "x3", response = "y"))
    expect_match(output$focus_bar$html, "Highlighted: x3")
    # every stage's plot carries the emphasis
    hp <- explorer_highlight_heatmap(heatmap_plot(), focus())
    expect_equal(length(hp$layers), length(heatmap_plot()$layers) + 2L)
    ip <- explorer_highlight_points(mpi_interval_plot(unc_mpi()), focus(), "value")
    expect_equal(as.character(ip$layers[[length(ip$layers)]]$data$factor), "x3")
    rp <- explorer_highlight_points(plot(boot_res()), focus(), "rank_mean")
    expect_equal(as.character(rp$layers[[length(rp$layers)]]$data$factor), "x3")
    np <- explorer_highlight_tiles(plot(mpi_compare_norms(mpi_input(analysis(), "y")[[1]], abs = TRUE)), focus())
    expect_true(all(as.character(np$layers[[length(np$layers)]]$data$factor) == "x3"))
    fp <- explorer_highlight_heatmap(plot(followup_mpi(), transpose = TRUE, reorder = FALSE, signed = FALSE), focus())
    expect_true(all(fp$layers[[length(fp$layers) - 1]]$data$factor == "x3"))
    # the selection survives re-runs and setting changes
    session$setInputs(norm = "l2"); session$setInputs(level = 0.9); session$setInputs(run_boot = 2)
    expect_equal(focus()$factor, "x3")
    session$setInputs(norm = "l1")
    # same cell again: cleared
    session$setInputs(heatmap_click = list(x = 1, y = 3.2, domain = list(discrete_limits = heat_lim)))
    expect_null(focus())
    expect_null(output$focus_bar$html)
  })
})

# 4. Reset ------------------------------------------------------------------------
test_that("4. changing the factor set resets the selection and the snapshot", {
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(heatmap_click = list(x = 1, y = 2, domain = list(discrete_limits = heat_lim)))
    session$setInputs(snap_take = 1)
    expect_equal(focus()$factor, "x2")
    expect_false(is.null(snapshot()))
    session$setInputs(factors = c("x1", "x2"))
    expect_null(focus())
    expect_null(snapshot())
    # a new snapshot on the new study, then another data set: cleared again
    session$setInputs(snap_take = 2)
    expect_false(is.null(snapshot()))
    do.call(session$setInputs, golden_inputs("pastry_table1"))
    expect_null(snapshot())
  })
})

# 5. Comparison -------------------------------------------------------------------
test_that("5. comparison: interval level, norm and scaling changes give the right arrows and rank strings", {
  fit <- pastry_fit()
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(snap_take = 1)
    session$setInputs(level = 0.99)
    cmp <- compare()
    # MPI and ranks do not depend on the interval level; the intervals widen
    expect_equal(explorer_change_arrows(cmp), rep("none", 3))
    expect_length(explorer_rank_strings(cmp), 0)
    u95 <- mpi(fit, bounds = "ellipsoid", level = 0.95, abs = TRUE)
    u99 <- mpi(fit, bounds = "ellipsoid", level = 0.99, abs = TRUE)
    k <- match(cmp$factor, u95$factor)
    expect_equal(cmp$width_change, (u99$upper - u99$lower)[k] - (u95$upper - u95$lower)[k])
    expect_true(all(cmp$width_change > 0))
    txt <- compare_text()
    expect_true("The values themselves are unchanged." %in% txt)
    expect_true("Intervals: wider for 3 and narrower for 0 of 3 factor-response pairs." %in% txt)
    expect_match(txt[1], "ellipsoid intervals at 0.95. Now: .*ellipsoid intervals at 0.99")
    # norm change: every |value| falls from L1 to L-infinity, ranks stay
    session$setInputs(level = 0.95, norm = "linf")
    cmp <- compare()
    linf <- mpi(fit, norm = "linf", abs = TRUE)
    l1 <- mpi(fit, abs = TRUE)
    k <- match(cmp$factor, l1$factor)
    expect_equal(explorer_change_arrows(cmp), ifelse(linf$estimate[k] > l1$estimate[k], "up", "down"))
    expect_length(explorer_rank_strings(cmp), 0)
  })
  # scaling that reorders factors: rank strings checked against an independent ranking
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, utils::modifyList(golden_inputs("pastry_table1"), list(source = "dss_sim",
      responses = paste0("Response_", 1:20))))
    session$setInputs(snap_take = 1)
    session$setInputs(std = "colmax_factor")
    raw <- tibble::as_tibble(mpi_multi(dss_sim$coefs, abs = TRUE))
    sc <- tibble::as_tibble(standardize_mpi(mpi_multi(dss_sim$coefs, abs = TRUE), "colmax", by = "factor"))
    rk <- function(d) stats::ave(-abs(d$estimate), d$response, FUN = function(v) rank(v, ties.method = "min"))
    raw$r0 <- rk(raw); sc$r1 <- rk(sc)
    j <- merge(raw[, c("response", "factor", "estimate", "r0")], sc[, c("response", "factor", "r1")])
    j <- j[!(j$estimate == 0) & j$r0 != j$r1, ]
    expected <- sort(paste0(j$response, " ", j$factor, ": ", j$r0, " → ", j$r1))
    expect_equal(sort(explorer_rank_strings(compare())), expected)
    expect_gt(length(expected), 50)
    # the heatmap marks follow the same directions
    hm <- explorer_mark_changes(heatmap_plot(), compare(), kind = "rank")
    tri <- hm$layers[[length(hm$layers) - 1]]$data
    expect_equal(sum(tri$up), sum(compare()$rank_shift > 0))
  })
})

# 6. Contour ----------------------------------------------------------------------
test_that("6. contour view: the annotated step equals the MPI and the fitted model (pastry SO fit)", {
  fit <- pastry_fit()
  m <- mpi(fit)
  for (f in c("x1", "x2", "x3")) {
    p <- mpi_contour(m, factor = f)
    s <- attr(p, "mpi_step")
    expect_equal(abs(s$change), abs(m$estimate[m$factor == f]), tolerance = 1e-12)
    # independent check with predict(): same change along the arrow (block fixed)
    nd <- as.data.frame(rbind(s$start, s$end))
    nd$block <- factor(levels(factor(pastrydough$block))[1], levels = levels(factor(pastrydough$block)))
    pr <- stats::predict(fit, newdata = nd)
    expect_equal(unname(pr[2] - pr[1]), s$change, tolerance = 1e-10)
  }
  p <- mpi_contour(m, factor = "x2")
  s <- attr(p, "mpi_step")
  expect_equal(s$change, 1.679015, tolerance = 1e-6)
  expect_equal(s$partner, "x1")
  expect_equal(unname(s$start), c(-1, -1, 1))
  b <- ggplot2::ggplot_build(p)
  seg <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))]]
  expect_false(is.null(seg$geom_params$arrow))
  expect_equal(c(seg$data$x, seg$data$xend), c(-1, 0))
  lab <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomLabel"), logical(1)))]]$aes_params$label
  expect_match(lab, "changes by 1.679 \\(\\|MPI\\| = 1.679\\)")
  expect_error(mpi_contour(m, factor = "x9"), "must be one of")
  # in the app
  shiny::testServer(explorer_server, {
    do.call(session$setInputs, golden_inputs("pastrydough"))
    session$setInputs(mpi_view = "contour", view_response = "y", contour_factor = "x2", contour_partner = ".auto")
    expect_match(output$contour_note$html, "changes the fitted response by 1.679: exactly its MPI \\(1.679\\)")
    expect_match(code_text(), 'mpi_contour(mpi_multi(input, factors = c("x1", "x2", "x3")), factor = "x2", response = "y")',
                 fixed = TRUE)
    expect_true("contour" %in% st$used())
  })
})

# 7. Smoke test: the app starts and serves its page ---------------------------------
test_that("7. runApp smoke test", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if_not(file.exists(rscript) || nzchar(Sys.which("Rscript")))
  if (!file.exists(rscript)) rscript <- Sys.which("Rscript")
  port <- 8700L + sample.int(200, 1)
  pidfile <- tempfile()
  lib <- paste(.libPaths(), collapse = .Platform$path.sep)
  code <- sprintf("writeLines(as.character(Sys.getpid()), '%s'); herminR::run_explorer(port = %d, launch.browser = FALSE)",
                  gsub("\\\\", "/", pidfile), port)
  withr::local_envvar(R_LIBS = lib)
  system2(rscript, c("-e", shQuote(code)), wait = FALSE, stdout = FALSE, stderr = FALSE)
  page <- NULL
  for (i in 1:60) {
    Sys.sleep(0.5)
    page <- tryCatch(suppressWarnings(readLines(sprintf("http://127.0.0.1:%d", port), warn = FALSE)),
                     error = function(e) NULL)
    if (!is.null(page)) break
  }
  if (file.exists(pidfile)) tools::pskill(as.integer(readLines(pidfile)))
  expect_false(is.null(page))
  expect_true(any(grepl("herminR explorer", page, fixed = TRUE)))
})

test_that("3b. feasible-region bars are outlined for the selected factor", {
  f <- mpi_feasible(pastry_fit(), region = list(x3 = c(-0.5, 0.5)), factors = c("x1", "x2", "x3"))
  p <- explorer_highlight_bars(explorer_feasible_plot(f), list(factor = "x2", response = "y"))
  d <- ggplot2::layer_data(p, length(p$layers))
  expect_equal(nrow(d), 2)                       # unrestricted and feasible bars of x2
  r <- tibble::as_tibble(f)
  expect_equal(sort(d$xmax), sort(c(r$mpi_unrestricted[r$factor == "x2"], r$estimate[r$factor == "x2"])))
  base <- ggplot2::layer_data(p, 1)
  expect_true(all(round(d$ymin, 8) %in% round(base$ymin, 8)))   # drawn exactly over the original bars
})

test_that("2b. hover works without Shiny's coordinate map (recorded panel geometry)", {
  m <- mpi_multi(list(a = pastry_fit(), b = pastry_fit()), bounds = "ellipsoid", abs = TRUE)
  p <- mpi_interval_plot(m)
  store <- new.env()
  W <- 900; H <- 400
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = W, height = H, res = 96)
  explorer_draw_interactive(p, store, "interval_plot")
  grDevices::dev.off()
  g <- store$interval_plot
  expect_length(g, 2)                                   # two facet panels
  expect_equal(vapply(g, `[[`, "", "panel"), c("a", "b"))
  # pointer at the middle of the second row of panel "b"
  to_css <- function(gp, x, y) {
    bx <- gp$box
    fx <- bx$left + (x - gp$x_range[1]) / diff(gp$x_range) * (bx$right - bx$left)
    fy <- bx$bottom + (y - gp$y_range[1]) / diff(gp$y_range) * (bx$top - bx$bottom)
    list(x = fx * W, y = (1 - fy) * H)
  }
  ev <- list(x = 0.5, y = 0.5, coords_css = to_css(g[[2]], 1, 2), domain = list(left = 0, right = 1))
  ge <- explorer_geometry_event(ev, g, W, H)
  expect_equal(ge$panelvar1, "b")
  key <- g[[2]]$y_limits[2]
  expect_equal(explorer_event_cell(ge, "interval", c("x1", "x2", "x3")),
               list(factor = sub("^.*\r", "", key), response = "b"))
  # outside every panel
  expect_null(explorer_geometry_event(list(coords_css = list(x = 1, y = 1)), g, W, H))
  expect_null(explorer_geometry_event(ev, NULL, W, H))
})
