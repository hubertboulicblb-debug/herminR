# 0.3.1: hover details, linked highlight, snapshot comparison

ev_at <- function(x, y, xl, yl, panel = NULL) {
  list(x = x, y = y, panelvar1 = panel, coords_css = list(x = 100, y = 50),
       domain = list(discrete_limits = list(x = xl, y = yl)))
}

test_that("pointer positions map to the right factor and response", {
  fs <- c("x1", "x2", "x3")
  # paper layout: responses on x, factors on y
  expect_equal(explorer_event_cell(ev_at(2.2, 3.1, c("y1", "y2"), fs), "heatmap", fs),
               list(factor = "x3", response = "y2"))
  # factors on x
  expect_equal(explorer_event_cell(ev_at(1, 2, fs, c("y1", "y2")), "heatmap", fs),
               list(factor = "x1", response = "y2"))
  # outside the categories
  expect_null(explorer_event_cell(ev_at(3.7, 1, c("y1", "y2"), fs), "heatmap", fs))
  expect_null(explorer_event_cell(NULL, "heatmap", fs))
  # interval plot keys are "response\rfactor"
  expect_equal(explorer_event_cell(ev_at(0.4, 2, NULL, c("y\rx1", "y\rx3")), "interval", fs),
               list(factor = "x3", response = "y"))
  expect_equal(explorer_event_cell(ev_at(2, 1, NULL, c("x2", "x1")), "rank", fs, "y"),
               list(factor = "x2", response = "y"))
})

test_that("factor facts give MPI, rank, interval, bootstrap and snapshot", {
  data(pastrydough, package = "herminR")
  fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  m <- mpi_multi(list(y = fit), abs = TRUE)
  u <- mpi_multi(list(y = fit), bounds = "ellipsoid", abs = TRUE)
  b <- bootstrap_ranks(fit, n = 30, seed = 1)
  cmp <- explorer_compare(m, mpi_multi(list(y = fit), norm = "l2", abs = TRUE))
  ff <- explorer_factor_facts("x2", "y", m, m, unc = u, boot = b, boot_response = "y", cmp = cmp)
  expect_match(ff[["MPI"]], "^1.6(79|8)")
  expect_equal(ff[["Rank in response"]], "1 of 3")
  expect_match(ff[["Interval"]], "ellipsoid, 0.95")
  expect_match(ff[["Bootstrap"]], "ranked first in 100% of 30 replicates")
  expect_match(ff[["Snapshot"]], "rank 1 \\(now rank 1\\)")
  # the bootstrap of another response is not shown
  expect_false("Bootstrap" %in% names(explorer_factor_facts("x2", "y", m, m, boot = b, boot_response = "z")))
  expect_length(explorer_factor_facts("x9", "y", m, m), 0)
  tip <- explorer_tip_ui(ev_at(1, 1, NULL, NULL), ff)
  expect_match(as.character(tip), "left:114px;top:64px")
})

test_that("comparison ranks, summary and absent factors", {
  before <- tibble::tibble(response = rep(c("A", "B"), each = 3), factor = rep(c("f1", "f2", "f3"), 2),
                           estimate = c(3, -2, 1, 0, 1, 2))
  after <- tibble::tibble(response = rep(c("A", "B"), each = 3), factor = rep(c("f1", "f2", "f3"), 2),
                          estimate = c(1, -2, 3, 0, 1, 2))
  cmp <- explorer_compare(before, after)
  expect_equal(nrow(cmp), 5)                         # B/f1 is zero in both: not ranked
  expect_equal(attr(cmp, "n_absent"), 1L)
  a <- cmp[cmp$response == "A", ]
  expect_equal(a$factor, c("f3", "f2", "f1"))       # ordered by current rank
  expect_equal(a$rank_shift, c(2L, 0L, -2L))
  txt <- explorer_compare_text(cmp, "old", "new")
  expect_match(txt[2], "2 of 5 factor ranks change \\(in 1 of 2 responses\\): f3 in A up 3\u21921; f1 in A down 1\u21923")
  expect_match(txt[3], "changes in A \\(f1 \u2192 f3\\)")
  expect_match(txt[4], "1 factor-response pair with no term")
  same <- explorer_compare_text(explorer_compare(before, before), "x", "x")
  expect_match(same[2], "No factor changes rank")
  expect_s3_class(explorer_compare_plot(cmp, list(factor = "f1")), "ggplot")
  hm <- plot(mpi_multi(list(A = c(f1 = 1, f2 = 2), B = c(f1 = 2, f2 = 1))), transpose = TRUE)
  cm2 <- explorer_compare(hm$data, tibble::tibble(response = c("A", "A", "B", "B"), factor = c("f1", "f2", "f1", "f2"),
                                                  estimate = c(2, 1, 2, 1)))
  expect_length(explorer_mark_changes(hm, cm2)$layers, length(hm$layers) + 2)
  expect_length(explorer_highlight_heatmap(hm, list(factor = "f1", response = "A"))$layers, length(hm$layers) + 2)
})

test_that("clicks highlight a factor everywhere; a second click clears it", {
  shiny::testServer(explorer_server, {
    session$setInputs(tabs = "start", source = "pastrydough", norm = "l1", signed = "abs", std = "none",
                      order = "SO", code = TRUE, palette = "ermine", transpose = TRUE, annotate = TRUE,
                      reorder = FALSE)
    session$setInputs(responses = "y", factors = c("x1", "x2", "x3"), block = "block")
    lim <- list(x = "y", y = c("x1", "x2", "x3"))
    session$setInputs(heatmap_click = list(x = 1, y = 2, domain = list(discrete_limits = lim)))
    expect_equal(focus(), list(factor = "x2", response = "y"))
    expect_match(as.character(output$focus_bar$html), "Highlighted: x2")
    session$setInputs(heatmap_hover = list(x = 1, y = 3, coords_css = list(x = 10, y = 10),
                                           domain = list(discrete_limits = lim)))
    expect_match(as.character(output$heatmap_tip$html), "x3 on y")
    session$setInputs(heatmap_click = list(x = 1, y = 2.1, domain = list(discrete_limits = lim)))
    expect_null(focus())
    session$setInputs(interval_plot_click = list(x = 1, y = 1, domain = list(discrete_limits = list(y = c("y\rx1", "y\rx3")))))
    expect_equal(focus()$factor, "x1")
    session$setInputs(focus_clear = 1)
    expect_null(focus())
  })
})

test_that("snapshot comparison follows the settings and reaches the guide", {
  shiny::testServer(explorer_server, {
    session$setInputs(tabs = "start", source = "dss_sim", norm = "l1", signed = "abs", std = "none",
                      palette = "ermine", transpose = TRUE, annotate = TRUE, reorder = FALSE, snap_mark = TRUE)
    session$setInputs(responses = paste0("Response_", 1:20))
    expect_null(snapshot())
    session$setInputs(snap_take = 1)
    expect_false(is.null(snapshot()))
    expect_true(all(compare()$rank_shift == 0))
    session$setInputs(std = "colmax_factor")
    expect_gt(sum(compare()$rank_shift != 0), 0)
    g <- guide$findings(2)
    expect_true("Compared with the snapshot" %in% names(g))
    expect_match(g[["Compared with the snapshot"]][1], "Snapshot: L1 MPI, absolute, no standardisation. Now: L1 MPI, absolute, per-factor maximum.")
    expect_type(output$compare_plot, "list")
    session$setInputs(snap_clear = 1)
    expect_null(compare())
  })
})

test_that("arrows survive a non-UTF-8 locale (no sprintf on them)", {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  before <- tibble::tibble(response = "A", factor = c("f1", "f2"), estimate = c(2, 1))
  after <- tibble::tibble(response = "A", factor = c("f1", "f2"), estimate = c(1, 2))
  txt <- explorer_compare_text(explorer_compare(before, after), "o", "n")
  expect_false(any(grepl("<U+", txt, fixed = TRUE)))
  expect_true(any(grepl("→", txt, fixed = TRUE)))
})

test_that("a snapshot sharing no cell with the current results shows a message, not an error", {
  shiny::testServer(explorer_server, {
    session$setInputs(tabs = "start", source = "pastry_table1", norm = "l1", signed = "abs", std = "none",
                      palette = "ermine", transpose = TRUE, annotate = TRUE, reorder = FALSE, snap_mark = TRUE)
    session$setInputs(responses = c("y1", "y2", "y3", "y4"))
    snapshot(list(data = data.frame(response = "other", factor = "z", estimate = 1), label = "old", time = "00:00:00"))
    session$flushReact()
    expect_equal(nrow(compare()), 0)
    expect_error(output$compare_table, "share no factor and response")
    expect_match(compare_text(), "nothing to compare")
  })
})
