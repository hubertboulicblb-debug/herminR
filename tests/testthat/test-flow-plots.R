# 0.4.0: contributions, Sankey diagrams, interaction network, rank flow

test_that("contributions add up to the paper's MPI (Table 1, y4)", {
  m <- mpi_multi(pastry_table1)
  cc <- mpi_contributions(m, response = "y4")
  tot <- tapply(cc$contribution, cc$factor, sum)
  expect_equal(unname(tot["x3"]), 1.0185, tolerance = 1e-10)
  expect_equal(unname(tot["x2"]), 0.655, tolerance = 1e-10)
  # the interaction counts for both factors
  ix <- cc[cc$term == "x2:x3", ]
  expect_equal(sort(ix$factor), c("x2", "x3"))
  expect_equal(ix$contribution, c(0.3706, 0.3706))
  expect_setequal(cc$type, c("main", "quadratic", "interaction"))
  expect_equal(as.numeric(tapply(cc$share, cc$factor, sum)), c(1, 1))
  expect_equal(cc$mpi, abs(m$estimate[match(paste(cc$response, cc$factor), paste(m$response, m$factor))]))
  expect_error(mpi_contributions(m, response = "nope"), "Unknown response")
})

test_that("contributions match L1 MPI for a fitted model and all paper cells", {
  fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  m <- mpi(fit, abs = TRUE)
  cc <- mpi_contributions(m)
  tot <- tapply(cc$contribution, cc$factor, sum)
  expect_equal(as.numeric(tot[m$factor]), m$estimate, tolerance = 1e-10)
  d <- mpi_multi(dss_sim$coefs)
  cd <- mpi_contributions(d)
  key <- paste(cd$response, cd$factor)
  s <- tapply(cd$contribution, key, sum)
  dd <- tibble::as_tibble(d)
  expect_equal(as.numeric(s), abs(dd$estimate[match(names(s), paste(dd$response, dd$factor))]), tolerance = 1e-10)
})

test_that("contributions follow range adjustment and subsets", {
  b <- c(x1 = 0.5, x2 = -0.3, x3 = 0.2, "x2:x3" = 0.4, "x1:x3" = 0.1)
  adj <- mpi_adjust_range(mpi(b), ranges = list(x3 = c(-0.1, 0.1)))
  cc <- mpi_contributions(adj)
  expect_equal(cc$contribution[cc$factor == "x2" & cc$term == "x2:x3"], 0.04)
  sub <- mpi_subset(mpi(b), keep = c("x1", "x2"))
  expect_setequal(unique(mpi_contributions(sub)$factor), c("x1", "x2"))
})

test_that("Sankey and network plots build for every view", {
  m <- mpi_multi(pastry_table1)
  p1 <- mpi_sankey(m, response = "y4", highlight = "x2")
  expect_s3_class(p1, "ggplot")
  expect_match(p1$labels$title, "y4")
  p2 <- mpi_sankey(m, view = "responses", min_value = 0.2)
  expect_s3_class(p2, "ggplot")
  expect_error(mpi_sankey(m, view = "responses", min_value = 5), "No flow")
  expect_error(mpi_sankey(m, response = "zz"), "must be one of")
  l2 <- mpi_sankey(mpi_multi(pastry_table1, norm = "l2"), response = "y1")
  expect_match(l2$labels$subtitle, "do not add up")
  n1 <- mpi_network(m, response = "y4", highlight = "x3")
  expect_s3_class(n1, "ggplot")
  # one edge (x2:x3) of width |delta|
  ed <- n1$layers[[1]]$data
  expect_equal(nrow(ed), 1)
  expect_equal(ed$value, 0.3706)
  pdf(NULL); on.exit(dev.off())
  for (p in list(p1, p2, l2, n1, mpi_network(mpi_multi(dss_sim$coefs), "Response_20"))) {
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("Sankey engine conserves flow and stacks links inside nodes", {
  nodes <- data.frame(id = c("a", "b", "c", "d"), col = c(1L, 1L, 2L, 2L), label = c("a", "b", "c", "d"),
                      fill = "g")
  links <- data.frame(from = c("a", "a", "b"), to = c("c", "d", "d"), value = c(1, 2, 3), fill = "g")
  p <- .ggsankey(nodes, links, colours = c(g = "grey"))
  nd <- p$layers[[2]]$data
  expect_equal(nd$value, c(3, 3, 1, 5))
  poly <- p$layers[[1]]$data
  # every ribbon is as wide as its value at both ends
  for (k in unique(poly$link)) {
    y <- poly$y[poly$link == k]
    n <- length(y) / 2
    expect_equal(y[2 * n] - y[1], y[n + 1] - y[n])
  }
})

test_that("bootstrap keeps replicate ranks and draws a rank flow", {
  fit <- lm(y ~ block + (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2), data = pastrydough)
  b <- bootstrap_ranks(fit, n = 40, seed = 2)
  mat <- attr(b, "rank_matrix")
  expect_equal(dim(mat), c(40L, 3L))
  expect_equal(colMeans(mat), b$rank_mean)
  expect_equal(colMeans(mat == 1), b$top1_frac)
  f <- plot(b, type = "flow", highlight = "x2")
  expect_s3_class(f, "ggplot")
  expect_true(any(grepl("first in 100%", f$layers[[3]]$data$lab, fixed = TRUE)))
  old <- b; attr(old, "rank_matrix") <- NULL
  expect_error(plot(old, type = "flow"), "no replicate ranks")
  expect_s3_class(plot(b), "ggplot")
})

test_that("anatomy text explains own versus interaction shares", {
  cc <- mpi_contributions(mpi_multi(pastry_table1), response = "y4")
  txt <- guide_anatomy_text(cc, "y4")
  expect_match(txt, "the influence of x3 \\(1.02\\) comes 64% from its own terms and 36% from interactions with x2")
  expect_match(txt, "Mostly driven by interactions: x2")
})

test_that("the explorer shows the flow views and links clicks", {
  shiny::testServer(explorer_server, {
    session$setInputs(tabs = "start", source = "pastry_table1", norm = "l1", signed = "abs", std = "none",
                      palette = "ermine", transpose = TRUE, annotate = TRUE, reorder = FALSE,
                      mpi_view = "terms", flow_scale = TRUE, flow_min = 0)
    session$setInputs(responses = c("y1", "y2", "y3", "y4"))
    session$setInputs(view_response = "y4")
    expect_match(output$anatomy_note$html, "x3 \\(1.02\\)")
    expect_type(output$sankey_terms, "list")
    expect_type(output$network, "list")
    expect_type(output$sankey_responses, "list")
    # x2 sits at angle pi/2 - 2*pi/3 on the unit circle
    a <- pi / 2 - 2 * pi / 3
    session$setInputs(network_click = list(x = cos(a) + 0.05, y = sin(a)))
    expect_equal(focus(), list(factor = "x2", response = "y4"))
    session$setInputs(network_click = list(x = 0, y = 0))
    expect_equal(focus()$factor, "x2")
    expect_true("Where it comes from" %in% names(guide$findings(2)))
    expect_match(code_text(), "mpi_network(m0, response = \"y4\")", fixed = TRUE)
  })
})
