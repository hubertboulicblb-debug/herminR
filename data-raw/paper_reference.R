# Reference values for tests/testthat/test-paper-reference.R
# Produced by the code of Otava and Mylona (2020) (not herminR)
# for every heatmap shown in the paper supplement.
#
# This script reconstructs the coefficient matrices from the published
# pastry and simulation examples, then calls heatmapVisualization() under
# the exact argument combinations used in the supplement figures. The
# resulting numeric values are written to inst/extdata/paper_reference.csv
# so that unit tests can verify that herminR reproduces the published
# heatmaps.

source("data-raw/paper_supplement/supplement_code.R")
load("data/pastry_table1.rda")
load("data/dss_sim.rda")

# Convert a nested list of coefficients (response -> term -> value)
# into the three matrix/list structures expected by heatmapVisualization():
#   - main : response × factor matrix of linear main-effect coefficients
#   - quad : response × factor matrix of pure-quadratic coefficients
#   - inter: named list (one matrix per response) of factor × factor
#            interaction coefficients
coefs_to_matrices <- function(cf, fs) {
  rs <- names(cf)
  main <- quad <- matrix(0, length(rs), length(fs), dimnames = list(rs, fs))
  inter <- stats::setNames(
    lapply(rs, function(r) matrix(0, length(fs), length(fs), dimnames = list(fs, fs))),
    rs
  )
  for (r in rs) {
    for (t in names(cf[[r]])) {
      v <- cf[[r]][[t]]
      if (grepl(":", t)) {
        # two-factor interaction term (e.g. "x1:x2")
        p <- strsplit(t, ":")[[1]]
        inter[[r]][p[1], p[2]] <- v
      } else if (grepl("\\^2$", t)) {
        # pure-quadratic term (e.g. "x1^2")
        quad[r, sub("\\^2$", "", t)] <- v
      } else {
        # linear main-effect term
        main[r, t] <- v
      }
    }
  }
  list(main = main, quad = quad, inter = inter)
}

# Two published examples used in the paper
studies <- list(
  pastry = list(
    m    = coefs_to_matrices(pastry_table1, paste0("x", 1:3)),
    drop = c(1, 1, 0)          # factors to keep in the "subset" figure
  ),
  sim = list(
    m    = coefs_to_matrices(dss_sim$coefs, paste0("Factor_", 1:11)),
    drop = c(1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1)
  )
)

# Collect one row per (study, figure, response, factor) combination
rows <- list()

for (st in names(studies)) {
  m  <- studies[[st]]$m
  fs <- colnames(m$main)

  # Default ranges used by the "range" figure (most factors on [-1, 1],
  # the third factor restricted to a narrower interval)
  rr <- matrix(rep(c(-1, 1), each = length(fs)), ncol = 2)
  rr[3, ] <- c(-0.1, 0.1)
  rownames(rr) <- fs

  # Argument sets that reproduce every heatmap appearing in the supplement
  figs <- list(
    main        = list(type = "mainOnly",           absoluteValue = FALSE, scale = "original"),
    main_quad   = list(type = "mainQuadraticOnly",  absoluteValue = FALSE, scale = "original"),
    all         = list(                             absoluteValue = FALSE, scale = "original"),
    all_perResp = list(                             absoluteValue = FALSE, scale = "perResponse"),
    abs_perResp = list(                             absoluteValue = TRUE,  scale = "perResponse"),
    subset      = list(                             absoluteValue = TRUE,  scale = "perResponse",
                        include = studies[[st]]$drop),
    range       = list(                             absoluteValue = TRUE,  scale = "perResponse",
                        fullRange = FALSE, ranges = rr)
  )

  for (fg in names(figs)) {
    args <- figs[[fg]]
    # Default visualisation type when not explicitly set
    if (is.null(args$type)) args$type <- "allEffects"

    # Call the visualisation function with the reconstructed matrices
    # and the figure-specific arguments
    p <- do.call(
      heatmapVisualization,
      c(list(main_matrix       = m$main,
             quadratic_matrix  = m$quad,
             interactions_list = m$inter),
        args)
    )

    d <- p$data
    # Use absolute values when the figure requested them
    v <- if (isTRUE(figs[[fg]]$absoluteValue)) abs(d$value) else d$value

    rows[[paste(st, fg)]] <- data.frame(
      study    = st,
      figure   = fg,
      response = as.character(d$Var),
      factor   = as.character(d$PP),
      expected = signif(v, 15)   # high precision for reliable unit-test comparison
    )
  }
}

# Bind all rows and write the reference CSV used by the test suite
ref <- do.call(rbind, rows)
utils::write.csv(ref, "inst/extdata/paper_reference.csv", row.names = FALSE)
