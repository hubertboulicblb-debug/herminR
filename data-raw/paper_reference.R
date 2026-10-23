# Reference values for tests/testthat/test-paper-reference.R, produced by the
# code of Otava and Mylona (2020) (not herminR) for every heatmap in the supplement.
source("data-raw/paper_supplement/supplement_code.R")
load("data/pastry_table1.rda"); load("data/dss_sim.rda")
coefs_to_matrices <- function(cf, fs) {
  rs <- names(cf)
  main <- quad <- matrix(0, length(rs), length(fs), dimnames = list(rs, fs))
  inter <- stats::setNames(lapply(rs, function(r) matrix(0, length(fs), length(fs), dimnames = list(fs, fs))), rs)
  for (r in rs) for (t in names(cf[[r]])) {
    v <- cf[[r]][[t]]
    if (grepl(":", t)) { p <- strsplit(t, ":")[[1]]; inter[[r]][p[1], p[2]] <- v }
    else if (grepl("\\^2$", t)) quad[r, sub("\\^2$", "", t)] <- v
    else main[r, t] <- v
  }
  list(main = main, quad = quad, inter = inter)
}
studies <- list(
  pastry = list(m = coefs_to_matrices(pastry_table1, paste0("x", 1:3)), drop = c(1, 1, 0)),
  sim = list(m = coefs_to_matrices(dss_sim$coefs, paste0("Factor_", 1:11)), drop = c(1,1,0,1,1,1,0,0,0,0,1))
)
rows <- list()
for (st in names(studies)) {
  m <- studies[[st]]$m
  fs <- colnames(m$main)
  rr <- matrix(rep(c(-1, 1), each = length(fs)), ncol = 2); rr[3, ] <- c(-0.1, 0.1); rownames(rr) <- fs
  figs <- list(
    main = list(type = "mainOnly", absoluteValue = FALSE, scale = "original"),
    main_quad = list(type = "mainQuadraticOnly", absoluteValue = FALSE, scale = "original"),
    all = list(absoluteValue = FALSE, scale = "original"),
    all_perResp = list(absoluteValue = FALSE, scale = "perResponse"),
    abs_perResp = list(absoluteValue = TRUE, scale = "perResponse"),
    subset = list(absoluteValue = TRUE, scale = "perResponse", include = studies[[st]]$drop),
    range = list(absoluteValue = TRUE, scale = "perResponse", fullRange = FALSE, ranges = rr))
  for (fg in names(figs)) {
    args <- figs[[fg]]
    if (is.null(args$type)) args$type <- "allEffects"
    p <- do.call(heatmapVisualization, c(list(main_matrix = m$main, quadratic_matrix = m$quad,
                 interactions_list = m$inter), args))
    d <- p$data
    v <- if (isTRUE(figs[[fg]]$absoluteValue)) abs(d$value) else d$value
    rows[[paste(st, fg)]] <- data.frame(study = st, figure = fg, response = as.character(d$Var),
                                        factor = as.character(d$PP), expected = signif(v, 15))
  }
}
ref <- do.call(rbind, rows)
utils::write.csv(ref, "inst/extdata/paper_reference.csv", row.names = FALSE)
