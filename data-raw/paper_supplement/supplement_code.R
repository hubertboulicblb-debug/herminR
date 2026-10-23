# Load the paper's code verbatim from the supplement Rmd:
#   * heatmapVisualization()  (returns the plot instead of printing it)
#   * sim_src: the simulation chunk (11 factors x 20 responses, seed 1234)
# One change: the supplement subsets quadratic_matrix twice (setup chunk,
# repeated line), which errors whenever `include` drops a factor; the
# duplicate is removed. The published HTML was evidently rendered without it.
supp_lines <- readLines("data-raw/paper_supplement/CaseStudiesFinal_vCodeSupplement.Rmd")
s <- grep("^heatmapVisualization <- function", supp_lines)
e <- grep("^```", supp_lines); e <- min(e[e > s]) - 1
fun_src <- sub("print(plotFinal)", "return(plotFinal)", supp_lines[s:e], fixed = TRUE)
dup <- which(trimws(fun_src) == "quadratic_matrix <- quadratic_matrix[, as.logical(include)]")
stopifnot(length(dup) == 2)
fun_src <- fun_src[-dup[2]]
suppressPackageStartupMessages({library(ggplot2); library(reshape2); library(scales); library(gtools)})
eval(parse(text = fun_src))
s2 <- grep("^```\\{r simulate2", supp_lines)
e2 <- grep("^```$", supp_lines); e2 <- min(e2[e2 > s2]) - 1
sim_src <- supp_lines[(s2 + 1):e2]

# Matrix form (paper) -> named coefficient vectors (herminR)
matrices_to_coefs <- function(main, quad, inter) {
  f <- colnames(main)
  lapply(stats::setNames(rownames(main), rownames(main)), function(r) {
    b <- c(stats::setNames(main[r, ], f), stats::setNames(quad[r, ], paste0(f, "^2")))
    I <- inter[[r]]; w <- which(I != 0, arr.ind = TRUE)
    if (nrow(w)) b <- c(b, stats::setNames(I[w], paste0(f[w[, 1]], ":", f[w[, 2]])))
    b[b != 0]
  })
}
