# Table 1 of Otava and Mylona (2020): GLS + backward-elimination coefficients, pastry study, 4 responses.
# Main and quadratic effects as printed in the HTML supplement. Interactions:
# y1 printed (none); y2, y3 none and y4 x2:x3 = 0.3706 (the worked-example
# value). Verified by rendering the supplement's heatmapVisualization() on
# these inputs: all 7 pastry heatmaps match the published images tile for tile
# (colour difference <= 2/255), see data-raw/paper_reference.R.
mainEffects <- matrix(c(0.9944, -1.4556, 0.7556,
                        0,      -0.6233, 0.3256,
                       -0.1894,  0.8783, -0.7094,
                        0,      -0.2844, 0.2294),
  nrow = 4, byrow = TRUE, dimnames = list(paste0("y", 1:4), paste0("x", 1:3)))
quadraticEffects <- matrix(c(0,  2.0222, 0,
                             0,  0.6870, 0,
                             0, -0.5904, 0,
                             0,  0,      0.4185),
  nrow = 4, byrow = TRUE, dimnames = dimnames(mainEffects))
z <- matrix(0, 3, 3, dimnames = list(paste0("x", 1:3), paste0("x", 1:3)))
interactions <- list(y1 = z, y2 = z, y3 = z, y4 = z)
interactions$y4["x2", "x3"] <- 0.3706
source("data-raw/paper_supplement/supplement_code.R")
pastry_table1 <- matrices_to_coefs(mainEffects, quadraticEffects, interactions)
save(pastry_table1, file = "data/pastry_table1.rda", compress = "xz")
