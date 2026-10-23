# Writes tests/testthat/fixtures/golden-030.rds: the explorer's default outputs
# as produced by herminR 0.3.0 (the release before the hover, comparison,
# flow and contour features). Run from the package root with 0.3.0 installed
# in a separate library, e.g.
#   R_LIBS=~/lib030:~/Rlib Rscript tools/golden/make-golden.R
library(herminR)
stopifnot(packageVersion("herminR") == "0.3.0")
source("tests/testthat/helper-golden.R")
srv <- herminR::explorer_server
golden <- list(
  version = as.character(packageVersion("herminR")),
  pastry = golden_capture(srv, "pastrydough"),
  table1 = golden_capture(srv, "pastry_table1", run_boot = FALSE))
saveRDS(golden, "tests/testthat/fixtures/golden-030.rds", version = 2)
cat("written; report chars:", nchar(golden$pastry$report), "\n")
