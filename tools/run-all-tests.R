# Run the complete test suite, including tests marked skip_on_cran(), and fail
# if anything fails, warns or is skipped. Writes full-tests.log.
# Usage (from the package root, after installing the package):
#   Rscript tools/run-all-tests.R
Sys.setenv(NOT_CRAN = "true")
suppressPackageStartupMessages({library(testthat); library(herminR)})
log_file <- "full-tests.log"
con <- file(log_file, open = "wt")
sink(con, split = TRUE)
cat("herminR full test suite\n")
cat("package version:", as.character(utils::packageVersion("herminR")), "\n")
cat("date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat("R:", R.version.string, "\n")
cat("platform:", R.version$platform, "|", utils::sessionInfo()$running, "\n")
cat("NOT_CRAN:", Sys.getenv("NOT_CRAN"), "\n")
cat("key packages:", paste(vapply(c("testthat", "ggplot2", "rsm", "dplyr", "tibble"), function(p)
  paste0(p, " ", utils::packageVersion(p)), character(1)), collapse = ", "), "\n\n")
res <- as.data.frame(testthat::test_dir("tests/testthat", reporter = "summary",
                                        stop_on_failure = FALSE, package = "herminR"))
cat(sprintf("\nfiles: %d  tests: %d  expectations: %d  failed: %d  warnings: %d  skipped: %d  errors: %d\n",
            length(unique(res$file)), nrow(res), sum(res$nb), sum(res$failed),
            sum(res$warning), sum(res$skipped), sum(res$error)))
clean <- !(sum(res$failed) || sum(res$skipped) || sum(res$warning) || any(res$error))
cat(if (clean) "FULL TEST SUITE CLEAN (no skips)\n" else "FULL TEST SUITE NOT CLEAN\n")
sink(); close(con)
if (!clean) stop("Full test suite not clean (see ", log_file, ").")
