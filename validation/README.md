# Validation evidence for this release (0.5.1)

* `validation-report-0.5.1.html` – `tools/validation/run-validation.R`: paper
  reproduction, brute force, coverage simulation, bootstrap scenarios,
  feasible-region convergence, contour step, edge cases, browser response
  times, and a summary of the logs below. Numbers in
  `validation-results-0.5.1.rds`.
* `R-CMD-check-0.5.1.log` – `R CMD check --as-cran --no-manual` on
  `herminR_0.5.1.tar.gz` (`NOT_CRAN=false`), in a library holding only the
  package's dependencies.
* `full-tests-0.5.1.log`, `full-tests-0.5.1-ggplot2-4.0.0.log` – the full
  suite (`NOT_CRAN=true`, no failure, warning or skip allowed) with ggplot2
  3.4.4 and 4.0.0.
* `e2e-0.5.1.log` – every browser check (`tools/e2e/run-all.sh`).
* `benchmark-0.5.1.html` / `.csv`, `browser-timing-0.5.1.csv` – computation
  times and memory (`tools/benchmark/run-benchmark.R`) and response times in
  the browser (`tools/benchmark/browser_timing.py`).
* `visual-review-0.5.1.md` – the visual acceptance review: what was looked
  at, what was found, what was changed (screenshots from
  `tools/visual/screens.py`).

All were produced on Linux (see the logs for versions). Other systems, R
versions and the latest ggplot2 are covered by
`.github/workflows/R-CMD-check.yaml`.

This folder is excluded from the CRAN tarball (`.Rbuildignore`).
