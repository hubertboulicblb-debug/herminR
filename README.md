# herminR

`herminR` implements the Maximal Possible Influence (MPI) of Otava and Mylona
(2020), [doi:10.1002/qre.2697](https://doi.org/10.1002/qre.2697), for fitted
second-order response-surface models: the largest change in a response that a
one-unit increase of one factor can produce, with the other factors set most
favourably. It works for one or many responses and draws response × factor
heatmaps.

Beyond the article's Eq. (4) it provides:

* uncertainty: soft-threshold SE bounds, delta-method intervals and exact
  simultaneous ellipsoid bounds;
* L1, L2 and L-infinity aggregation;
* Eq. (5) column-max, residual-SD and rank standardisation;
* the article's follow-up tools (range adjustment, factor subsets);
* feasible-region MPI (exact for boxes, sampling for any constraint);
* model-selection path sensitivity and bootstrap rank stability, with plots
  that show precision and ranking stability next to the estimates.

*Kentoc'h mervel eget em zaotra*: rather death than a defiled heatmap.

## Status (0.5.1)

The core calculations are validated against brute-force computation and an
independent suite (closed forms, `predict()` grid searches, Monte Carlo), and the
article's four figures are reproduced from its bundled inputs: herminR
matches the article's own supplement code on all 1,520 heatmap cells of both
case studies. See `vignette("paper-reproduction")`.

## Interactive explorer

```r
herminR::run_explorer()   # needs the shiny package
```

A guided Shiny app: it opens on a *Start here* page, shows a progress bar
through six stages, and on each stage explains the question being answered,
how to read the result and what was found, in plain language. It uses the
package's own functions: data or coefficients -> model -> MPI heatmaps -> uncertainty
intervals and bootstrap rank stability -> selection-path and norm robustness
-> follow-up ranges, factor subsets and feasible regions (labelled exact or
sampling-based). Stages that your data cannot support are greyed out
with an explanation. Hover a heatmap cell or interval for its numbers, click
it to highlight that factor on every stage, and keep a snapshot to see which
rankings change when you change the settings. Sankey diagrams and an
interaction network show where each factor's influence comes from
(`mpi_contributions()`, `mpi_sankey()`, `mpi_network()`), and a rank flow
shows how stable the ranking is (`plot(bootstrap_ranks(fit), type = "flow")`).
A contour view (`mpi_contour()`) draws the one-unit step that defines a
factor's MPI on the fitted surface.

Before reporting results, read the vignette *Interpreting MPI: what it can
and cannot tell you* (`vignette("interpretation", package = "herminR")`).
Each release ships a validation report (`validation/validation-report-*.html`
in the source) produced by `tools/validation/run-validation.R`. The *Reproduce* tab writes the R script that recreates
the results on screen, downloads a self-contained HTML report, and gives a
link that restores the analysis.

## Installation

```r
# install.packages("remotes")
# from GitHub
remotes::install_github("hubertboulicblb-debug/herminR", build_vignettes = TRUE)

# or from a release tarball
remotes::install_local("herminR_0.5.1.tar.gz")
```

## Quick example

```r
library(herminR)
data(pastry_table1)
pastry_table1$y4      # x3 = 0.2294, x3^2 = 0.4185, x2:x3 = 0.3706
mpi(pastry_table1$y4) # MPI_34 = 1.0185, MPI_24 = -0.655

data(pastrydough)
fit <- rsm::rsm(y ~ block + SO(x1, x2, x3), data = pastrydough)
m <- mpi(fit, bounds = "ellipsoid")
plot(m)
```
