# herminR (development version)

* The method's source is now cited throughout: Otava, M. and Mylona, K. (2020),
  *Quality and Reliability Engineering International* 36(8), 2688-2698,
  doi:10.1002/qre.2697. `citation("herminR")` returns the package and the
  article; DESCRIPTION, the README, the help pages (`?mpi`, `?herminR`, the
  datasets, `mpi_paper_figures()`), the vignettes and the reference-value notes
  name the article instead of "the paper". `pastry_table1` also credits Mylona
  and Goos (2011). No change to code, results or the app.

# herminR 0.5.1

Visual acceptance review, response-time benchmark, edge-case audit and a
user-evaluation protocol (responding to the 0.5.0 app review). No new
dependency.

## Visual review: what was found and fixed
Every stage and view was captured at 1440 px and 390 px wide
(`tools/visual/screens.py`, contact sheet) and inspected; the log is in
`validation/visual-review-0.5.1.md`.
* Plot titles, subtitles and captions are wrapped to the plot's width; on
  narrow screens legends move under the plot and text is smaller (they were
  cut off on phones).
* Dense heatmaps (e.g. 11 x 20) drop their printed values and turn the
  column labels upright when cells become too narrow to read them; the values
  table has the numbers.
* Sankey diagrams: labels are no longer clipped at the left; labels of very
  thin nodes (which overlapped) are left out and listed in the table; on
  narrow screens the first column's labels go inside the diagram.
* The contour annotation is on two lines; the comparison chart names the
  factors when no rank changed; the top menu collapses into a menu button on
  phones; the guide's Back/Next buttons no longer squeeze the stage title on
  phones; durations under 0.1 s read "< 0.1 s".

## Limitations in the result itself
* Intervals: when the delta-method interval may be too narrow (a term of the
  factor is not clearly non-zero; 86% coverage in the validation simulation),
  the app says so in an orange box above the plot, marks the interval "may be
  too narrow" on the plot, adds a reliability column to the table and a
  caution to the guide, and recommends the ellipsoid interval. SE
  soft-threshold intervals are labelled as not being confidence intervals.
* Feasible region: sampled results carry a verdict for the current settings
  ("Convergence not checked", "Not yet stable for ...", "Stable under this
  sampling"), in the notes, the plot subtitle and the guide; "stable" is
  worded as numerical stability of a lower estimate, not proof that every
  feasible setting was searched or that the model is right.
* The HTML report has a "Methods and limitations" section (model,
  intervals and flags, rank stability, feasible-region status).

## Edge cases
* Uploaded data are checked before fitting, with plain explanations: text in
  a factor or response, a factor with a single value, too few runs for the
  model (previously a 1-run file produced results and a "reliable leader"),
  missing values (runs left out, reported), two-level factors in a
  second-order model and aliased terms (reported), very few residual degrees
  of freedom (reported). Coefficient tables: non-numeric estimates or
  negative standard errors are refused.
* The guide says when the top two factors are practically tied (within 1%)
  or close (within 5%) instead of ranking them confidently.

## Performance
* `tools/benchmark/run-benchmark.R` (computation times and memory) and
  `tools/benchmark/browser_timing.py` (time from a click to the page being
  idle) on three data sets, including a 729-run, 6-factor, 4-response study;
  results in `validation/benchmark-<version>.html`.
* The benchmark found that building the MPI table dominated the bootstrap:
  it is now built in one step, making every MPI computation faster. The
  bootstrap is about twice as fast with identical results (200 replicates:
  pastry 2.1 s to 1.0 s; 729-run study 4.3 s to 2.0 s; 1000 replicates on
  729 runs, the worst case measured: 21 s to 9 s).

## Other
* `tools/user-evaluation/protocol.md`: a 45-minute protocol for testing the
  explorer with non-expert scientists (tasks, questions with an answer key
  from the app, scoring, pass criteria, questionnaire).
* The validation report adds the edge-case audit and the browser response
  times.

# herminR 0.5.0

Release evidence, interpretation and readability (responding to the 0.4.2
package and app reviews). No new dependency.

## Evidence
* `tools/validation/run-validation.R` writes a self-contained validation
  report (`validation/validation-report-<version>.html`, with every number in
  an `.rds`): the paper's 1,520 heatmap cells; MPI against brute force on 300
  random second-order models; interval coverage by simulation (delta,
  ellipsoid, SE soft-threshold) in three scenarios, with Wilson intervals;
  bootstrap rank stability with a clear leader and with a tie; feasible-region
  convergence; the contour step against `predict()`; and the package-check,
  test-suite and browser-test logs of the release.
* `tools/e2e/run-all.sh` starts the explorer, runs every browser check and
  logs them (`validation/e2e-<version>.log`); new `tools/e2e/viewports.py`
  checks five window sizes (1440 x 900 to 390 x 844: plot heights, no
  horizontal scrolling, text alternatives) and keyboard use.
* CI (`.github/workflows/R-CMD-check.yaml`) now runs a matrix: Ubuntu, macOS
  and Windows; R release, oldrel-1 and R 4.2 on macOS; the full suite with
  ggplot2 3.4.4 and the latest ggplot2; the browser checks; the validation
  report. Logs and the report are uploaded as artifacts.

## Feasible regions
* New `mpi_feasible_convergence()` (with `print()` and `plot()` methods):
  repeats the sampling over increasing sizes and several seeds, compares with
  the exact value when the region is a box, and flags each factor as stable
  or not (spread across seeds and change from the previous size below a
  tolerance), with the fewest feasible pairs found.
* Explorer: "Check convergence" on the feasible-region tab (1/8, 1/4, 1/2 and
  all of the samples, three seeds), with a plain-language verdict.

## Interpretation
* New vignette "Interpreting MPI: what it can and cannot tell you": model
  adequacy, precision (what each interval covers and when it fails),
  ranking stability, practical importance, extrapolation, sampled feasible
  values, influence versus causation, and a reporting template.
* Uncertainty stage: a note next to the intervals says what the chosen
  method covers and when it is unreliable, and that rank stability is a
  separate question; the bootstrap plot says it is not a confidence interval.
* MPI stage: each view states the question it answers; the Sankey and
  network views say they show how the model adds up, not causal pathways.

## Explorer usability
* Plots follow the window height (bounded), so they fit small screens
  without becoming unreadable on large ones.
* Tables sit in collapsible sections under their plot (the comparison table
  starts open); new contributions table under the term Sankey.
* Run buttons show "Running... n s" while busy; "Last run" lines give the
  duration.
* Keyboard and screen readers: "Highlight a factor" select (same as
  clicking a factor), text alternatives on plots, visible focus outlines,
  stronger hint contrast.
* Navigation: the Advanced View moved to a "More" menu, shown only when the
  Advanced View is switched on, so the main bar is the six-stage path.
* Fix: printing a column subset of an MPI table no longer warns about a
  missing `norm` column.

# herminR 0.4.2

Fixes from running 0.4.0 / 0.4.1 on macOS with R 4.2 and a recent ggplot2.

* Hover and click now work with ggplot2 >= 4.0 whatever the Shiny version.
  With ggplot2 4.0, Shiny versions before 1.11 cannot read a ggplot's
  coordinates, so pointer events arrived without categories and tooltips,
  highlighting and links did nothing. The explorer now records where each
  panel of the heatmap, interval and bootstrap plots is drawn and maps the
  pointer itself (tested in a browser with ggplot2 3.4.4 and 4.0.0 on
  Shiny 1.8.0).
* Comparison table: no more "arguments imply differing number of rows: 0, 1"
  when the snapshot and the current results share no cell (for example a
  snapshot from another data set in 0.4.0); a message is shown instead.
* ggplot2 >= 3.5 (including 4.0): labels in the contour view and the
  interaction network no longer pass the deprecated `label.size` ("Ignoring
  unknown parameters: `label.size`"); the network no longer warns "No shared
  levels found ... linetype" or "Ignoring unknown labels" when a response has
  no interaction.
* The full test suite now passes with ggplot2 3.4.4 and 4.0.0, with no
  warnings. The golden-value test compares what the plots show (positions,
  fills, labels, shapes), not the defaults a ggplot2 version adds.

# herminR 0.4.1

Completes the interaction roadmap (hover and click, comparison, contour view)
against a single acceptance suite. No new dependency; `shiny` in Suggests now
states its minimum version (>= 1.6.0, for `bindCache()`).

## Defaults unchanged
* The R script, the HTML report and the guide's findings mention the flow
  views, the rank flow and the contour view only once they have been opened.
  With none of the new features used, the explorer's outputs equal those of
  0.3.0: tables, script, guide findings, report text and plot layer data
  (golden values in `tests/testthat/fixtures/golden-030.rds`, written by
  `tools/golden/make-golden.R` from the 0.3.0 release).

## Hover and click
* The hover panel adds how often the factor keeps its rank in the bootstrap
  replays ("rank 1 in 100% of the replays").
* The highlighted factor is now also outlined on the Robustness stage (norm
  comparison, selection paths) and the Follow-up stage (range/subset heatmap,
  feasible-region bars).
* The highlight survives every re-run and setting change and is reset only
  when the set of factors changes.

## Comparison
* The snapshot also keeps the intervals of the Uncertainty stage; the table
  shows them side by side with their width change, and the summary says
  whether intervals got wider or narrower (e.g. after changing the level).
* Heatmap marks: "Rank changes" (as before) or "Value changes" (up/down
  triangle with the size of the change in |value|, a dash when unchanged).
* Rank changes are listed as "x3: 2 -> 1".
* The snapshot is cleared when the study changes (data set, responses or
  factors); it still travels with "Copy a link".

## Contour view of the MPI definition
* New `mpi_contour()`: the fitted surface over two factors, every other factor
  at its most favourable level, with the one-coded-unit step whose change is
  the MPI drawn as an annotated arrow (second-order models). The change
  along the arrow equals the MPI for every cell of the paper simulation and
  equals `predict()` differences for the pastry fit.
* Explorer: "What MPI means (contour)" view on the MPI stage (factor and
  second factor selectors, cached with `bindCache()`).

## Tests
* `tests/testthat/test-e2e-03x.R`, one acceptance suite: defaults against
  the 0.3.0 golden values, synthetic hover, click toggle across stages,
  reset on a new factor set, comparison (interval level, norm and scaling
  changes; arrows and rank strings checked against independent
  computations), contour known-answer case, and a `runApp()` smoke test in a
  separate R process.

# herminR 0.4.0

Flow views: where influence comes from, how factors interact, how stable the
ranking is. Drawn with ggplot2 only (no new dependency).

## New functions
* `mpi_contributions()`: the term-by-term anatomy of MPI. Each term
  contributes D x |coefficient| (D = 1 for main and quadratic effects, the
  partner's half-range for a two-factor interaction); an interaction
  contributes to both of its factors. For L1 and full ranges the
  contributions add up exactly to MPI (checked on Table 1, every cell of the
  paper simulation and the pastry fit); works after
  `mpi_adjust_range()` and `mpi_subset()`.
* `mpi_sankey()`:
  * `view = "terms"`: model terms -> factors for one response, coloured by
    term type (main, quadratic, interaction). Shows, for example, that the
    paper's MPI of x3 on y4 (1.0185) is 0.229 (x3) + 0.418 (x3^2) + 0.371
    (x2:x3), and that the same x2:x3 term makes up 57% of x2's influence.
  * `view = "responses"`: factors -> responses, ribbon width |MPI| (Eq. 5
    scaling by default), with `min_value` to hide small flows.
* `mpi_network()`: factors on a circle; node area = |MPI|, node shade = share
  from the factor's own terms, edges = interactions (width |delta|, dashed
  when negative).
* `plot(bootstrap_ranks(...), type = "flow")`: Sankey from each factor to the
  ranks it took in the replays. `bootstrap_ranks()` now keeps the replicate
  ranks (`attr(, "rank_matrix")`).

## Explorer
* MPI stage: view switcher (Heatmap / Where it comes from / How factors
  interact / Factors to responses) with a response selector, a plain-language
  note ("x3's influence comes 64% from its own terms and 36% from
  interactions with x2") that also appears in the guide's findings, and a
  "Mostly driven by interactions" warning.
* Clicking a factor in the network highlights it on every stage; the
  highlighted factor is emphasised in the Sankey diagrams and network.
* Uncertainty stage: "Rank intervals" / "Rank flow (Sankey)" switch.
* The report includes the term Sankey, the network and the rank flow; the R
  script reproduces them (`mpi_contributions()`, `mpi_sankey()`,
  `mpi_network()`, `plot(br, type = "flow")`).
* Tests: `test-flow-plots.R`; `tools/e2e/flows.py` (6 browser checks).

# herminR 0.3.1

Interactive reading of the results, with no new dependency (Shiny's own plot
interaction; no plotly).

* **Hover details.** Hovering a heatmap cell, an interval row or a bootstrap
  rank row shows the numbers behind it: the MPI with its direction, the shown
  (standardised) value, the rank within the response, the interval, the
  bootstrap rank stability and, when a snapshot exists, its earlier value and
  rank.
* **Linked highlight.** Clicking a cell or row highlights that factor on every
  stage (outlined heatmap row and cell, ringed points on the interval and
  rank plots, a summary bar under the progress bar). Click again or "Clear"
  to remove it.
* **Snapshot comparison** ("Compare settings" on the MPI stage). Keep the
  current results, change the aggregation, values or standardisation, and
  see what moved: plain-language summary (how many ranks changed, whether the
  top factor changed), a rank slope chart (the 9 responses with the largest
  changes when there are more), a table, and blue up/down marks on the
  heatmap. Ranks are by absolute value within a response, so they compare
  across norms and standardisations; factor-response pairs with no term are
  not ranked. The comparison also appears in the guide's findings and in the
  HTML report.
* The highlight and the snapshot travel with "Copy a link to this analysis".
* Fix: arrows in generated text are no longer turned into `<U+2192>` when R
  runs in a non-UTF-8 locale.
* Tests: `test-explorer-v031.R` (pointer mapping, facts, comparison, click and
  snapshot behaviour in the server, locale); `tools/e2e/interaction.py`
  (hover, click, linking across stages, comparison, link restore, 11 checks).

# herminR 0.3.0

The explorer milestone: from a guided demo to a working tool.

## Context-aware stages
* What the data supports (`explorer_capabilities()`: fitted models, table with
  SEs, coefficients only) drives the whole app: the Data stage lists what is
  available; the progress bar greys out stages that are not available
  ("not available" / "limited", with a tooltip); unavailable stages and parts
  of stages show a plain explanation and a "Go to Data" button instead of an
  error; residual-SD scaling is switched off automatically without models.

## Safer, clearer heavy computations
* Bootstrap, selection paths and feasible-region sampling run only on their
  button (eagerly, so results never get lost between tabs), the button is
  disabled while running, errors are shown instead of crashing, and a "Last
  run" line says which settings produced the displayed result, with a warning
  when the settings have changed since.
* Feasible region: exact box results stay instant; LHS / grid sampling now
  needs "Run sampling", defaults to 5,000 samples (was 20,000), carries an
  Exact / Approximate badge and reports the share of sampled steps that stay
  allowed. Constraint examples written with the analysis' own factor names.
* Heatmap redraws are debounced (300 ms); model fitting shows a progress
  notification.

## Structure
* The server is split into one file per stage (`R/explorer_stage_*.R`, plus
  `R/explorer_helpers.R` and `R/explorer_report.R`); `explorer_server()` only
  wires them together. Input ids are unchanged, so links, scripts and tests
  keep working.

## Save and share
* "Copy a link to this analysis" (URL bookmarking): restores every setting,
  the model and the results of the run buttons; uploaded files must be
  uploaded again (the app says so).
* "Download report (HTML)": one self-contained file (no pandoc needed) with
  the findings of every stage, the plots and the R script.
* The generated R script now also covers selection paths, the feasible
  region (ranges, constraint, method, samples, seed) and advanced runs, and
  uses the settings of the results actually displayed.

## Tests
* `test-explorer-v030.R`: capabilities and stage status, run-on-click state
  (stale detection, invalid settings, button re-enabling), sampling and
  constraint examples, the full generated script reproducing bootstrap,
  selection and feasible results, the report (including a base64 encoder
  check) and bookmarking.
* `tools/e2e/happy_path.py`: a Playwright walk through the app in a real
  browser (22 checks, including restoring a link in a new tab).

# herminR 0.2.2

## Explorer: guided, stage-by-stage experience

* New **Start here** page (now the landing page): what the app is for, the key
  number (MPI) in one paragraph, the six stages as cards (question answered
  and what you get), "Try the paper example" and "Start with my own data"
  buttons, and a glossary.
* **Progress bar** on every page: Start, Data, Influence, Certainty
  (optional), Robustness (optional), Next experiment (optional), Reproduce;
  the current stage is highlighted, visited stages are ticked, and every
  stage is clickable.
* **Guide box** at the top of each stage: step number, the question it
  answers, what the app does, a "How to read this" section, Back / Next
  buttons, and **What we found**: plain-language findings generated from the
  current results (e.g. "y4: x3 has the largest influence; one coded unit
  more of x3 can change y4 by up to 1.02 (the response goes up)"; "x2 ranked
  first in 100% of 200 replays: a reliable leader"; "y4: influence of x2 goes
  from 0.655 to 0.321 (-51%)"). Findings use L1 wording only for MPI and say
  so for L2/L-infinity scores.
* New file `R/explorer_guide.R` (stage definitions, finding generators, UI
  and server); calculations unchanged. Tests in `test-explorer-guide.R`
  check the generated sentences against known results and the server
  wiring.

# herminR 0.2.1

## Explorer: advanced settings and Advanced View (opt-in)

* **Advanced settings** (MPI explorer sidebar, collapsed by default; native
  HTML `<details>`, no new dependency): bootstrap replicates (20-5000,
  default 200), solver tolerance (ellipsoid duality gap, 1e-12 to 1e-2,
  default 1e-6), random seed (0 to 2^31 - 1, default 1), interval method
  (ellipsoid F [default] / ellipsoid chi-squared / delta / SE / SE Bonferroni /
  none), verbose logging, caching, and "Show Advanced View tab". Defaults
  equal the previous hard-coded behaviour.
* Values and combinations are validated on the server (`adv_validate()`):
  out-of-range values, intervals with a coefficient-only source, residual-SD
  scaling without models, and L1 ellipsoid bounds beyond 16 terms per factor
  are rejected with a message, live and again on run; invalid settings never
  reach the calculation functions.
* Calculations run only when **Run advanced analysis** is clicked
  (`eventReactive`); the standard outputs never read the advanced inputs, so
  they are unchanged whatever the panel holds.
* **Advanced View** tab (hidden until enabled): run parameters, timing of
  each step (`system.time()`) with cache status, heatmap with clustering
  options (on/off, linkage, distance), verbose log, diagnostics CSV and the
  full output object as `.rds`.
* **Caching**: per-session store keyed by `rlang::hash()` of each step's own
  parameters plus a fingerprint of the data, so changing a parameter
  invalidates exactly the steps that depend on it.
* Core: `mpi_ellipsoid(gap_tol = 1e-6)` and `plot.herminR_mpi(cluster_method
  = "complete", dist_method = "euclidean")` expose previously hard-coded
  values with unchanged defaults. `rlang (>= 1.0.0)` declared for
  `rlang::hash()` (no new package).
* Tests (`test-advanced-settings.R`): validation (ranges, boundaries,
  malformed input, combinations, core never reached), defaults unchanged
  (formals, UI defaults, bit-for-bit equality with the standard functions,
  standard view independent of advanced inputs), cache invalidation per step,
  run-on-click only, outputs and downloads.

# herminR 0.2.0

## Interactive explorer (replaces the placeholder app)

* `run_explorer()` / `explorer_app()`: a Shiny app built only on the
  package's functions, in six tabs:
  1. **Data & model**: paper Table 1, paper simulation, pastry data, uploaded
     data (responses, factors, optional block, FO / FO+TWI / SO model, coding
     to [-1, 1] with the coding table shown) or an uploaded coefficient table
     (with or without SEs); design terms shown, nuisance and aliased terms
     listed separately.
  2. **MPI explorer**: heatmaps (norm, signed/absolute, Eq. 5 / per-factor /
     residual-SD / rank scaling, palette, paper layout), compact table, CSV
     and PNG downloads; non-L1 norms carry their "not MPI" note.
  3. **Uncertainty**: SE / delta / ellipsoid intervals next to bootstrap rank
     stability, presented as separate questions.
  4. **Robustness**: selection paths (terms kept per path) and norm
     comparison.
  5. **Follow-up**: paper 3.3 range adjustment and factor subsets; feasible
     region with box ranges and an optional constraint, labelled exact or
     sampling-based. Constraints are parsed through an allow-list
     (arithmetic, comparisons, abs/sqrt/exp/log/pmin/pmax, factor names);
     anything else is rejected before evaluation.
  6. **Reproduce**: the R script that recreates the results on screen.
* Tests (`test-explorer.R`, via `shiny::testServer()`): paper Figure 1/2
  values through the app; app fits equal direct `lm()` fits; coding makes
  natural-unit uploads equal coded ones; coefficient-table uploads with and
  without SEs; the generated script, evaluated, reproduces the app's MPI,
  intervals and follow-up values; constraint sandbox. The running app was
  also driven in headless Chromium through every tab.

## Fixes found while building the app

* `bootstrap_ranks()` and `mpi_selection_paths()` now work for models fitted
  inside a function (data recovered from the model frame when it holds the
  raw variables).
* `mpi_feasible()` gains `factors`, so factors with no term in a response can
  be used in `region` and `constraint`.
* `plot.herminR_mpi()` gains `text_size`; selection-path ranks are readable
  on dark tiles.

# herminR 0.1.9

Release-reproducibility fixes; no change to any calculation.

* CI: `NOT_CRAN=true` is now set only for the full-suite step; the CRAN-style
  step sets `NOT_CRAN=false` explicitly, so the expensive tests run once per
  platform. The full-suite log is uploaded as a workflow artifact.
* `tools/run-all-tests.R` writes `full-tests.log` with the package version,
  date, R version, platform and key package versions.
* The source archive now contains `validation/`: the `R CMD check --as-cran`
  log and the full-suite log for this exact version (not part of the CRAN
  tarball). Cross-platform results come from the CI workflow once the
  repository is on GitHub; the logs shipped here are from Linux only.
* Correction to the 0.1.8 entry below: its full-suite log was delivered next
  to the archive, not inside it.

# herminR 0.1.8

A validation release; no new analysis features.

* `tools/run-all-tests.R` runs the complete suite with `NOT_CRAN=true`
  (including the expensive `skip_on_cran()` tests) and fails on any failure,
  warning or skip; `.github/workflows/R-CMD-check.yaml` runs it on Linux,
  macOS and Windows after the `--as-cran` check.
* Bootstrap (`tests/testthat/test-bootstrap-validation.R`):
  * results equal an independent hand recomputation (same seed, plain `lm()`,
    explicit coefficient groups) for mean rank, rank quantiles and top-1
    share;
  * known distributions: a dominant factor ranks first in every replicate;
    exchangeable factors split first place about evenly;
  * ties share the smallest rank (documented);
  * failed/aliased replicates are counted (`n_ok + n_failed + n_aliased = n`),
    excluded and reported;
  * `n`, `level` and `norm` are validated.
* Interval plot: partial missing bounds (drawn as "no interval"), all-zero
  influences, non-L1 scores near zero, and responses with different factor
  sets all build without warnings; the caption and help state that "lower
  bound > 0" is not a significance test.
* More closed-form checks of the ellipsoid bounds: diagonal covariance
  (including near-singular, variances down to 1e-12) for the L1 maximum and
  minimum, and regions centred on zero.

# herminR 0.1.7

## Validation

* New independent validation suite (`tests/testthat/test-validation.R`);
  expected values never come from herminR's internals:
  * ellipsoid bounds against closed forms (one coefficient; spherical regions
    for L1/L2/L-infinity; axis-aligned L-infinity minimum);
  * an ill-conditioned design (condition number about 78) and a 12-term factor
    group, against Monte Carlo sampling of the confidence region (bounds
    contain every sampled point and are within 2% of the sampled extremes);
  * delta-method SE and interval against explicit sigma^2 (X'X)^-1 algebra;
  * L1 MPI for `lm`, `rsm` and blocked models, and feasible MPI (box and
    non-rectangular constraint), against grid searches with `stats::predict()`;
  * bounds on models with aliased coefficients.

## Uncertainty next to ranking

* New `mpi_interval_plot()`: estimates with their intervals per factor,
  marking factors whose interval reaches zero, with a caption that the
  intervals are not simultaneous across factors.
* `bootstrap_ranks()` returns class `herminR_ranks` with a `plot()` method
  (rank intervals and share of replicates ranked first).
* The uncertainty vignette explains why wide per-factor intervals and a
  stable ranking can coexist.

## Presentation

* Norm-comparison plot and docs state that each panel is scaled by its own
  maximum (patterns comparable, magnitudes not).
* The label test is now table-driven over all heatmap options (norm x panel x
  palette x signed x transpose x standardisation: 192 combinations), plus
  waterfall, interval, rank and selection-path plots.

# herminR 0.1.6

* No L2 / L-infinity plot is titled or labelled as MPI any more: heatmap
  titles ("Factor influence score (L2 norm, not MPI)"), legends ("|L2
  score|"), negative-cell captions, waterfall and selection-path plots follow
  the norm; the norm-comparison plot is titled "Factor influence summaries
  under different norms". "Maximal Possible Influence" / "MPI" is used only
  for L1. A test checks every non-L1 plot type.

# herminR 0.1.5

* Maintainer email set (hb914162@gmail.com).
* L2 / L-infinity results are now labelled as contribution scores (not
  attainable changes) in `print()`, heatmap subtitles and legends, and in
  `?mpi`; their sign/direction is documented as coming from the L1 maximiser.

# herminR 0.1.4

* Worked example corrected everywhere (README, `?mpi`, vignettes): for
  response 4, `x3^2` = 0.4185 (quadratic) and `x2:x3` = 0.3706 (interaction);
  0.1.3 had them swapped outside the paper-reproduction vignette. A test now
  checks the coefficient attribution in `pastry_table1$y4`, not only the
  totals.
* The paper reference now covers all 14 supplement heatmaps (1,520 cells),
  including the main-only and main + quadratic heatmaps.
* `mpi_se(pseudo_se = TRUE)` is an error until Lenth pseudo-SEs are
  implemented (it was silently ignored when a covariance matrix existed).
* `mpi_ellipsoid()`:
  * documentation states which extrema are closed form and which use
    numerical solvers, with tolerances;
  * L1 and L-infinity minima now return a dual-certified lower bound (valid
    whatever the solver accuracy, so the lower confidence bound stays
    conservative); a warning is issued if the duality gap exceeds 1e-6
    (relative). None of 1,800 minima from 300 simulated fits triggered it;
  * singular covariance blocks warn and give `NA` bounds;
  * the L1 maximum enumerates sign vectors in chunks (bounded memory), up to
    `max_terms = 16` terms by default.
* `mpi_feasible()`: `constraint` must return one non-missing logical per row.
* `response_cor()`: `n_obs` is now the matrix of observations used per pair.
* `mpi_paper_figures()`: `which` is validated.

# herminR 0.1.3

* Paper reproduction from the code supplement:
  * new dataset `pastry_table1` (paper Table 1, four responses);
  * `dss_sim` is now the paper's simulated study, generated by the
    supplement's own code (seed 1234) instead of an illustrative stand-in;
  * `mpi_paper_figures()` uses these by default and returns the paper's values;
  * `inst/extdata/paper_reference.csv` (1,520 cells) is computed by the
    paper's `heatmapVisualization()`; herminR is tested against every cell.
* `data-raw/` regenerates the datasets and reference values from the bundled
  supplement Rmd; the supplement's heatmaps are reproduced tile for tile.
* `pastrydough` is documented as the paper's response `y3`.
* `plot()` gains `transpose` (paper layout); `mpi_paper_figures(plot = TRUE)`
  draws the paper's layout.
* Factors are ordered naturally (`Factor_2` before `Factor_10`).

# herminR 0.1.2

* `mpi_delta()` uses a t quantile with the residual degrees of freedom
  (normal only when df is unavailable). Simulated coverage at 27 runs rose from
  about 93% to about 95% across 10 seeds.
* `mpi_feasible()` is L1 only: feasible influence is the attainable response
  change; L2 / L-infinity are contribution scores and are now rejected with an
  explanation (previously accepted by `method = "exact"` only).
* `mpi_paper_figures(plot = TRUE)` calls the plot method explicitly (it already
  worked through `match.fun()`, but read as a bug). Now tested.
* Tests: ellipsoid bounds checked against an independent multistart optimiser
  for near-singular covariance, repeated eigenvalues, the trust-region "hard
  case", zero coefficients and groups of 6-8 terms; coverage tests use a 99%
  Wilson criterion instead of a raw threshold.
* New acceptance test against values transcribed from the paper
  (`inst/extdata/README.md`); it activates when `paper_table1.csv` and
  `paper_reference.csv` are supplied and reports which Eq. (5) grouping
  matches the published figures.

# herminR 0.1.1

## Bug fixes (results from 0.1.0 should not be relied on)

* `mpi()` no longer fails on models with interactions under R >= 4.3
  (`||` applied to a vector in the term parser).
* `rsm` models (`FO()`, `TWI()`, `PQ()`, `SO()`) are now parsed correctly;
  0.1.0 produced invented factors and wrong values. Term mapping is now
  exact (monomial exponents) and fails loudly on unsupported terms, including
  orthogonal `poly()`.
* Main effects whose name ends in "2" (`x2`, `x12`) were classified as
  quadratic terms, which changed their sign.
* `standardize_mpi(method = "colmax")` returned the same value for every
  factor.
* `mpi_subset()` stripped "2" and "I" from term names, zeroing factor `x2`.
* `mpi_adjust_range()` now scales the interaction in the *partner* factors'
  MPI (exact, including asymmetric ranges); 0.1.0 scaled the restricted
  factor's own interactions and left partners unchanged.
* `mpi_feasible()` is now an exact computation for rectangular regions, plus
  LHS/grid sampling with arbitrary `constraint` functions; `size` and `seed`
  are used. 0.1.0 used a range-fraction approximation with substring matching.
* `response_cor()` matches observations by row name instead of truncating.
* `bootstrap_ranks()`: `"pairs"` now means case resampling; new default
  `"residual"`. Works with `rsm` and blocked models; aliased replicates are
  counted, not silently used.
* `mpi_selection_paths()` works on monomial terms (including `rsm` fits),
  enforces polynomial hierarchy, reports failed paths, and keeps factors that
  were removed (MPI 0) so stability is not inflated.
* Heatmaps: all-`NA` panels no longer give infinite limits; the "ermine"
  signed palette now distinguishes signs.

## Statistics

* `se` in `mpi()` is the covariance-aware delta-method SE.
* `mpi_ellipsoid()` computes the exact range of the norm over the joint
  confidence ellipsoid (F critical value by default).
* `mpi_se(k_adj = TRUE)` uses a Bonferroni t quantile.
* New `direction` column; the sign convention is documented in `?mpi`.

## Data

* `pastrydough` now holds the real Gilmour & Trinca design and the response
  distributed in `daewr` (0.1.0 shipped simulated values). `dss_sim` is
  regenerated by `data-raw/dss_sim.R` and documented as illustrative.
* `mpi_paper_figures()` accepts the paper's Table 1 via `coef_table` and
  records the provenance of its inputs.

# herminR 0.1.0

* Initial release.
