# Visual acceptance review, herminR 0.5.1

**Method.** `tools/visual/screens.py` captures every stage (full page) and
every plot (alone, at its rendered size) at 1440 x 900 and 390 x 844, for
the pastry dough data and for the dense paper simulation (11 factors x 20
responses): 66 screenshots per pass. Each screenshot was inspected for cut-off
or overlapping text, legibility of labels and legends, plot sizing and the
order of information. Three passes were made: before the changes, after the
first fixes, and on the release build.

## Pass 1 (0.5.0): problems found

| Where | Width | Problem |
|---|---|---|
| All plots | 390 | Titles and subtitles cut off at the right edge. |
| Heatmap, 11 x 20 | 390 | Printed values overlap into unreadable smudges; column labels overlap. |
| Term Sankey | 390, 1440 | Labels of the first column clipped at the left; labels of very thin nodes overlap. |
| Contour view | 390 | Legend on the right squeezes the surface; annotation cut off. |
| Interaction network | 390 | Legend title cut off. |
| Navigation | 390 | The tab bar unfolds into a long column of links above every page. |
| Feasible region | 1440 | Duration shown as "took 0.0 s". |
| Interval plot, delta method | 1440 | Reliability problem only explained in text above the plot. |
| Feasible region, sampled | 1440 | No convergence verdict next to the result itself. |

## Pass 2: after the first fixes

Titles, heatmap, contour and network fixed. Remaining: term Sankey at 390 px
(long first-column labels left too little room; column headings overlapped),
interval-plot caption overflowing at 1440 px, guide Back/Next buttons
squeezing the stage title at 390 px, comparison chart without any labels when
no rank changed.

## Pass 3: release build

All items above resolved (see NEWS). Plots that remain dense by nature at
390 px (factors-to-responses Sankey for 20 responses) are readable but busy;
the "Hide flows below" slider and the tables are the recommended way to read
them on a phone.

**Not covered by this review:** colour-vision deficiency simulation, screen
readers other than the text alternatives' presence, and real users; the
user-evaluation protocol (`tools/user-evaluation/protocol.md`) covers the
last point.
