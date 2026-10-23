# herminR explorer: user evaluation protocol

**Purpose.** Find out whether scientists who are not statisticians read the
explorer's results correctly without help: what MPI means, what an interval
does and does not say, how stable a ranking is, and what a sampled
feasible-region value is worth. Calculations and rendering are covered by the
automated tests; this protocol tests *interpretation*.

**Version.** herminR 0.5.1 or later, opened with `herminR::run_explorer()`.
The answer key below uses the built-in *Pastry dough data* and the default
seeds, so every participant sees the same numbers.

## 1. Participants and setup

* 3 to 5 scientists who run designed experiments but are not statisticians
  (a chemist, a process engineer, a food scientist, ...). Five participants
  find most interpretation problems; three are enough for a first round.
* One facilitator, who does not explain or hint, and one note-taker (can be
  the same person with a screen recording).
* 45 minutes per participant: 5 introduction, 30 tasks, 10 questionnaire and
  debrief.
* Desktop browser at full screen. Optionally repeat tasks 1-2 on a phone.
* Ask participants to think aloud. If they ask a question, answer: "What
  would you do if you were on your own?"

## 2. Tasks and questions

Give each task on a card. Record, for each question, the participant's own
words and the score (section 3). Do not correct answers during the session.

### Task 1 - Influence (stage 1 and 2, about 6 min)

"Load the *Pastry dough data* and find out which process factor matters most
for the response."

| # | Question to the participant | Correct answer (key) | Typical misreadings to watch for |
|---|---|---|---|
| 1.1 | Which factor matters most, and how much? | x2: one coded unit more of x2 can change y by up to about 1.68 (y goes up). | "x2 explains 1.68 of y"; "x2 is 1.68 times more important". |
| 1.2 | What does "one coded unit" mean for x2 in practice? | Half the studied range of x2 (from the middle to the top, or bottom to the middle). | "One unit of the original scale"; "the whole range". |
| 1.3 | Does this mean that changing x2 will always change y by 1.68? | No: 1.68 is the largest change, with the other factors at the settings where they amplify x2 the most; elsewhere the change is smaller. | "Yes, always". |

### Task 2 - Where the influence comes from (stage 2 views, about 5 min)

"Open *Where it comes from* and *What MPI means*."

| # | Question | Correct answer | Misreadings |
|---|---|---|---|
| 2.1 | Is x2's influence mainly its own, or does it come through other factors? | Mainly its own: about 78% from its own terms and 22% from interactions with x1 and x3. | Treating the flows as causal pathways between factors. |
| 2.2 | In the contour view, what does the arrow show? | The one-unit step of x2 that gives the MPI, with x1 and x3 at their most favourable levels. | "The direction to optimise y". |

### Task 3 - How sure are we (stage 3, about 8 min)

"Go to the next step. Run the bootstrap with its default settings."

| # | Question | Correct answer | Misreadings |
|---|---|---|---|
| 3.1 | How precisely is x2's influence known? | Between about 0.84 and 2.54 (95% ellipsoid interval). | Reading the interval as the range of y. |
| 3.2 | Can we say x1 has an effect? | Not from these data: x1's interval reaches 0. (This is not proof that x1 has no effect.) | "x1 has no effect" (absence of evidence read as evidence of absence). |
| 3.3 | Is x2 reliably the most important factor? | Yes: first in 100% of 200 replays. | Confusing the rank result with the interval. |
| 3.4 | Switch the interval to *Delta method*. What does the app tell you, and what would you do? | Some intervals may be too narrow (terms not clearly non-zero); go back to the ellipsoid interval for reporting. | Ignoring the orange warning; reporting the narrower delta intervals because they "look better". |

### Task 4 - Robustness (stage 4, about 4 min)

| # | Question | Correct answer | Misreadings |
|---|---|---|---|
| 4.1 | Would a simpler model change the conclusion? | No: x2 stays first on every selection path. | - |
| 4.2 | Are the L2 and L-infinity values "MPI"? | No: they are contribution scores, not attainable changes of y. | Comparing magnitudes across the three panels. |

### Task 5 - Next experiment (stage 5, about 6 min)

"Suppose only settings with x1 + x2 <= 0 are allowed. Use the *Feasible
region* tab with that constraint and LHS sampling; run the sampling, then
check convergence."

| # | Question | Correct answer | Misreadings |
|---|---|---|---|
| 5.1 | How much influence does x1 keep under the constraint? | About 0.16 (sampled), much less than without it (0.55). | - |
| 5.2 | Is that number exact? | No: a sampled maximum is a lower estimate, and here it is *not yet stable* (for x1, x2 and x3); more samples are needed before reporting it. | "The app computed it, so it is exact"; reading "stable" (in other settings) as "exact". |
| 5.3 | Would "stable" prove that the true maximum was found? | No: it shows the value stopped changing under this sampling, not that every allowed setting was searched, nor that the model is right. | "Yes". |

### Task 6 - Record and reproduce (stage 6, about 4 min)

"Save your analysis so that a colleague can check it next month."

| # | Question | Correct answer | Misreadings |
|---|---|---|---|
| 6.1 | What did you save, and what would your colleague need? | The report (findings, plots, script, methods and limitations) and/or the link; an uploaded file must be sent separately. | Assuming the link contains the data. |
| 6.2 | Open the report: which limitation applies to the intervals you used? | The text under *Methods and limitations* for the chosen interval method. | - |

## 3. Scoring

For every question: **2** correct in substance, **1** partly correct (right
idea, missing the key qualification), **0** incorrect or no answer. Note
whether the participant used the on-screen help ("How to read this", the notes
under the plots) to get there.

Record also: time per task, places where the participant hesitated for more
than 20 seconds, wrong clicks, and any question they asked.

## 4. Pass criteria

The interface passes this round when, across participants:

* the key interpretation questions **1.1, 1.3, 3.2, 3.4, 5.2 and 5.3** score
  2 for at least 80% of participants, and none of them scores 0 for more than
  one participant;
* the average score over all questions is at least 1.6;
* every task is completed without the facilitator's help.

Every question that fails becomes a design change (wording, placement or
emphasis), and the round is repeated with new participants.

## 5. Questionnaire (after the tasks)

Agree 1-5 (1 = strongly disagree, 5 = strongly agree):

1. I understood what MPI measures.
2. I knew when a result was uncertain.
3. The warnings were clear about what to do next.
4. I could find the step I needed.
5. The plots were readable.
6. I would trust a report from this app enough to show it to a colleague.
7. I would need a statistician to use this app. *(reverse-scored)*

Open questions: What was the most confusing moment? What would you change
first?

## 6. Session log template

| Participant | Role | Task | Question | Answer (own words) | Score | Used help? | Time | Notes |
|---|---|---|---|---|---|---|---|---|
| P1 | | 1 | 1.1 | | | | | |

## 7. Answer key provenance

All numbers in the key come from the app with the *Pastry dough data*
(second-order model with blocks, coded factors), L1 MPI, ellipsoid intervals
at 95%, bootstrap 200 residual replicates with seed 1, LHS sampling of 5000
pairs with seed 1. If the data or defaults change, recompute the key from the
app before a session.
