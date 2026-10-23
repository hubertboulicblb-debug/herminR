"""Response times as a user sees them: from an action in the browser to the
page being idle again (all outputs redrawn), for three data sets.

Usage (app running): python tools/benchmark/browser_timing.py URL OUT_CSV [chromium_path]
"""
import sys, time, csv, os, random, tempfile, itertools
from playwright.sync_api import sync_playwright

URL, OUT = sys.argv[1], sys.argv[2]
CHROME = sys.argv[3] if len(sys.argv) > 3 else None
rows = []

# synthetic 3^6 design, 4 responses (as in run-benchmark.R, different noise)
random.seed(7)
big = os.path.join(tempfile.mkdtemp(), "big.csv")
with open(big, "w") as fh:
    fh.write("A1,A2,A3,A4,A5,A6,R1,R2,R3,R4\n")
    coefs = [[random.gauss(0, 1) for _ in range(6)] for _ in range(4)]
    for x in itertools.product([-1, 0, 1], repeat=6):
        ys = [sum(c * v for c, v in zip(cs, x)) + 0.5 * sum(v * v for v in x) * (k - 1.5) + 0.3 * x[0] * x[1] + random.gauss(0, 0.5)
              for k, cs in enumerate(coefs)]
        fh.write(",".join(str(v) for v in list(x) + [round(y, 5) for y in ys]) + "\n")

def idle(p, t=0.15):
    p.wait_for_function("() => !document.documentElement.classList.contains('shiny-busy')", timeout=600000)
    time.sleep(t)

def timed(p, study, action, fn, settle=0.4):
    t0 = time.perf_counter()
    fn()
    time.sleep(0.25)   # let Shiny start the update
    idle(p, 0)
    # wait until no output is recalculating
    p.wait_for_function("() => document.querySelectorAll('.recalculating').length === 0", timeout=600000)
    dt = time.perf_counter() - t0 - 0.25
    rows.append({"study": study, "action": action, "seconds": round(max(dt, 0), 3)})
    print(f"{study:10s} {action:55s} {dt:6.2f} s", flush=True)
    time.sleep(settle)

def tab(p, name):
    p.evaluate(f"() => $('a[data-value=\"{name}\"]').tab('show')")

with sync_playwright() as pw:
    b = pw.chromium.launch(executable_path=CHROME) if CHROME else pw.chromium.launch()
    for study in ["pastry", "synthetic", "simulation"]:
        p = b.new_context(viewport={"width": 1440, "height": 900}, accept_downloads=True).new_page()
        t0 = time.perf_counter(); p.goto(URL); idle(p, 0)
        rows.append({"study": study, "action": "Open the app", "seconds": round(time.perf_counter() - t0, 3)})
        tab(p, "1 Data & model"); idle(p)
        if study == "pastry":
            timed(p, study, "Load data and fit models", lambda: p.locator('input[name=source][value=pastrydough]').check())
        elif study == "simulation":
            timed(p, study, "Load data (coefficients)", lambda: p.locator('input[name=source][value=dss_sim]').check())
        else:
            p.locator('input[name=source][value=upload_data]').check(); idle(p, 1)
            p.set_input_files("#file", big); time.sleep(1.5); idle(p, 0.5)
            p.evaluate("() => document.querySelector('#responses').selectize.setValue(['R1','R2','R3','R4'])"); idle(p, 0.5)
            # timed: choosing the factors fits the 4 second-order models on 729 runs and draws the results
            timed(p, study, "Fit 4 models on 729 runs (choose factors)",
                  lambda: p.evaluate("() => document.querySelector('#factors').selectize.setValue(['A1','A2','A3','A4','A5','A6'])"))
        tab(p, "2 MPI explorer"); idle(p, 1)
        timed(p, study, "Change the norm (L1 to L2)", lambda: p.locator('input[name=norm][value=l2]').check())
        timed(p, study, "Change the norm back", lambda: p.locator('input[name=norm][value=l1]').check())
        timed(p, study, "Open the term Sankey", lambda: p.locator('input[name=mpi_view][value=terms]').check())
        if study != "simulation":
            timed(p, study, "Open the contour view (first time)", lambda: p.locator('input[name=mpi_view][value=contour]').check())
            p.locator('input[name=mpi_view][value=heatmap]').check(); idle(p, 0.5)
            timed(p, study, "Open the contour view again (cached)", lambda: p.locator('input[name=mpi_view][value=contour]').check())
        p.locator('input[name=mpi_view][value=heatmap]').check(); idle(p, 0.5)
        if study != "simulation":
            timed(p, study, "Go to Uncertainty (intervals drawn)", lambda: tab(p, "3 Uncertainty"))
            timed(p, study, "Change the interval level", lambda: p.evaluate("() => Shiny.setInputValue('level', 0.9)"))
            timed(p, study, "Run bootstrap (200 replicates)", lambda: p.click("#run_boot"))
            timed(p, study, "Go to Robustness and run selection paths", lambda: (tab(p, "4 Robustness"), time.sleep(0.5), p.click("#run_sel")))
            tab(p, "5 Follow-up"); idle(p, 0.5)
            p.evaluate("() => $('a[data-value=\"Feasible region\"]').tab('show')"); idle(p, 0.5)
            p.locator('input[name=feas_method][value=lhs]').check(); idle(p, 0.3)
            timed(p, study, "Run feasible-region sampling (5000)", lambda: p.click("#run_feas"))
            timed(p, study, "Check convergence (12 runs)", lambda: p.click("#run_conv"))
        tab(p, "6 Reproduce"); idle(p, 0.5)
        t0 = time.perf_counter()
        with p.expect_download(timeout=600000) as dl:
            p.click("#dl_report")
        dl.value.path()
        rows.append({"study": study, "action": "Download the HTML report", "seconds": round(time.perf_counter() - t0, 3)})
        print(f"{study:10s} {'Download the HTML report':55s} {rows[-1]['seconds']:6.2f} s", flush=True)
        p.context.close()
    b.close()

with open(OUT, "w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=["study", "action", "seconds"]); w.writeheader(); w.writerows(rows)
print(f"{len(rows)} timings written to {OUT}")
