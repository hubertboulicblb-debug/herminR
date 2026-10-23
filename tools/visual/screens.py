"""Screenshots of every stage and view for a visual acceptance review.

Usage (app running): python tools/visual/screens.py URL OUT_DIR [chromium_path]
Writes OUT_DIR/<width>/<nn>_<name>.png (full page) and OUT_DIR/<width>/plot_<id>.png
(each plot alone at its rendered size), then OUT_DIR/index.html (contact sheet).
"""
import sys, time, os
from playwright.sync_api import sync_playwright

URL, OUT = sys.argv[1], sys.argv[2]
CHROME = sys.argv[3] if len(sys.argv) > 3 else None
SIZES = [(1440, 900), (390, 844)]
shots = []

def idle(p, t=1.0):
    p.wait_for_function("() => !document.documentElement.classList.contains('shiny-busy')", timeout=300000)
    time.sleep(t)

def tab(p, name):
    p.evaluate(f"() => $('a[data-value=\"{name}\"]').tab('show')"); idle(p, 2)

def radio(p, name, value):
    p.locator(f'input[name={name}][value="{value}"]').check(); idle(p, 2)

with sync_playwright() as pw:
    b = pw.chromium.launch(executable_path=CHROME) if CHROME else pw.chromium.launch()
    for (w, h) in SIZES:
        d = os.path.join(OUT, str(w)); os.makedirs(d, exist_ok=True)
        p = b.new_context(viewport={"width": w, "height": h}).new_page()
        n = [0]
        def shot(name, full=True):
            n[0] += 1
            f = os.path.join(d, f"{n[0]:02d}_{name}.png"); p.screenshot(path=f, full_page=full); shots.append((w, f))
        tag = [""]
        def plot(id_):
            loc = p.locator(f"#{id_}")
            if loc.count() and loc.is_visible():
                f = os.path.join(d, f"plot_{tag[0]}{id_}.png"); loc.screenshot(path=f); shots.append((w, f))
        p.goto(URL); idle(p, 3); shot("start")
        tab(p, "1 Data & model"); radio(p, "source", "pastrydough"); idle(p, 3); shot("data")
        tab(p, "2 MPI explorer")
        for v in ["heatmap", "terms", "network", "responses", "contour"]:
            radio(p, "mpi_view", v); idle(p, 2); shot(f"mpi_{v}")
            plot({"heatmap": "heatmap", "terms": "sankey_terms", "network": "network",
                  "responses": "sankey_responses", "contour": "contour"}[v])
        radio(p, "mpi_view", "heatmap")
        p.click("#snap_take"); idle(p, 2); radio(p, "norm", "linf"); idle(p, 2); shot("mpi_compare"); plot("compare_plot")
        radio(p, "norm", "l1")
        tab(p, "3 Uncertainty"); p.click("#run_boot"); idle(p, 3); shot("uncertainty"); plot("interval_plot"); plot("rank_plot")
        radio(p, "bounds", "delta"); idle(p, 2); shot("uncertainty_delta")
        radio(p, "rank_view", "flow"); idle(p, 2); tag[0] = "flow_"; plot("rank_plot"); tag[0] = ""
        tab(p, "4 Robustness"); p.click("#run_sel"); idle(p, 4); shot("robustness"); plot("sel_plot"); plot("norm_plot")
        tab(p, "5 Follow-up"); shot("followup_ranges"); plot("followup_plot")
        p.evaluate("() => $('a[data-value=\"Feasible region\"]').tab('show')"); idle(p, 2)
        radio(p, "feas_method", "lhs"); p.fill("#constraint", "x1 + x2 <= 0"); p.keyboard.press("Tab"); idle(p, 1)
        p.click("#run_feas"); idle(p, 4); p.click("#run_conv"); idle(p, 6); shot("followup_feasible")
        plot("feas_plot"); plot("conv_plot")
        tab(p, "6 Reproduce"); shot("reproduce")
        # dense case: 11 factors x 20 responses
        tag[0] = "dense_"
        tab(p, "1 Data & model"); radio(p, "source", "dss_sim"); idle(p, 3)
        tab(p, "2 MPI explorer"); radio(p, "mpi_view", "heatmap"); idle(p, 2); shot("dense_heatmap"); plot("heatmap")
        radio(p, "mpi_view", "responses"); idle(p, 3); plot("sankey_responses")
        radio(p, "mpi_view", "terms"); idle(p, 3); plot("sankey_terms")
        radio(p, "mpi_view", "network"); idle(p, 3); plot("network")
        p.context.close()
    b.close()

with open(os.path.join(OUT, "index.html"), "w") as fh:
    fh.write("<html><body style='font-family:sans-serif'><h1>herminR visual review</h1>")
    for w in sorted({s[0] for s in shots}, reverse=True):
        fh.write(f"<h2>{w}px</h2>")
        for (ww, f) in shots:
            if ww == w:
                rel = os.path.relpath(f, OUT)
                fh.write(f"<figure style='display:inline-block;margin:8px;vertical-align:top'><img src='{rel}' style='max-width:460px;border:1px solid #ccc'><figcaption>{rel}</figcaption></figure>")
    fh.write("</body></html>")
print(f"{len(shots)} screenshots")
