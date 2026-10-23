"""End-to-end check of the herminR explorer in a real browser (Playwright).

Usage:
  1. In R:   herminR::run_explorer(port = 8771, launch.browser = FALSE)
  2. Shell:  pip install playwright && playwright install chromium
             python tools/e2e/happy_path.py http://localhost:8771 [screenshot_dir] [chromium_path]

Walks the guided happy path and the 0.3.0 behaviours; exits non-zero on failure.
"""
import sys, time, os
from playwright.sync_api import sync_playwright

URL = sys.argv[1] if len(sys.argv) > 1 else "http://localhost:8771"
OUT = sys.argv[2] if len(sys.argv) > 2 else "e2e_screens"
CHROME = sys.argv[3] if len(sys.argv) > 3 else None
os.makedirs(OUT, exist_ok=True)
failures = []

def check(cond, msg):
    print(("PASS " if cond else "FAIL ") + msg)
    if not cond:
        failures.append(msg)

def idle(p, t=1.0):
    p.wait_for_function("() => !document.documentElement.classList.contains('shiny-busy')", timeout=120000)
    time.sleep(t)

def active_tab(p):
    return p.locator("ul#tabs li.active a").inner_text().strip()

with sync_playwright() as pw:
    b = pw.chromium.launch(executable_path=CHROME) if CHROME else pw.chromium.launch()
    ctx = b.new_context(viewport={"width": 1400, "height": 1000}, accept_downloads=True)
    p = ctx.new_page()
    p.goto(URL); idle(p, 3)
    check(active_tab(p) == "Start here", "opens on Start here")

    # Paper example: coefficients only -> Certainty unavailable
    p.click("#guide_example"); idle(p, 3)
    check(active_tab(p) == "2 MPI explorer", "example goes to step 2")
    stepper = p.inner_text(".guide-stepper")
    check("(not available)" in stepper and "(limited)" in stepper, "stepper greys out unavailable stages")
    p.click(".guide-pill:has-text('Certainty')"); idle(p, 2)
    check(p.locator(".unavailable:visible").count() >= 1, "step 3 explains why it is unavailable")
    check(not p.locator("#interval_plot").is_visible(), "step 3 controls hidden for coefficient-only data")
    p.screenshot(path=f"{OUT}/1_unavailable.png", full_page=True)
    p.click("#goto_data_3"); idle(p, 2)
    check(active_tab(p) == "1 Data & model", "'Go to Data' button navigates to step 1")

    # Own data
    p.click('input[name=source][value=pastrydough]'); idle(p, 3)
    check("(not available)" not in p.inner_text(".guide-stepper"), "stages become available with raw data")
    p.click(".guide-pill:has-text('Influence')"); idle(p, 2)
    check("can change y by up to 1.68" in p.inner_text("#guide_find_2"),
          "blocked pastry model gives the reference MPI (x2: 1.68)")
    p.click(".guide-pill:has-text('Certainty')"); idle(p, 2)
    check(p.locator("#interval_plot").is_visible(), "intervals shown with fitted models")
    p.click("#run_boot")
    time.sleep(0.05)
    check(p.locator("#run_boot").is_disabled(), "Run bootstrap is disabled while running")
    idle(p, 3)
    check(not p.locator("#run_boot").is_disabled(), "Run bootstrap re-enabled afterwards")
    check("Last run" in p.inner_text("#boot_last"), "bootstrap shows last-run details")
    p.fill("#boot_seed", "7"); p.keyboard.press("Tab"); idle(p, 1.5)
    check("Settings changed" in p.inner_text("#boot_last"), "stale-result warning after changing settings")
    p.screenshot(path=f"{OUT}/2_certainty.png", full_page=True)

    # Feasible region with an example constraint
    p.click(".guide-pill:has-text('Next experiment')"); idle(p, 2)
    p.click('a[data-value="Feasible region"]'); idle(p, 2)
    p.evaluate("() => document.querySelector('#constraint_example').selectize.setValue('x1 + x2 <= 0')"); idle(p, 2)
    check(p.input_value("#constraint") == "x1 + x2 <= 0", "example fills the constraint")
    check("press 'Run sampling'" in p.inner_text("#feas_info") or p.locator("#feas_info").inner_text() == "",
          "sampling waits for the button")
    p.click("#run_feas"); idle(p, 4)
    check("Approximate" in p.inner_text("#feas_info"), "sampled result labelled Approximate")
    p.screenshot(path=f"{OUT}/3_feasible.png", full_page=True)

    # Reproduce: report and bookmark
    p.click(".guide-pill:has-text('Reproduce')"); idle(p, 2)
    with p.expect_download(timeout=120000) as d:
        p.click("#dl_report")
    path = f"{OUT}/report.html"
    d.value.save_as(path)
    html = open(path, encoding="utf-8").read()
    check("Which factors matter most?" in html and html.count("data:image/png;base64,") >= 4,
          "report has findings and embedded plots")
    p.click("#bookmark"); idle(p, 2)
    link = p.locator(".modal textarea, .modal input").first.input_value()
    check("_inputs_" in link, "bookmark link generated")
    p.screenshot(path=f"{OUT}/4_reproduce.png", full_page=True)
    p2 = ctx.new_page()
    p2.goto(link); idle(p2, 5)
    check(p2.locator("input[name=source][value=pastrydough]").is_checked(), "link restores the data source")
    p2.click(".guide-pill:has-text('Influence')"); idle(p2, 2)
    check("can change y by up to 1.68" in p2.inner_text("#guide_find_2"), "link restores the same model and results")
    p2.click(".guide-pill:has-text('Reproduce')"); idle(p2, 2)
    check(active_tab(p2) == "6 Reproduce", "link restores the current stage")
    errs = [e for e in p.locator(".shiny-output-error:visible").all_inner_texts() if e.strip()]
    check(not errs, "no visible errors")
    b.close()

print(f"\n{len(failures)} failure(s)")
sys.exit(1 if failures else 0)
