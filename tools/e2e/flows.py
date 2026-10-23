"""Browser check of the 0.4.0 flow views (Sankey, network, rank flow).

Usage (app running as for happy_path.py):
  python tools/e2e/flows.py http://localhost:8771 [screenshot_dir] [chromium_path]
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

def drawn(p, sel):
    return p.locator(f"{sel} img").count() == 1 and p.locator(f"{sel}.shiny-output-error").count() == 0

with sync_playwright() as pw:
    b = pw.chromium.launch(executable_path=CHROME) if CHROME else pw.chromium.launch()
    p = b.new_context(viewport={"width": 1400, "height": 1000}).new_page()
    p.goto(URL); idle(p, 3)
    p.click("#guide_example"); idle(p, 3)          # paper Table 1
    p.click('input[name=mpi_view][value=terms]'); idle(p, 3)
    p.evaluate("() => document.querySelector('#view_response').selectize.setValue('y4')"); idle(p, 3)
    check(drawn(p, "#sankey_terms"), "term Sankey drawn")
    check("x3 (1.02) comes 64% from its own terms and 36% from interactions with x2" in p.inner_text("#anatomy_note"),
          "anatomy note explains the y4 decomposition")
    p.screenshot(path=f"{OUT}/10_sankey_terms.png", full_page=True)
    p.click('input[name=mpi_view][value=network]'); idle(p, 3)
    check(drawn(p, "#network"), "interaction network drawn")
    box = p.locator("#network img").bounding_box()
    # click the middle of the plot (no factor) then check nothing is highlighted
    p.mouse.click(box["x"] + box["width"] / 2, box["y"] + box["height"] / 2); idle(p, 1)
    check(p.locator("#focus_bar .focus-bar").count() == 0, "click away from factors highlights nothing")
    p.locator("#network").screenshot(path=f"{OUT}/11_network.png")
    p.click('input[name=mpi_view][value=responses]'); idle(p, 3)
    check(drawn(p, "#sankey_responses"), "factor-to-response Sankey drawn")
    p.locator("#sankey_responses").screenshot(path=f"{OUT}/12_sankey_responses.png")
    # contour view of the MPI definition (pastry fit)
    p.click("a[data-value='1 Data & model']"); idle(p, 1)
    p.click('input[name=source][value=pastrydough]'); idle(p, 3)
    p.click("a[data-value='2 MPI explorer']"); idle(p, 2)
    p.click('input[name=mpi_view][value=contour]'); idle(p, 3)
    check(drawn(p, "#contour"), "contour view drawn")
    check("changes the fitted response by 1.679: exactly its MPI (1.679)" in p.inner_text("#contour_note"),
          "contour note gives the MPI step of x2 (1.679)")
    p.locator("#contour").screenshot(path=f"{OUT}/14_contour.png")
    p.click('input[name=mpi_view][value=heatmap]'); idle(p, 1)
    # rank flow with raw data
    p.click("a[data-value='1 Data & model']"); idle(p, 1)
    p.click('input[name=source][value=pastrydough]'); idle(p, 3)
    p.click("a[data-value='3 Uncertainty']"); idle(p, 2)
    p.click("#run_boot"); idle(p, 4)
    p.click('input[name=rank_view][value=flow]'); idle(p, 3)
    check(drawn(p, "#rank_plot"), "bootstrap rank flow drawn")
    p.locator("#rank_plot").screenshot(path=f"{OUT}/13_rank_flow.png")
    b.close()

print(f"{len(failures)} failure(s)")
sys.exit(1 if failures else 0)
