"""Browser check of the 0.3.1 interactions: hover details, linked highlight,
snapshot comparison, and their restoration from a link (Playwright).

Usage (app running as for happy_path.py):
  python tools/e2e/interaction.py http://localhost:8771 [screenshot_dir] [chromium_path]
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

def scan(p, img_sel, tip_sel, want):
    """Move over the plot until the tooltip names `want`; return the point."""
    box = p.locator(img_sel).bounding_box()
    for fy in [i / 40 for i in range(4, 37)]:
        for fx in [0.3, 0.45, 0.6]:
            x, y = box["x"] + fx * box["width"], box["y"] + fy * box["height"]
            p.mouse.move(x, y)
            time.sleep(0.35)
            t = p.locator(tip_sel)
            if t.count() and want in t.inner_text():
                return x, y
    return None

with sync_playwright() as pw:
    b = pw.chromium.launch(executable_path=CHROME) if CHROME else pw.chromium.launch()
    ctx = b.new_context(viewport={"width": 1400, "height": 1000})
    p = ctx.new_page()
    p.goto(URL); idle(p, 3)
    p.click("a[data-value='1 Data & model']"); idle(p, 1)
    p.click('input[name=source][value=pastrydough]'); idle(p, 3)
    p.click("a[data-value='2 MPI explorer']"); idle(p, 3)

    pt = scan(p, "#heatmap img", "#heatmap_tip .herminR-tip", "x2 on y")
    check(pt is not None, "hovering the heatmap shows a tooltip for x2 on y")
    if pt:
        tip = p.inner_text("#heatmap_tip")
        check("1.679" in tip and "1 of 3" in tip, "tooltip gives the MPI (1.679) and the rank (1 of 3)")
        p.screenshot(path=f"{OUT}/5_hover.png")
        p.mouse.click(*pt); idle(p, 2)
        check("Highlighted: x2" in p.inner_text("#focus_bar"), "click highlights x2 (bar on every stage)")
    p.click("a[data-value='3 Uncertainty']"); idle(p, 3)
    check("Highlighted: x2" in p.inner_text("#focus_bar"), "highlight carried to the Uncertainty stage")
    pt2 = scan(p, "#interval_plot img", "#interval_plot_tip .herminR-tip", "x1 on y")
    check(pt2 is not None and "Interval" in p.inner_text("#interval_plot_tip"), "interval plot tooltip with the interval")
    p.screenshot(path=f"{OUT}/6_linked.png", full_page=True)

    # snapshot comparison
    p.click("a[data-value='2 MPI explorer']"); idle(p, 2)
    p.click("#snap_take"); idle(p, 2)
    check("Snapshot" in p.inner_text("#snap_info"), "snapshot kept")
    p.click('input[name=norm][value=linf]'); idle(p, 3)
    txt = p.inner_text("#compare_text")
    check("Now: L-infinity score" in txt, "comparison describes both settings")
    check(p.locator("#compare_plot img").count() == 1, "rank comparison plot drawn")
    p.screenshot(path=f"{OUT}/7_compare.png", full_page=True)

    # link restores highlight and snapshot
    p.click(".guide-pill:has-text('Reproduce')"); idle(p, 2)
    p.click("#bookmark"); idle(p, 2)
    link = p.locator(".modal textarea, .modal input").first.input_value()
    q = ctx.new_page(); q.goto(link); idle(q, 5)
    check("Highlighted: x2" in q.inner_text("#focus_bar"), "link restores the highlighted factor")
    q.click("a[data-value='2 MPI explorer']"); idle(q, 3)
    check("Snapshot" in q.inner_text("#snap_info") and "L-infinity" in q.inner_text("#compare_text"),
          "link restores the snapshot and comparison")

    # many responses: rank-change marks on the heatmap
    p.keyboard.press("Escape"); time.sleep(1)
    if p.locator("#shiny-modal").count():
        p.evaluate("() => $('#shiny-modal').modal('hide')"); time.sleep(1)
    p.click("a[data-value='1 Data & model']"); idle(p, 1)
    p.click('input[name=source][value=dss_sim]'); idle(p, 3)
    p.click("a[data-value='2 MPI explorer']"); idle(p, 2)
    p.click('input[name=norm][value=l1]'); idle(p, 2)
    p.click("#snap_take"); idle(p, 2)
    p.evaluate("() => document.querySelector('#std').selectize.setValue('colmax_factor')"); idle(p, 4)
    check("factor ranks change" in p.inner_text("#compare_text"), "per-factor scaling changes rankings (dss_sim)")
    p.locator("#heatmap").screenshot(path=f"{OUT}/8_marks.png")
    p.locator("#compare_plot").screenshot(path=f"{OUT}/9_slopes.png")
    b.close()

print(f"{len(failures)} failure(s)")
sys.exit(1 if failures else 0)
