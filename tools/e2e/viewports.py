"""Layout and keyboard checks at several window sizes (Playwright).

Usage (app running as for happy_path.py):
  python tools/e2e/viewports.py http://localhost:8771 [screenshot_dir] [chromium_path]
"""
import sys, time, os
from playwright.sync_api import sync_playwright

URL = sys.argv[1] if len(sys.argv) > 1 else "http://localhost:8771"
OUT = sys.argv[2] if len(sys.argv) > 2 else "e2e_screens"
CHROME = sys.argv[3] if len(sys.argv) > 3 else None
os.makedirs(OUT, exist_ok=True)
failures = []
SIZES = [(1440, 900), (1280, 800), (1024, 768), (768, 1024), (390, 844)]

def check(cond, msg):
    print(("PASS " if cond else "FAIL ") + msg)
    if not cond:
        failures.append(msg)

def idle(p, t=1.0):
    p.wait_for_function("() => !document.documentElement.classList.contains('shiny-busy')", timeout=120000)
    time.sleep(t)

with sync_playwright() as pw:
    b = pw.chromium.launch(executable_path=CHROME) if CHROME else pw.chromium.launch()
    for (w, h) in SIZES:
        p = b.new_context(viewport={"width": w, "height": h}).new_page()
        p.goto(URL); idle(p, 3)
        p.evaluate("() => Shiny.setInputValue('source', 'pastrydough')")
        p.evaluate("() => $('a[data-value=\"1 Data & model\"]').tab('show')"); idle(p, 1)
        p.click('input[name=source][value=pastrydough]'); idle(p, 3)
        p.evaluate("() => $('a[data-value=\"2 MPI explorer\"]').tab('show')"); idle(p, 3)
        img = p.locator("#heatmap img")
        box = img.bounding_box() if img.count() else None
        check(box is not None and 360 - 1 <= box["height"] <= 640 + 1,
              f"{w}x{h}: heatmap height {box and round(box['height'])} px within 360-640")
        sw = p.evaluate("() => document.documentElement.scrollWidth")
        check(sw <= w + 1, f"{w}x{h}: no horizontal page scroll (content {sw} px)")
        alt = img.get_attribute("alt") if img.count() else ""
        check(bool(alt) and "Heatmap" in alt, f"{w}x{h}: heatmap has a text alternative")
        p.screenshot(path=f"{OUT}/vp_{w}x{h}.png", full_page=False)
        if (w, h) == (1280, 800):
            # keyboard only: reach the highlight select and choose a factor
            p.locator("#highlight_select + .selectize-control input").focus()
            p.keyboard.type("x3"); p.keyboard.press("Enter"); idle(p, 2)
            check("Highlighted: x3" in p.inner_text("#focus_bar"), "keyboard: 'Highlight a factor' selects x3")
            det = p.locator("details.more").filter(has=p.locator("#mpi_table"))
            det.locator("summary").focus(); p.keyboard.press("Enter"); time.sleep(0.5)
            check(det.get_attribute("open") is not None and p.locator("#mpi_table table").is_visible(),
                  "keyboard: Enter opens the collapsed values table")
            # the 'More' menu holds the advanced view
            check(not p.locator("a[data-value='more']").is_visible(), "no empty 'More' menu while the Advanced View is off")
            p.evaluate("() => { document.querySelector('details.adv-settings').open = true; }")
            p.locator("#adv_show").check(); idle(p, 1.5)
            p.locator("a[data-value='more']").click(); time.sleep(0.8)
            item = p.locator("a[data-value='advanced']")
            check(item.is_visible() and "Advanced View" in item.inner_text(), "Advanced View appears under 'More' when switched on")
        p.context.close()
    b.close()

print(f"{len(failures)} failure(s)")
sys.exit(1 if failures else 0)
