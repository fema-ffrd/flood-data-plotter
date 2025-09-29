import json
from pathlib import Path

import requests
from yarl import URL

# local development server port
LOCAL_HOST = "127.0.0.1"
LOCAL_PORT = 8081  #

# Docker container server port
DOCKER_HOST = "localhost"
DOCKER_PORT = 8080

WKDIR = Path(__file__).parent.resolve()
EXAMPLE_FILE_PATH = f"{WKDIR}/src/example-jsons/flow-example.json".replace("examples", "server")  # adjust if needed
EXAMPLE_OUTPUT_PATH = f"{WKDIR}/test-output/Flow_AMS_Green_200th_01Hour"

HTML_AND_PNG = True


def save_html_to_png(html_path, png_path, selector=None, width=1280, height=800, scale=2, wait_ms=500):
    """Render an HTML file to PNG using Playwright.

    - selector=None -> full-page screenshot
    - selector="css" -> screenshot of first matching element
    """
    try:
        from playwright.sync_api import sync_playwright
    except ImportError as e:
        raise RuntimeError(
            "PNG output requested but Playwright is not installed. "
            "Run: pip install playwright && playwright install chromium"
        ) from e

    html_file = Path(html_path).resolve()
    png_file = Path(png_path)
    if png_file.parent and not png_file.parent.exists():
        png_file.parent.mkdir(parents=True, exist_ok=True)

    with sync_playwright() as p:
        browser = p.chromium.launch()
        context = browser.new_context(
            viewport={"width": int(width), "height": int(height)},
            device_scale_factor=scale,
        )
        page = context.new_page()
        page.goto(f"file://{html_file}", wait_until="load")
        if wait_ms:
            page.wait_for_timeout(wait_ms)

        if selector:
            loc = page.locator(selector).first
            loc.wait_for(state="visible", timeout=10_000)
            loc.screenshot(path=str(png_file))
        else:
            # Full page screenshot
            page.screenshot(path=str(png_file), full_page=True)

        browser.close()

    return png_file


def make_plot_request(json_data, url, output_html_path, output_png_path=None, png_selector=None, timeout_sec=60):
    """Read JSON from file_path, POSTs it to url, writes HTML to output_html_path.

    If output_png_path is provided, also renders the HTML to PNG (optionally a specific element via css selector).
    Returns {'response': requests.Response, 'html': str, 'png_path': Optional[Path]}.
    """

    # Send request
    try:
        resp = requests.post(
            url,
            json=json_data,  # requests sets Content-Type automatically
            headers={"Content-Type": "application/json"},
            timeout=timeout_sec,
        )
    except requests.RequestException as e:
        raise RuntimeError(f"HTTP request failed: {e}") from e

    # Save HTML (even on error, for debugging)
    content = resp.text
    out_path = Path(output_html_path)
    if out_path.parent and not out_path.parent.exists():
        out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(content, encoding="utf-8")

    if not resp.ok:
        # Keep behavior: print failure but continue (HTML saved above)
        print(f"Failed: Server returned HTTP {resp.status_code}.\n\n{resp.text[:2000]}")

    png_path = None
    # Only attempt PNG if a path is provided and the response was OK
    if output_png_path and resp.ok:
        try:
            png_path = save_html_to_png(
                html_path=out_path,
                png_path=output_png_path,
                selector=png_selector,  # e.g., "#plot" if you want just one element
                width=1280,
                height=800,
                scale=2,  # higher = crisper image
                wait_ms=500,  # brief settle time for any JS/layout
            )
            print(f"PNG saved to: {Path(png_path).resolve()}")
        except Exception as e:
            print(f"PNG export failed: {e}")

    print(f"Request successful. File saved to: {out_path.resolve()}")
    return {"response": resp, "content": content}


# --------- RUN SCRIPT
if __name__ == "__main__":
    print(f"Working directory: {WKDIR}")

    LOCAL_URL = f"http://{LOCAL_HOST}:{LOCAL_PORT}/api"
    DOCKER_URL = f"http://{DOCKER_HOST}:{DOCKER_PORT}/api"

    # Try to connect to local development server first, then Docker
    try:
        if requests.get(f"{LOCAL_URL}/health/status", timeout=2).status_code == 200:
            url = f"{LOCAL_URL}/realization/flows"
            print(f"✅ Connected to local development server at {LOCAL_URL}")
    except (requests.exceptions.RequestException, requests.exceptions.Timeout):
        try:
            if requests.get(f"{DOCKER_URL}/health/status", timeout=2).status_code == 200:
                url = f"{DOCKER_URL}/realization/flows"
                print(f"✅ Connected to Docker server at {DOCKER_URL}")
        except (requests.exceptions.RequestException, requests.exceptions.Timeout):
            raise RuntimeError(
                f"Could not connect to server at:\n"
                f"  - Local development: {LOCAL_URL}\n"
                f"  - Docker container: {DOCKER_URL}\n"
            )

            # Load JSON payload
    with open(EXAMPLE_FILE_PATH, "r", encoding="utf-8") as f:
        json_data = json.load(f)

    if json_data.get("json") is True:
        print("Requesting JSON output from server...")
        output_json_path = f"{EXAMPLE_OUTPUT_PATH}.json"
        make_plot_request(json_data, url, output_json_path)
    else:
        print("Requesting HTML output from server...")
        output_html_path = f"{EXAMPLE_OUTPUT_PATH}.html"

        if HTML_AND_PNG:
            output_png_path = f"{EXAMPLE_OUTPUT_PATH}.png"
            make_plot_request(json_data, url, output_html_path, output_png_path=output_png_path)
        else:
            make_plot_request(json_data, url, output_html_path)
