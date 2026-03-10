import os
import json
from html import escape
from urllib.parse import quote

HTML_DIR = "html"
INDEX_FILE = "all_files.html"
SLIDESHOW_FILE = "index.html"


def parse_filename(filename):
    """
    Parse filenames like:
    Ryan_D._Morin_IDau0mkAAAAJ_blob1_Colorado_Trebuchet MS_0.6_black_90_wordcloud.html

    Expected structure from the right:
    <scholar_name_parts...>_<scholar_id>_<shape>_<theme>_<font>_<scale>_<background>_<rotation>_wordcloud.html
    """
    suffix = "_wordcloud.html"
    if not filename.endswith(suffix):
        return None

    base = filename[:-len(suffix)]
    parts = base.split("_")

    if len(parts) < 8:
        return None

    return {
        "scholar": " ".join(parts[:-7]),
        "scholar_id": parts[-7],
        "shape": parts[-6],
        "theme": parts[-5],
        "font": parts[-4],
        "scale": parts[-3],
        "background": parts[-2],
        "rotation": parts[-1],
        "file": filename,
        "href": f"{HTML_DIR}/{quote(filename)}"
    }


def find_wordcloud_files():
    if not os.path.isdir(HTML_DIR):
        raise FileNotFoundError(f"Directory not found: {HTML_DIR}")

    files = sorted(
        f for f in os.listdir(HTML_DIR)
        if f.endswith("_wordcloud.html")
    )

    parsed = []
    skipped = []

    for f in files:
        info = parse_filename(f)
        if info is None:
            skipped.append(f)
        else:
            parsed.append(info)

    return parsed, skipped


def write_index(rows):
    with open(INDEX_FILE, "w", encoding="utf-8") as out:
        out.write("""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>ScholarGoggler Word Clouds</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      margin: 40px;
      line-height: 1.4;
    }
    h1 {
      margin-bottom: 0.2em;
    }
    p.toplinks {
      margin-top: 0;
      margin-bottom: 1.5em;
    }
    table {
      border-collapse: collapse;
      width: 100%;
      max-width: 1200px;
    }
    th, td {
      border: 1px solid #ccc;
      padding: 8px 10px;
      text-align: left;
      vertical-align: top;
    }
    th {
      background: #f2f2f2;
    }
    tr:nth-child(even) {
      background: #fafafa;
    }
    a {
      text-decoration: none;
    }
    a:hover {
      text-decoration: underline;
    }
    .small {
      color: #666;
      font-size: 0.9em;
    }
  </style>
</head>
<body>
  <h1>ScholarGoggler Word Clouds</h1>
  <p class="toplinks">
    <a href="slideshow.html">Open slideshow preview</a>
  </p>

  <table>
    <tr>
      <th>Scholar</th>
      <th>Scholar ID</th>
      <th>Shape</th>
      <th>Theme</th>
      <th>Font</th>
      <th>Scale</th>
      <th>Background</th>
      <th>Rotation</th>
      <th>Link</th>
    </tr>
""")

        for r in rows:
            out.write(f"""    <tr>
      <td>{escape(r["scholar"])}</td>
      <td class="small">{escape(r["scholar_id"])}</td>
      <td>{escape(r["shape"])}</td>
      <td>{escape(r["theme"])}</td>
      <td>{escape(r["font"])}</td>
      <td>{escape(r["scale"])}</td>
      <td>{escape(r["background"])}</td>
      <td>{escape(r["rotation"])}</td>
      <td><a href="{r["href"]}">view</a></td>
    </tr>
""")

        out.write("""  </table>
</body>
</html>
""")


def write_slideshow(rows):
    slides = []
    for r in rows:
        slides.append({
            "file": r["href"],
            "label": (
                f'{r["scholar"]} | {r["shape"]} | {r["theme"]} | '
                f'{r["font"]} | scale {r["scale"]} | '
                f'bg {r["background"]} | rot {r["rotation"]}'
            )
        })

    slides_json = json.dumps(slides, ensure_ascii=False)

    with open(SLIDESHOW_FILE, "w", encoding="utf-8") as out:
        out.write(f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>ScholarGoggler Slideshow</title>
  <style>
    body {{
      font-family: Arial, sans-serif;
      margin: 0;
      padding: 20px;
      background: #f7f7f7;
    }}
    h1 {{
      margin-top: 0;
      margin-bottom: 0.2em;
    }}
    p.toplinks {{
      margin-top: 0;
      margin-bottom: 1em;
    }}
    .controls {{
      display: flex;
      gap: 8px;
      align-items: center;
      flex-wrap: wrap;
      margin-bottom: 12px;
    }}
    button {{
      padding: 6px 12px;
      font-size: 14px;
      cursor: pointer;
    }}
    input[type="number"] {{
      width: 70px;
      padding: 4px;
    }}
    label {{
      display: inline-flex;
      align-items: center;
      gap: 5px;
    }}
    .viewer {{
      background: white;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    }}
    iframe {{
      width: 100%;
      height: 78vh;
      border: none;
      background: white;
    }}
    .caption {{
      padding: 10px 14px;
      border-top: 1px solid #ddd;
      background: #fafafa;
      font-size: 14px;
    }}
    .meta {{
      color: #444;
      margin-top: 4px;
      word-break: break-word;
    }}
  </style>
</head>
<body>
  <h1>ScholarGoggler Slideshow</h1>


  <div class="controls">
    <button onclick="prevSlide()">Previous</button>
    <button onclick="togglePlay()" id="playBtn">Pause</button>
    <button onclick="nextSlide()">Next</button>
    <button onclick="reshuffle()">Shuffle Now</button>

    <label>
      <input id="shuffleMode" type="checkbox" checked>
      Shuffle mode
    </label>

    <label>
      Seconds per slide:
      <input id="delayInput" type="number" min="1" value="4">
    </label>
    <button onclick="applyDelay()">Apply</button>

    <span id="status"></span>
  </div>

  <div class="viewer">
    <iframe id="previewFrame" src=""></iframe>
    <div class="caption">
      <div><strong id="captionText"></strong></div>
      <div class="meta">
        <a id="fileLink" href="#" target="_blank">Open this file directly</a>
      </div>
    </div>
  </div>

  <script>
    const originalSlides = {slides_json};
    let slides = [...originalSlides];

    let current = 0;
    let playing = true;
    let delayMs = 4000;
    let timer = null;

    const frame = document.getElementById("previewFrame");
    const captionText = document.getElementById("captionText");
    const fileLink = document.getElementById("fileLink");
    const status = document.getElementById("status");
    const playBtn = document.getElementById("playBtn");
    const shuffleModeCheckbox = document.getElementById("shuffleMode");

    function shuffleArray(arr) {{
      for (let i = arr.length - 1; i > 0; i--) {{
        const j = Math.floor(Math.random() * (i + 1));
        [arr[i], arr[j]] = [arr[j], arr[i]];
      }}
    }}

    function resetSlides() {{
      slides = [...originalSlides];
      if (shuffleModeCheckbox.checked) {{
        shuffleArray(slides);
      }}
      current = 0;
    }}

    function render() {{
      if (slides.length === 0) {{
        captionText.textContent = "No word cloud files found.";
        fileLink.style.display = "none";
        frame.removeAttribute("src");
        status.textContent = "";
        return;
      }}

      const slide = slides[current];
      frame.src = slide.file;
      captionText.textContent = slide.label;
      fileLink.href = slide.file;
      fileLink.style.display = "";
      status.textContent = `Showing ${{current + 1}} of ${{slides.length}}`;
    }}

    function nextSlide() {{
      if (slides.length === 0) return;
      current = (current + 1) % slides.length;
      render();
    }}

    function prevSlide() {{
      if (slides.length === 0) return;
      current = (current - 1 + slides.length) % slides.length;
      render();
    }}

    function stopTimer() {{
      if (timer !== null) {{
        clearInterval(timer);
        timer = null;
      }}
    }}

    function startTimer() {{
      stopTimer();
      if (playing && slides.length > 1) {{
        timer = setInterval(nextSlide, delayMs);
      }}
    }}

    function togglePlay() {{
      playing = !playing;
      playBtn.textContent = playing ? "Pause" : "Play";
      startTimer();
    }}

    function applyDelay() {{
      const val = parseInt(document.getElementById("delayInput").value, 10);
      if (!isNaN(val) && val > 0) {{
        delayMs = val * 1000;
        startTimer();
      }}
    }}

    function reshuffle() {{
      resetSlides();
      render();
      startTimer();
    }}

    shuffleModeCheckbox.addEventListener("change", function() {{
      resetSlides();
      render();
      startTimer();
    }});

    resetSlides();
    render();
    startTimer();
  </script>
</body>
</html>
""")


def main():
    rows, skipped = find_wordcloud_files()

    rows.sort(key=lambda r: (
        r["scholar"].lower(),
        r["shape"].lower(),
        r["theme"].lower(),
        r["font"].lower(),
        r["scale"],
        r["background"].lower(),
        r["rotation"]
    ))

    write_index(rows)
    write_slideshow(rows)

    print(f"Wrote {INDEX_FILE} with {len(rows)} entries")
    print(f"Wrote {SLIDESHOW_FILE} with {len(rows)} entries")

    if skipped:
        print("\\nSkipped files with unexpected naming format:")
        for f in skipped:
            print(f"  - {f}")


if __name__ == "__main__":
    main()
