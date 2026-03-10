#!/usr/bin/env python3
"""
Run standalone.R multiple times for a given scholar and post results to Bluesky.

Requires:
  - Rscript on PATH
  - R packages used by standalone.R already installed
  - Python package: atproto

Auth:
  - BSKY_HANDLE and BSKY_APP_PASSWORD env vars (recommended), or CLI flags.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path
from shutil import copy2
from typing import Dict, List, Optional, Tuple


def run_rscript(
    rscript: str,
    standalone_path: Path,
    args: List[str],
) -> None:
    if "--quiet" not in args:
        args = args + ["--quiet"]
    cmd = [rscript, str(standalone_path)] + args
    subprocess.run(cmd, check=True)


def list_new_clouds(
    clouds_dir: Path,
    since_ts: float,
) -> List[Tuple[Path, Path]]:
    pngs = sorted(
        [
            p
            for p in clouds_dir.glob("*_wordcloud*.png")
            if p.stat().st_mtime >= since_ts
        ],
        key=lambda p: p.stat().st_mtime,
    )
    pairs = []
    for png in pngs:
        alt = png.with_name(png.name + "_alt.txt")
        pairs.append((png, alt))
    return pairs


def filter_pairs_by_id_or_key(
    pairs: List[Tuple[Path, Path]],
    scholar_id: Optional[str],
    scholar_key: Optional[str],
) -> List[Tuple[Path, Path]]:
    if scholar_id:
        return [pair for pair in pairs if scholar_id in pair[0].name]
    if scholar_key:
        return [pair for pair in pairs if scholar_key in pair[0].name]
    return pairs


def read_alt_text(alt_path: Path) -> str:
    if alt_path.exists():
        return alt_path.read_text(encoding="utf-8").strip()
    return "Word cloud visualization generated from a Google Scholar profile."


def infer_scholar_name_from_file(png_path: Path, scholar_id: Optional[str] = None) -> Optional[str]:
    # Filename format (after update): {scholar_name}_{scholar_id}_{shape}_..._wordcloud.png
    name_part = png_path.name.split("_wordcloud", 1)[0]
    if scholar_id and f"_{scholar_id}_" in name_part:
        return name_part.split(f"_{scholar_id}_", 1)[0].replace("_", " ")
    # Fallback: use all tokens before the last 6 fields (shape/scheme/font/zoom/bg/rot)
    parts = name_part.split("_")
    if len(parts) > 6:
        return " ".join(parts[:-6]).replace("_", " ")
    return None


def _byte_index(text: str, char_index: int) -> int:
    return len(text[:char_index].encode("utf-8"))


def build_facets(client, text: str):
    from atproto import models
    import re

    url_re = re.compile(r"https?://[^\s)]+")
    handle_re = re.compile(r"@([a-zA-Z0-9][a-zA-Z0-9.-]+\.[a-zA-Z0-9.-]+)")
    tag_re = re.compile(r"#([A-Za-z0-9_]+)")

    facets = []
    spans = []

    for m in url_re.finditer(text):
        spans.append(("link", m.start(), m.end(), m.group(0)))
    for m in handle_re.finditer(text):
        spans.append(("mention", m.start(), m.end(), m.group(1)))
    for m in tag_re.finditer(text):
        spans.append(("tag", m.start(), m.end(), m.group(1)))

    spans.sort(key=lambda x: x[1])
    used = []
    did_cache: Dict[str, str] = {}

    for kind, start, end, value in spans:
        # avoid overlap
        if any(start < u_end and end > u_start for u_start, u_end in used):
            continue
        if kind == "link":
            uri = value
            try:
                feature = models.AppBskyRichtextFacet.Link(uri=uri)
                index = models.AppBskyRichtextFacet.ByteSlice(
                    byte_start=_byte_index(text, start),
                    byte_end=_byte_index(text, end),
                )
                facets.append(models.AppBskyRichtextFacet.Main(index=index, features=[feature]))
            except Exception:
                facets.append(
                    {
                        "index": {
                            "byteStart": _byte_index(text, start),
                            "byteEnd": _byte_index(text, end),
                        },
                        "features": [
                            {"$type": "app.bsky.richtext.facet#link", "uri": uri}
                        ],
                    }
                )
        elif kind == "mention":
            handle = value
            did = did_cache.get(handle)
            if not did:
                try:
                    prof = client.get_profile(handle)
                except Exception:
                    prof = None
                if prof is None:
                    continue
                did = getattr(prof, "did", None) or (prof.get("did") if isinstance(prof, dict) else None)
                if not did:
                    continue
                did_cache[handle] = did
            try:
                feature = models.AppBskyRichtextFacet.Mention(did=did)
                index = models.AppBskyRichtextFacet.ByteSlice(
                    byte_start=_byte_index(text, start),
                    byte_end=_byte_index(text, end),
                )
                facets.append(models.AppBskyRichtextFacet.Main(index=index, features=[feature]))
            except Exception:
                facets.append(
                    {
                        "index": {
                            "byteStart": _byte_index(text, start),
                            "byteEnd": _byte_index(text, end),
                        },
                        "features": [
                            {"$type": "app.bsky.richtext.facet#mention", "did": did}
                        ],
                    }
                )
        else:
            tag = value
            try:
                feature = models.AppBskyRichtextFacet.Tag(tag=tag)
                index = models.AppBskyRichtextFacet.ByteSlice(
                    byte_start=_byte_index(text, start),
                    byte_end=_byte_index(text, end),
                )
                facets.append(models.AppBskyRichtextFacet.Main(index=index, features=[feature]))
            except Exception:
                facets.append(
                    {
                        "index": {
                            "byteStart": _byte_index(text, start),
                            "byteEnd": _byte_index(text, end),
                        },
                        "features": [
                            {"$type": "app.bsky.richtext.facet#tag", "tag": tag}
                        ],
                    }
                )
        used.append((start, end))

    return facets or None


def extract_topics_from_alt(alt_text: str, max_words: int = 6) -> str:
    marker = "include"
    idx = alt_text.lower().find(marker)
    if idx == -1:
        return ""
    tail = alt_text[idx + len(marker) :].strip()
    words = [w.strip(".,;:!()[]\"'") for w in tail.split()]
    words = [w for w in words if w]
    if not words:
        return ""
    return ", ".join(words[:max_words])


def load_handles_map(path: Path) -> dict:
    if not path.exists():
        return {}
    lines = path.read_text(encoding="utf-8").splitlines()
    if not lines:
        return {}
    header = lines[0].split("\t")
    idx_id = header.index("scholar_id") if "scholar_id" in header else None
    idx_handle = None
    if "handle" in header:
        idx_handle = header.index("handle")
    elif "bsky_handle" in header:
        idx_handle = header.index("bsky_handle")
    if idx_id is None or idx_handle is None:
        return {}
    mapping = {}
    for line in lines[1:]:
        if not line.strip():
            continue
        parts = line.split("\t")
        if len(parts) <= max(idx_id, idx_handle):
            continue
        scholar_id = parts[idx_id].strip()
        handle = parts[idx_handle].strip()
        if scholar_id and handle:
            mapping[scholar_id] = handle
    return mapping


def post_to_bluesky(
    handle: str,
    app_password: str,
    text: str,
    images: List[Tuple[Path, str]],
    thread: bool,
    single_post: bool,
) -> None:
    from atproto import Client, models

    client = Client()
    client.login(handle, app_password)
    facets = build_facets(client, text)

    if single_post:
        if len(images) > 4:
            raise ValueError("Bluesky allows up to 4 images per post.")
        embed_images = []
        for img_path, alt_text in images:
            with img_path.open("rb") as f:
                blob_resp = client.upload_blob(f.read())
            blob = blob_resp.blob if hasattr(blob_resp, "blob") else blob_resp
            embed_images.append(models.AppBskyEmbedImages.Image(alt=alt_text, image=blob))
        embed = models.AppBskyEmbedImages.Main(images=embed_images)
        client.send_post(text=text, embed=embed, facets=facets)
        return

    root = None
    parent = None

    for i, (img_path, alt_text) in enumerate(images):
        with img_path.open("rb") as f:
            blob_resp = client.upload_blob(f.read())
        blob = blob_resp.blob if hasattr(blob_resp, "blob") else blob_resp

        embed = models.AppBskyEmbedImages.Main(
            images=[models.AppBskyEmbedImages.Image(alt=alt_text, image=blob)]
        )

        post_text = text if i == 0 else ""
        post_facets = facets if i == 0 else None

        if thread and root is not None and parent is not None:
            res = client.send_post(
                text=post_text,
                embed=embed,
                reply_to=models.AppBskyFeedPost.ReplyRef(root=root, parent=parent),
                facets=post_facets,
            )
        else:
            res = client.send_post(text=post_text, embed=embed, facets=post_facets)

        if thread and root is None:
            root = res
        if thread:
            parent = res


def build_r_args(args: argparse.Namespace) -> List[str]:
    r_args = []
    if args.scholar_id:
        r_args += ["--scholar_id", args.scholar_id]
    if args.first_name:
        r_args += ["--first_name", args.first_name]
    if args.last_name:
        r_args += ["--last_name", args.last_name]
    if args.year_min is not None:
        r_args += ["--year_min", str(args.year_min)]
    if args.year_max is not None:
        r_args += ["--year_max", str(args.year_max)]
    if args.bg_colour:
        r_args += ["--bg_colour", args.bg_colour]
    if args.shape:
        r_args += ["--shape", args.shape]
    if args.rotation:
        r_args += ["--rotation", args.rotation]
    if args.font_family:
        r_args += ["--font_family", args.font_family]
    if args.colour_scheme:
        r_args += ["--colour_scheme", args.colour_scheme]
    if args.zoomout is not None:
        r_args += ["--zoomout", str(args.zoomout)]
    return r_args


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run standalone.R multiple times and post results to Bluesky."
    )
    parser.add_argument(
        "--config",
        default=str(Path(__file__).resolve().parents[1] / "scripts" / "scholar_bluesky.example.json"),
        help="Path to JSON config file with defaults and optional scholars list.",
    )
    parser.add_argument(
        "--scholar-key",
        help="Short label used to identify outputs and fill templates (optional).",
    )
    parser.add_argument("--scholar-id", dest="scholar_id")
    parser.add_argument("--first-name", dest="first_name")
    parser.add_argument("--last-name", dest="last_name")
    parser.add_argument("--runs", type=int, default=3)
    parser.add_argument("--year-min", type=int)
    parser.add_argument("--year-max", type=int)
    parser.add_argument("--bg-colour")
    parser.add_argument("--shape")
    parser.add_argument("--rotation")
    parser.add_argument("--font-family")
    parser.add_argument("--colour-scheme")
    parser.add_argument("--zoomout", type=float)
    parser.add_argument("--text", help="Post text. Defaults to a generated caption.")
    parser.add_argument("--thread", action="store_true", help="Post as a thread.")
    parser.add_argument(
        "--single-post",
        action="store_true",
        help="Combine all images into a single post (max 4 images).",
    )
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--reuse-existing",
        action="store_true",
        help="Skip R generation if enough matching images already exist.",
    )
    parser.add_argument("--rscript", default="Rscript")
    parser.add_argument(
        "--posted-file",
        default=str(Path(__file__).resolve().parents[1] / "posted.tsv"),
        help="TSV file tracking already-posted scholar IDs.",
    )
    parser.add_argument(
        "--skip-posted-check",
        action="store_true",
        help="Skip posted.tsv check (allow re-posting a scholar ID).",
    )
    parser.add_argument(
        "--html-dir",
        default=str(Path(__file__).resolve().parents[1] / "html"),
        help="Directory to copy temp.html outputs into.",
    )
    parser.add_argument(
        "--handles-file",
        default=str(Path(__file__).resolve().parents[1] / "prioritize.tsv"),
        help="TSV with scholar_id and handle columns for mention lookup.",
    )
    parser.add_argument(
        "--standalone",
        default=str(Path(__file__).resolve().parents[1] / "standalone.R"),
    )
    parser.add_argument(
        "--clouds-dir",
        default=str(Path(__file__).resolve().parents[1] / "clouds"),
    )
    parser.add_argument("--handle", default=os.getenv("BSKY_HANDLE"))
    parser.add_argument("--app-password", default=os.getenv("BSKY_APP_PASSWORD"))

    args = parser.parse_args()

    config = {}
    if args.config:
        config_path = Path(args.config)
        try:
            config = json.loads(config_path.read_text(encoding="utf-8"))
        except Exception as exc:
            print(f"Failed to read config: {exc}", file=sys.stderr)
            return 2

    def get_val(name, default=None):
        cli_val = getattr(args, name)
        if cli_val is not None:
            return cli_val
        return config.get(name, default)

    scholars = [None]

    if not (args.scholar_id or (args.first_name and args.last_name)):
        print("Provide --scholar-id or both --first-name and --last-name.", file=sys.stderr)
        return 2

    clouds_dir = Path(get_val("clouds_dir", args.clouds_dir))
    clouds_dir.mkdir(parents=True, exist_ok=True)
    standalone_path = Path(get_val("standalone", args.standalone))

    html_dir = Path(get_val("html_dir", args.html_dir))
    html_dir.mkdir(parents=True, exist_ok=True)
    handles_map = load_handles_map(Path(get_val("handles_file", args.handles_file)))

    for scholar in scholars:
        # Scholar-specific overrides
        scholar_id = args.scholar_id
        first_name = args.first_name
        last_name = args.last_name
        full_name = None

        if not scholar_id and not (first_name and last_name):
            print("Scholar entry missing id or name. Skipping.", file=sys.stderr)
            continue

        local_args = argparse.Namespace(**vars(args))
        local_args.scholar_id = scholar_id
        local_args.first_name = first_name
        local_args.last_name = last_name
        local_args.runs = int(get_val("runs", args.runs))
        local_args.year_min = get_val("year_min", args.year_min)
        local_args.year_max = get_val("year_max", args.year_max)
        local_args.bg_colour = get_val("bg_colour", args.bg_colour)
        local_args.shape = get_val("shape", args.shape)
        local_args.rotation = get_val("rotation", args.rotation)
        local_args.font_family = get_val("font_family", args.font_family)
        local_args.colour_scheme = get_val("colour_scheme", args.colour_scheme)
        local_args.zoomout = get_val("zoomout", args.zoomout)
        local_args.text = get_val("text", args.text)
        local_args.thread = bool(get_val("thread", args.thread))
        local_args.handle = get_val("handle", args.handle)
        local_args.app_password = get_val("app_password", args.app_password)
        local_args.reuse_existing = bool(get_val("reuse_existing", args.reuse_existing))

        if scholar_id and not args.dry_run and not args.skip_posted_check:
            posted_path = Path(get_val("posted_file", args.posted_file))
            if posted_path.exists():
                posted_ids = set()
                for line in posted_path.read_text(encoding="utf-8").splitlines():
                    line = line.strip()
                    if not line or line.startswith("#"):
                        continue
                    posted_ids.add(line)
                if scholar_id in posted_ids:
                    print(
                        f"Scholar ID {scholar_id} already in {posted_path.name}. Aborting.",
                        file=sys.stderr,
                    )
                    return 3

        r_args = build_r_args(local_args)

        existing_pairs = list_new_clouds(clouds_dir, 0)
        existing_pairs = filter_pairs_by_id_or_key(
            existing_pairs, scholar_id, args.scholar_key
        )

        if local_args.reuse_existing and len(existing_pairs) >= local_args.runs:
            new_pairs = existing_pairs[-local_args.runs :]
        else:
            new_pairs = []
            for _ in range(local_args.runs):
                run_start = time.time()
                run_rscript(args.rscript, standalone_path, r_args)
                run_pairs = list_new_clouds(clouds_dir, run_start)
                run_pairs = filter_pairs_by_id_or_key(
                    run_pairs, scholar_id, args.scholar_key
                )
                if not run_pairs:
                    continue
                # Use the newest file from this run and snapshot temp.html
                newest_png, newest_alt = run_pairs[-1]
                html_name = newest_png.name.replace(".png", ".html")
                temp_html = Path(__file__).resolve().parents[1] / "temp.html"
                if temp_html.exists():
                    copy2(temp_html, html_dir / html_name)
                new_pairs.append((newest_png, newest_alt))

        if not new_pairs:
            print("No new wordcloud images found.", file=sys.stderr)
            continue

        new_pairs = new_pairs[-local_args.runs :]
        images = [(png, read_alt_text(alt)) for png, alt in new_pairs]

        name_guess = infer_scholar_name_from_file(images[0][0], scholar_id)
        years = []
        if local_args.year_min is not None:
            years.append(str(local_args.year_min))
        if local_args.year_max is not None:
            years.append(str(local_args.year_max))
        year_str = f"{'-'.join(years)}" if years else ""
        who = (
            " ".join([first_name, last_name]).strip() if first_name and last_name else None
        )
        who = who or name_guess or "Scholar"
        if args.scholar_key:
            who = who or args.scholar_key

        topics = extract_topics_from_alt(images[0][1])
        template_vars = {
            "user_full_name": who,
            "first_name": first_name or "",
            "last_name": last_name or "",
            "scholar_id": scholar_id or "",
            "scholar_key": args.scholar_key or "",
            "year_min": local_args.year_min or "",
            "year_max": local_args.year_max or "",
            "years": year_str,
            "topics": topics,
            "bsky_handle": handles_map.get(scholar_id or "", ""),
        }
        if template_vars["bsky_handle"]:
            template_vars["bsky_handle_at"] = f"@{template_vars['bsky_handle']}"
        else:
            template_vars["bsky_handle_at"] = ""

        if local_args.text:
            try:
                template = local_args.text.replace("{@bsky_handle}", "{bsky_handle_at}")
                post_text = template.format(**template_vars)
            except Exception:
                post_text = local_args.text
        else:
            post_text = f"Scholar Goggler: {who}"

        if local_args.dry_run:
            print("Dry run. Would post:")
            print("Text (formatted):")
            print(post_text)
            if local_args.text:
                print("Text (template):")
                print(local_args.text)
            if template_vars.get("bsky_handle"):
                print(f"Handle: @{template_vars['bsky_handle']}")
            print("Images:")
            for img, alt in images:
                print(f"- {img.name}")
                print("  Alt:")
                print(f"  {alt}")
            continue

        if not local_args.handle or not local_args.app_password:
            print(
                "Missing Bluesky credentials. Set BSKY_HANDLE and BSKY_APP_PASSWORD "
                "or pass --handle and --app-password.",
                file=sys.stderr,
            )
            return 2

        post_to_bluesky(
            local_args.handle,
            local_args.app_password,
            post_text,
            images,
            local_args.thread,
            local_args.single_post,
        )
        if scholar_id:
            posted_path = Path(get_val("posted_file", args.posted_file))
            posted_path.parent.mkdir(parents=True, exist_ok=True)
            with posted_path.open("a", encoding="utf-8") as f:
                f.write(f"{scholar_id}\n")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
