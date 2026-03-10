#!/usr/bin/env python3
"""
Poll Bluesky mentions and run scholar_bluesky.py when a post includes a Scholar ID.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import time
import urllib.request
from pathlib import Path
from typing import Dict, Optional


SCHOLAR_ID_RE = re.compile(r"\b[A-Za-z0-9_-]{12}\b")
SCHOLAR_URL_RE = re.compile(r"scholar\.google\.com/citations\?[^\\s]*user=([A-Za-z0-9_-]+)")


def load_config(path: Path) -> dict:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def get_attr(obj, name: str):
    if isinstance(obj, dict):
        return obj.get(name)
    return getattr(obj, name, None)


def extract_scholar_id(text: str) -> Optional[str]:
    if not text:
        return None
    m = SCHOLAR_URL_RE.search(text)
    if m:
        return m.group(1)
    # Prefer common Google Scholar ID length (12), but allow other lengths.
    candidates = SCHOLAR_ID_RE.findall(text)
    if not candidates:
        return None
    # Heuristic: pick the longest candidate (often matches ID better).
    return max(candidates, key=len)


def validate_scholar_id(scholar_id: str) -> bool:
    return len(scholar_id) == 12


def validate_scholar_url(scholar_id: str) -> bool:
    url = f"https://scholar.google.com/citations?user={scholar_id}"
    try:
        req = urllib.request.Request(url, method="GET", headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req, timeout=10) as resp:
            return 200 <= resp.status < 300
    except Exception:
        return False


def load_state(path: Path) -> dict:
    if not path.exists():
        return {"seen": {}, "actor_last": {}}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {"seen": {}, "actor_last": {}}


def save_state(path: Path, data: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2, sort_keys=True), encoding="utf-8")


def _byte_index(text: str, char_index: int) -> int:
    return len(text[:char_index].encode("utf-8"))


def build_facets(client, text: str):
    from atproto import models
    import re

    url_re = re.compile(r"https?://[^\s)]+")
    handle_re = re.compile(r"@([a-zA-Z0-9][a-zA-Z0-9.-]+\.[a-zA-Z0-9.-]+)")

    facets = []
    spans = []

    for m in url_re.finditer(text):
        spans.append(("link", m.start(), m.end(), m.group(0)))
    for m in handle_re.finditer(text):
        spans.append(("mention", m.start(), m.end(), m.group(1)))

    spans.sort(key=lambda x: x[1])
    used = []
    did_cache: Dict[str, str] = {}

    for kind, start, end, value in spans:
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
        else:
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
        used.append((start, end))

    return facets or None


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run ScholarGoggler when mentioned on Bluesky.",
        epilog=(
            "Examples:\n"
            "  python3 scripts/mention_bot.py --once\n"
            "  python3 scripts/mention_bot.py --dry-run --once\n"
            "  python3 scripts/mention_bot.py --single-post --reuse-existing\n"
        ),
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        "--config",
        default=str(Path(__file__).resolve().parents[1] / "scripts" / "scholar_bluesky.example.json"),
        help="JSON config with Bluesky handle and app password.",
    )
    parser.add_argument(
        "--state-file",
        default="cache/mentions_state.json",
        help="State file to track processed mentions and per-author cooldown.",
    )
    parser.add_argument(
        "--poll-seconds",
        type=int,
        default=120,
        help="Polling interval in seconds (ignored with --once).",
    )
    parser.add_argument(
        "--once",
        action="store_true",
        help="Run a single poll cycle and exit.",
    )
    parser.add_argument(
        "--max-per-run",
        type=int,
        default=3,
        help="Maximum mentions to process per run.",
    )
    parser.add_argument(
        "--cooldown-hours",
        type=int,
        default=24,
        help="Minimum hours between processing requests from the same handle.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Do not post or run the generator; print actions only.",
    )
    parser.add_argument(
        "--reply",
        action="store_true",
        default=True,
        help="Reply to mentions (acknowledgements/invalid responses).",
    )
    parser.add_argument(
        "--no-reply",
        action="store_true",
        help="Disable all replies.",
    )
    parser.add_argument(
        "--ack-template",
        default="Got it @{handle}! Generating your word clouds now.",
        help="Immediate acknowledgement for valid IDs; supports {handle} and {scholar_id}.",
    )
    parser.add_argument(
        "--invalid-template",
        default=(
            "Sorry @{handle} — I couldn't validate that Scholar ID. "
            "Please send a Google Scholar profile URL like "
            "https://scholar.google.com/citations?user=ID."
        ),
        help="Reply when ID validation fails; supports {handle}.",
    )
    parser.add_argument(
        "--cooldown-template",
        default="Thanks @{handle}! I can only run once per {hours} hours. Please try again later.",
        help="Reply when user is in cooldown; supports {handle} and {hours}.",
    )
    parser.add_argument(
        "--single-post",
        action="store_true",
        help="Pass --single-post to scholar_bluesky.py (combine images into one post).",
    )
    parser.add_argument(
        "--reuse-existing",
        action="store_true",
        help="Pass --reuse-existing to scholar_bluesky.py (skip R generation if files exist).",
    )
    parser.add_argument(
        "--skip-posted-check",
        action="store_true",
        help="Pass --skip-posted-check to scholar_bluesky.py (allow re-posting).",
    )
    parser.add_argument(
        "--validate-url",
        action="store_true",
        default=True,
        help="Validate Scholar ID by fetching the Scholar profile URL (default on).",
    )
    parser.add_argument(
        "--no-validate-url",
        action="store_true",
        help="Disable Scholar URL validation (faster, less safe).",
    )
    args = parser.parse_args()

    validate_url = args.validate_url and not args.no_validate_url

    config = load_config(Path(args.config))
    handle = config.get("handle")
    app_password = config.get("app_password")
    if not handle or not app_password:
        print("Missing handle/app_password in config.", file=sys.stderr)
        return 2

    from atproto import Client, models

    client = Client()
    client.login(handle, app_password)

    state_path = Path(args.state_file)
    state = load_state(state_path)

    def can_process(actor: str, now_ts: float) -> bool:
        last = state.get("actor_last", {}).get(actor)
        if last is None:
            return True
        return (now_ts - last) >= args.cooldown_hours * 3600

    while True:
        processed = 0
        now_ts = time.time()
        try:
            notif_res = None
            try:
                notif_res = client.app.bsky.notification.listNotifications({"limit": 50})
            except Exception:
                try:
                    notif_res = client.app.bsky.notification.list_notifications({"limit": 50})
                except Exception:
                    try:
                        notif_res = client.list_notifications(limit=50)
                    except Exception:
                        notif_res = None
            if notif_res is None:
                raise RuntimeError("Notifications API not available in atproto client.")
            notifs = notif_res.notifications if hasattr(notif_res, "notifications") else notif_res
        except Exception as exc:
            print(f"Failed to list notifications: {exc}", file=sys.stderr)
            return 1

        for n in notifs:
            reason = get_attr(n, "reason") or ""
            if reason != "mention":
                continue
            uri = get_attr(n, "uri") or ""
            cid = get_attr(n, "cid") or ""
            key = uri or cid
            if not key:
                continue
            if state.get("seen", {}).get(key):
                continue

            record = get_attr(n, "record") or {}
            text = get_attr(record, "text") or ""
            scholar_id = extract_scholar_id(text)
            author = get_attr(get_attr(n, "author") or {}, "handle") or ""
            invalid = False
            if not scholar_id:
                invalid = True
            elif not validate_scholar_id(scholar_id):
                invalid = True
            elif validate_url and not validate_scholar_url(scholar_id):
                invalid = True

            if invalid:
                if args.reply and not args.no_reply and author:
                    reply_text = args.invalid_template.format(handle=author)
                    facets = build_facets(client, reply_text)
                    reply_to = {
                        "root": {"cid": get_attr(n, "cid"), "uri": get_attr(n, "uri")},
                        "parent": {"cid": get_attr(n, "cid"), "uri": get_attr(n, "uri")},
                    }
                    try:
                        client.send_post(
                            text=reply_text,
                            reply_to=reply_to,
                            facets=facets,
                        )
                    except Exception as exc:
                        print(f"Reply failed: {exc}", file=sys.stderr)
                state.setdefault("seen", {})[key] = True
                save_state(state_path, state)
                continue

            if author and not can_process(author, now_ts):
                if args.reply and not args.no_reply and author:
                    cooldown_text = args.cooldown_template.format(
                        handle=author, hours=args.cooldown_hours
                    )
                    facets = build_facets(client, cooldown_text)
                    reply_to = {
                        "root": {"cid": get_attr(n, "cid"), "uri": get_attr(n, "uri")},
                        "parent": {"cid": get_attr(n, "cid"), "uri": get_attr(n, "uri")},
                    }
                    try:
                        client.send_post(
                            text=cooldown_text,
                            reply_to=reply_to,
                            facets=facets,
                        )
                    except Exception as exc:
                        print(f"Reply failed: {exc}", file=sys.stderr)
                state.setdefault("seen", {})[key] = True
                save_state(state_path, state)
                continue

            if args.reply and not args.no_reply and author:
                ack_text = args.ack_template.format(handle=author, scholar_id=scholar_id)
                facets = build_facets(client, ack_text)
                reply_to = {
                    "root": {"cid": get_attr(n, "cid"), "uri": get_attr(n, "uri")},
                    "parent": {"cid": get_attr(n, "cid"), "uri": get_attr(n, "uri")},
                }
                try:
                    client.send_post(
                        text=ack_text,
                        reply_to=reply_to,
                        facets=facets,
                    )
                except Exception as exc:
                    print(f"Reply failed: {exc}", file=sys.stderr)

            cmd = [
                sys.executable,
                str(Path(__file__).resolve().parents[1] / "scripts" / "scholar_bluesky.py"),
                "--scholar-id",
                scholar_id,
            ]
            if args.single_post:
                cmd.append("--single-post")
            if args.reuse_existing:
                cmd.append("--reuse-existing")
            if args.skip_posted_check:
                cmd.append("--skip-posted-check")
            if args.dry_run:
                cmd.append("--dry-run")

            print(f"Processing mention {key}: {scholar_id}")
            if args.dry_run:
                print("Dry run - would run:", " ".join(cmd))
            else:
                try:
                    subprocess.run(cmd, check=True)
                except Exception as exc:
                    print(f"Run failed: {exc}", file=sys.stderr)
                    state.setdefault("seen", {})[key] = True
                    save_state(state_path, state)
                    continue

            # No post-completion reply; the generated post should mention the user.

            state.setdefault("seen", {})[key] = True
            if author:
                state.setdefault("actor_last", {})[author] = now_ts
            save_state(state_path, state)
            processed += 1
            if processed >= args.max_per_run:
                break

        if args.once:
            break
        time.sleep(args.poll_seconds)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
