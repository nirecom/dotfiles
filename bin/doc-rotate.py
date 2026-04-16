#!/usr/bin/env python3
"""Rotate a history.md by archiving old entries to history/YYYY.md files.

Generates history/index.md with year-grouped list of all entries.

Usage:
    uv run bin/doc-rotate.py <path> [--threshold-warn 500] [--threshold-hard 800]
        [--floor 20] [--dry-run]
"""

import argparse
import re
import sys
from datetime import date as Date, timedelta
from pathlib import Path

DATE_RE = re.compile(r"\((\d{4}-\d{2}-\d{2}),")
ARCHIVED_SECTION_RE = re.compile(
    r"^## Archived\n.*?(?=\n## |\Z)", re.MULTILINE | re.DOTALL
)

WARN_LINES = 500
HARD_LINES = 800
DEFAULT_FLOOR = 20


def _parse_entries(content: str):
    """Parse history.md into (entries, preamble) tuple.

    Each entry dict: {header, body_lines, date (Date|None)}
    preamble: text before first ### entry (e.g. # title, ## section headings)
    """
    entries = []
    lines = content.split("\n")
    current = None
    preamble_lines = []

    for line in lines:
        if line.startswith("## "):
            if current is not None:
                entries.append(current)
                current = None
            preamble_lines.append(line)
        elif line.startswith("### "):
            if current is not None:
                entries.append(current)
            current = {"header": line, "body_lines": [], "date": _parse_date(line)}
        else:
            if current is not None:
                current["body_lines"].append(line)
            else:
                preamble_lines.append(line)

    if current is not None:
        entries.append(current)

    return entries, "\n".join(preamble_lines)


def _parse_date(header_line: str) -> Date | None:
    m = DATE_RE.search(header_line)
    if not m:
        return None
    try:
        return Date.fromisoformat(m.group(1))
    except ValueError:
        return None


def _entry_text(entry: dict) -> str:
    body = "\n".join(entry["body_lines"])
    return f"{entry['header']}\n{body}" if body else entry["header"]


def _make_anchor(header: str) -> str:
    """Generate a GitHub-style markdown anchor from a ### header line."""
    text = re.sub(r"^### #?\d*:?\s*", "", header)
    text = re.sub(r"\s*\(.*?\)", "", text).strip().lower()
    text = re.sub(r"[^a-z0-9\s-]", "", text)
    text = re.sub(r"\s+", "-", text)
    return text


def _first_sentence(body_lines: list[str]) -> str:
    """Extract first sentence from Background/Cause line."""
    for line in body_lines:
        if line.startswith("Background:") or line.startswith("Cause:"):
            text = re.sub(r"^(?:Background|Cause):\s*", "", line)
            m = re.match(r"([^.!?]+[.!?]?)", text)
            if m:
                return m.group(1).strip()[:120]
    return ""


def rotate(path: Path, floor: int, dry_run: bool) -> int:
    """Perform rotation. Returns 0 on success."""
    if not path.exists():
        print(f"Error: {path} not found", file=sys.stderr)
        return 1

    content = path.read_text(encoding="utf-8")
    entries, preamble = _parse_entries(content)

    if not entries:
        return 0

    today = Date.today()
    cutoff = today - timedelta(days=365)

    dated = [(e, e["date"]) for e in entries if e["date"] is not None]
    undated = [e for e in entries if e["date"] is None]

    recent = [e for e, d in dated if d >= cutoff]
    old = [e for e, d in dated if d < cutoff]

    # Floor protection: only when recent is empty to prevent completely empty body.
    if not recent and old:
        to_keep = min(floor, len(old))
        promoted = old[-to_keep:]
        old = old[:-to_keep] if to_keep < len(old) else []
        recent = promoted

    if not old:
        return 0

    by_year: dict[int, list] = {}
    for e in old:
        by_year.setdefault(e["date"].year, []).append(e)

    archive_dir = path.parent / "history"

    if dry_run:
        print(f"Would archive {len(old)} entries to {archive_dir}/YYYY.md")
        for y, es in sorted(by_year.items()):
            print(f"  {archive_dir}/{y}.md: {len(es)} entries")
        print(f"Would keep {len(recent)} dated + {len(undated)} undated in body")
        print(f"Would generate/update {archive_dir}/index.md")
        return 0

    archive_dir.mkdir(exist_ok=True)

    for y, es in sorted(by_year.items()):
        archive_path = archive_dir / f"{y}.md"
        if archive_path.exists():
            existing = archive_path.read_text(encoding="utf-8")
            new_es = [e for e in es if _entry_text(e) not in existing]
            if not new_es:
                continue
            text = (
                existing.rstrip("\n")
                + "\n\n"
                + "\n\n".join(_entry_text(e) for e in new_es)
                + "\n"
            )
        else:
            text = (
                f"# History {y}\n\n"
                + "\n\n".join(_entry_text(e) for e in es)
                + "\n"
            )
        archive_path.write_text(text, encoding="utf-8")

    archived_lines = ["## Archived"]
    for y in sorted(by_year.keys()):
        archived_lines.append(f"- [{y}](history/{y}.md) — {len(by_year[y])} entries")

    body_entries = recent + undated
    body_parts = []
    if preamble.strip():
        body_parts.append(preamble.strip())
    body_parts.append("\n".join(archived_lines))
    body_parts.append("\n".join(_entry_text(e) for e in body_entries))

    new_body = "\n\n".join(p for p in body_parts if p.strip()) + "\n"

    if new_body != content:
        path.write_text(new_body, encoding="utf-8")

    _write_index(archive_dir / "index.md", recent + old + undated, by_year, archive_dir)

    return 0


def _write_index(
    index_path: Path,
    entries: list[dict],
    archived_years: dict,
    archive_dir: Path,
):
    """Write history/index.md with year-grouped entry list (newest year first)."""
    by_year: dict[int, list] = {}
    undated = []
    for e in entries:
        if e["date"] is not None:
            by_year.setdefault(e["date"].year, []).append(e)
        else:
            undated.append(e)

    # Include any existing archive files not already in by_year
    for archive_file in sorted(archive_dir.glob("[0-9]*.md")):
        try:
            y = int(archive_file.stem)
        except ValueError:
            continue
        if y in by_year:
            continue
        arch_entries, _ = _parse_entries(archive_file.read_text(encoding="utf-8"))
        for e in arch_entries:
            if e["date"] is not None:
                by_year.setdefault(e["date"].year, []).append(e)

    lines = ["# History Index", ""]

    for y in sorted(by_year.keys(), reverse=True):
        lines.append(f"## {y}")
        year_entries = sorted(by_year[y], key=lambda e: e["date"], reverse=True)
        for e in year_entries:
            anchor = _make_anchor(e["header"])
            link = (
                f"history/{y}.md#{anchor}"
                if y in archived_years
                else f"../history.md#{anchor}"
            )
            subj = re.sub(r"^### #?\d*:?\s*", "", e["header"])
            subj = re.sub(r"\s*\(.*", "", subj).strip()
            summary = _first_sentence(e["body_lines"])
            if summary:
                lines.append(f"- **{e['date'].isoformat()}** [{subj}]({link}) — {summary}")
            else:
                lines.append(f"- **{e['date'].isoformat()}** [{subj}]({link})")
        lines.append("")

    if undated:
        lines.append("## (undated)")
        for e in undated:
            anchor = _make_anchor(e["header"])
            subj = re.sub(r"^### ", "", e["header"]).strip()
            lines.append(f"- [{subj}](../history.md#{anchor})")
        lines.append("")

    index_path.write_text("\n".join(lines), encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description="Rotate history.md entries to archive files")
    parser.add_argument("path", help="Path to history.md")
    parser.add_argument("--threshold-warn", type=int, default=WARN_LINES)
    parser.add_argument("--threshold-hard", type=int, default=HARD_LINES)
    parser.add_argument("--floor", type=int, default=DEFAULT_FLOOR)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    sys.exit(rotate(Path(args.path), floor=args.floor, dry_run=args.dry_run))


if __name__ == "__main__":
    main()
