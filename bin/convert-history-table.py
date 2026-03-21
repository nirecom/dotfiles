#!/usr/bin/env python3
"""Convert history.md table format to section format.

Usage:
    python bin/convert-history-table.py > docs/history.md.new
    diff docs/history.md docs/history.md.new
    mv docs/history.md.new docs/history.md
"""

import re
import sys
from pathlib import Path


def parse_table_rows(lines):
    """Parse markdown table lines into list of dicts."""
    rows = []
    headers = None
    for line in lines:
        line = line.strip()
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.split("|")[1:-1]]
        if headers is None:
            headers = cells
            continue
        if all(re.match(r"^:?-+:?$", c) for c in cells):
            continue
        rows.append(dict(zip(headers, cells)))
    return rows


def format_change_section(row):
    phase = row.get("Phase", "").strip()
    request = row.get("User Request", "").strip()
    impl = row.get("Implementation", "").strip()
    commits = row.get("Key Commits", "").strip()
    return (
        f"### {phase} ({commits})\n"
        f"Background: {request}\n"
        f"Changes: {impl}"
    )


def format_incident_section(row):
    num = row.get("#", "").strip()
    incident = row.get("Incident", "").strip()
    cause = row.get("Cause", "").strip()
    fix = row.get("Fix", "").strip()
    commit = row.get("Commit", "").strip()
    return (
        f"### #{num}: {incident} ({commit})\n"
        f"Cause: {cause}\n"
        f"Fix: {fix}"
    )


def main():
    history_path = Path(__file__).resolve().parent.parent / "docs" / "history.md"
    if not history_path.exists():
        print(f"Error: {history_path} not found", file=sys.stderr)
        sys.exit(1)

    content = history_path.read_text(encoding="utf-8")
    sections = re.split(r"(^## .+$)", content, flags=re.MULTILINE)

    output_parts = []
    i = 0
    while i < len(sections):
        part = sections[i]

        if part.strip() == "## Change History":
            output_parts.append("## Change History\n")
            i += 1
            if i < len(sections):
                table_lines = [l for l in sections[i].strip().split("\n") if l.strip() != "---"]
                for row in parse_table_rows(table_lines):
                    output_parts.append(format_change_section(row))
                    output_parts.append("")

        elif part.strip() == "## Incident History":
            output_parts.append("---\n")
            output_parts.append("## Incident History\n")
            i += 1
            if i < len(sections):
                table_lines = sections[i].strip().split("\n")
                for row in parse_table_rows(table_lines):
                    output_parts.append(format_incident_section(row))
                    output_parts.append("")

        elif part.startswith("# "):
            output_parts.append(part.strip())
            output_parts.append("")

        i += 1

    sys.stdout = open(sys.stdout.fileno(), "w", encoding="utf-8", closefd=False)
    print("\n".join(output_parts).rstrip() + "\n")


if __name__ == "__main__":
    main()
