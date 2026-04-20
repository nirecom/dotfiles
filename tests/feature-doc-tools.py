"""
Integration tests for bin/doc-append.py and bin/doc-rotate.py.
These tests verify only the CLI contract; the tools are not yet implemented.
All tests are expected to FAIL with FileNotFoundError (or similar) until
the tools are created.
"""

import os
import subprocess
import sys
from pathlib import Path

import pytest

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

DOTFILES = Path("c:/git/dotfiles")

DOC_APPEND = ["uv", "run", "bin/doc-append.py"]
DOC_ROTATE = ["uv", "run", "bin/doc-rotate.py"]

_TOOL_NOT_FOUND_PHRASES = (
    "Failed to spawn",
    "cannot find the file",
    "No such file or directory",
)


def _tool_missing(result) -> bool:
    """Return True if the subprocess failed because the script doesn't exist."""
    combined = result.stdout + result.stderr
    return any(phrase in combined for phrase in _TOOL_NOT_FOUND_PHRASES)


def assert_tool_error(result):
    """Assert the tool exited non-zero AND the error is NOT 'tool missing'."""
    assert not _tool_missing(result), (
        "Tool binary not found — implement the tool first. "
        f"stderr: {result.stderr!r}"
    )
    assert result.returncode != 0, f"Expected nonzero exit; got 0. stderr: {result.stderr!r}"


def run(*args, cwd=DOTFILES, check=False, input_text=None):
    """Run a command and return CompletedProcess."""
    return subprocess.run(
        list(args),
        cwd=str(cwd),
        capture_output=True,
        text=True,
        input=input_text,
    )


def run_append(path, *extra_args, **kwargs):
    return run(*DOC_APPEND, str(path), *extra_args, **kwargs)


def run_rotate(path, *extra_args, **kwargs):
    return run(*DOC_ROTATE, str(path), *extra_args, **kwargs)


# ---------------------------------------------------------------------------
# Fixtures / shared builders
# ---------------------------------------------------------------------------


def make_history(tmp_path, content: str, encoding="utf-8", bom=False, crlf=False):
    """Write a history.md file and return its Path."""
    p = tmp_path / "history.md"
    if crlf:
        content = content.replace("\n", "\r\n")
    raw = content.encode(encoding)
    if bom:
        raw = b"\xef\xbb\xbf" + raw
    p.write_bytes(raw)
    return p


SAMPLE_HISTORY = """\
### First entry (2025-01-01, abc1234)
Background: initial
Changes: created

### Second entry (2025-06-01, def5678)
Background: update
Changes: revised
"""

SAMPLE_WITH_INCIDENTS = """\
### First entry (2025-01-01, abc1234)
Background: initial
Changes: created

### #1: First incident (2025-03-01, aaa0001)
Cause: bug
Fix: patched

### #2: Second incident (2025-05-01, bbb0002)
Cause: another bug
Fix: fixed
"""

APPEND_BASE_ARGS = dict(
    subject="New entry",
    date="2025-12-01",
    commits="fff9999",
    background="some background",
    changes="some changes",
)


def make_big_history(tmp_path, n_entries=125):
    """Generate a history.md with n_entries 5-line blocks (### + 3 + blank).

    Default n_entries=125 → 125 * 4 = 500 lines (4 lines per entry; the trailing
    blank serves as the inter-entry separator). Adjust n_entries to target
    different line counts.
    """
    lines = []
    for i in range(n_entries):
        day = (i % 28) + 1
        month = ((i // 28) % 12) + 1
        lines.append(
            f"### FEATURE: Entry {i:03d} (2024-{month:02d}-{day:02d}, abc{i:04d})"
        )
        lines.append(f"Background: background text for entry {i}")
        lines.append(f"Changes: changes for entry {i}")
        lines.append("")
    content = "\n".join(lines)
    p = tmp_path / "history.md"
    p.write_text(content, encoding="utf-8")
    return p


def append_args(
    path,
    subject=APPEND_BASE_ARGS["subject"],
    date=APPEND_BASE_ARGS["date"],
    commits=APPEND_BASE_ARGS["commits"],
    background=None,
    changes=None,
    cause=None,
    fix=None,
    incident=False,
):
    args = [
        "--subject", subject,
        "--date", date,
        "--commits", commits,
    ]
    if incident:
        args += ["--category", "INCIDENT"]
        args += ["--cause", cause or "some cause"]
        args += ["--fix", fix or "some fix"]
    else:
        args += ["--category", "FEATURE"]
        args += ["--background", background or "some background"]
        args += ["--changes", changes or "some changes"]
    return run_append(path, *args)


# ===========================================================================
# doc-append tests
# ===========================================================================


class TestDocAppendNormal:
    def test_n1_standard_entry_appended(self, tmp_path):
        """N1: Append standard entry to existing history.md → entry at end."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### FEATURE: New entry (2025-12-01, fff9999)" in content
        assert "Background: some background" in content
        assert "Changes: some changes" in content
        # New entry must be after existing entries
        pos_second = content.index("### Second entry")
        pos_new = content.index("### FEATURE: New entry")
        assert pos_new > pos_second

    def test_n2_incident_numbered_after_last(self, tmp_path):
        """N2: Append incident to file with #2: → new entry is #3:."""
        p = make_history(tmp_path, SAMPLE_WITH_INCIDENTS)
        result = run_append(
            p,
            "--category", "INCIDENT",
            "--subject", "Third incident",
            "--date", "2025-12-01",
            "--commits", "ccc0003",
            "--cause", "yet another bug",
            "--fix", "fixed again",
        )
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### INCIDENT: #3: Third incident (2025-12-01, ccc0003)" in content

    def test_n3_incident_from_tail_scan(self, tmp_path):
        """N3: #3: exists in tail → new incident is #4:."""
        base = SAMPLE_WITH_INCIDENTS + """\

### #3: Third incident (2025-07-01, ccc0003)
Cause: bug3
Fix: fix3
"""
        p = make_history(tmp_path, base)
        result = run_append(
            p,
            "--category", "INCIDENT",
            "--subject", "Fourth incident",
            "--date", "2025-12-01",
            "--commits", "ddd0004",
            "--cause", "bug4",
            "--fix", "fix4",
        )
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### INCIDENT: #4: Fourth incident (2025-12-01, ddd0004)" in content


class TestDocAppendError:
    def test_e1_date_before_last_entry(self, tmp_path):
        """E1: New date earlier than last entry date → nonzero exit, file unchanged."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        original = p.read_bytes()
        result = append_args(p, date="2025-01-01")  # earlier than 2025-06-01
        assert_tool_error(result)
        assert p.read_bytes() == original

    def test_e2_invalid_date_format(self, tmp_path):
        """E2: Invalid date format → nonzero exit."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        result = append_args(p, date="not-a-date")
        assert_tool_error(result)

    def test_e3_parent_dir_missing(self, tmp_path):
        """E3: Parent dir missing → nonzero exit."""
        p = tmp_path / "nonexistent" / "history.md"
        result = append_args(p)
        assert_tool_error(result)

    def test_e4_subject_missing(self, tmp_path):
        """E4: --subject missing → nonzero exit."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        result = run_append(
            p,
            "--date", "2025-12-01",
            "--commits", "abc1234",
            "--background", "bg",
            "--changes", "ch",
        )
        assert_tool_error(result)


class TestDocAppendEdge:
    def test_g1_empty_file(self, tmp_path):
        """G1: Empty file → appends correctly."""
        p = tmp_path / "history.md"
        p.write_text("")
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### FEATURE: New entry (2025-12-01, fff9999)" in content
        assert "Background: some background" in content

    def test_g2_no_trailing_newline(self, tmp_path):
        """G2: No trailing newline → newline added before entry."""
        content_no_nl = SAMPLE_HISTORY.rstrip("\n")
        p = make_history(tmp_path, content_no_nl)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        raw = p.read_text(encoding="utf-8")
        # New entry should be on its own line, not concatenated to previous
        assert "\n### FEATURE: New entry" in raw

    def test_g3_crlf_file(self, tmp_path):
        """G3: CRLF file → appended entry uses CRLF."""
        p = make_history(tmp_path, SAMPLE_HISTORY, crlf=True)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        raw = p.read_bytes()
        # The appended section should also use CRLF
        new_entry_header = b"### FEATURE: New entry"
        idx = raw.index(new_entry_header)
        # Scan for \r\n in the new section
        new_section = raw[idx:]
        assert b"\r\n" in new_section

    def test_g4_large_file_no_entry_in_tail(self, tmp_path):
        """G4: Last 4096 bytes have no ### → 16KB expansion, still appends correctly."""
        # Build a file whose first part has ### entries, but the last 4096 bytes
        # consist only of bulk text (no ### headers).
        header_section = SAMPLE_HISTORY
        # Add a big trailing comment block > 4096 bytes with no ### markers
        filler = ("# comment line without entry header\n") * 130  # ~5200 bytes
        content = header_section + "\n" + filler
        p = make_history(tmp_path, content)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        text = p.read_text(encoding="utf-8")
        assert "### FEATURE: New entry (2025-12-01, fff9999)" in text

    def test_g5_first_incident_no_prior_numbers(self, tmp_path):
        """G5: First incident (no #N: anywhere) → #1:, stderr warning."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        result = run_append(
            p,
            "--category", "INCIDENT",
            "--subject", "First incident",
            "--date", "2025-12-01",
            "--commits", "zzz0001",
            "--cause", "cause",
            "--fix", "fix",
        )
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### INCIDENT: #1: First incident (2025-12-01, zzz0001)" in content
        # Warning should appear on stderr
        assert result.stderr.strip() != ""


class TestDocAppendIdempotency:
    def test_i1_two_consecutive_appends(self, tmp_path):
        """I1: Two consecutive appends with different dates → both present, no corruption."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        r1 = append_args(p, subject="Entry A", date="2025-12-01", commits="aaa0001")
        assert r1.returncode == 0, r1.stderr
        r2 = append_args(p, subject="Entry B", date="2025-12-02", commits="bbb0002")
        assert r2.returncode == 0, r2.stderr
        content = p.read_text(encoding="utf-8")
        assert "### FEATURE: Entry A (2025-12-01, aaa0001)" in content
        assert "### FEATURE: Entry B (2025-12-02, bbb0002)" in content
        # Entry A must appear before Entry B
        assert content.index("### FEATURE: Entry A") < content.index("### FEATURE: Entry B")

    def test_i2_multiple_trailing_blanks(self, tmp_path):
        """I2: File with multiple trailing blank lines → no extra blank gap."""
        content_extra = SAMPLE_HISTORY + "\n\n\n"
        p = make_history(tmp_path, content_extra)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        text = p.read_text(encoding="utf-8")
        # Should not have 3+ consecutive blank lines before new entry
        assert "\n\n\n\n### New entry" not in text


class TestDocAppendAutoRotate:
    def test_n1_auto_rotate_triggers_over_threshold(self, tmp_path):
        """N1: append that pushes file >= 500 lines auto-rotates archive + index."""
        p = make_big_history(tmp_path, n_entries=100)
        # Sanity check on initial size: 100 entries * 4 lines = 400 lines
        # (no trailing newline after final "" join element).
        initial_lines = p.read_text(encoding="utf-8").count("\n") + 1
        assert 390 <= initial_lines <= 510, (
            f"Expected initial line count in [390, 510]; got {initial_lines}"
        )

        # Bump to 125 entries (~500 lines) so that the appended entry pushes over
        # the 500-line threshold. We use n_entries=125 so the post-append file
        # is comfortably over 500 lines.
        p = make_big_history(tmp_path, n_entries=125)
        pre_append_lines = p.read_text(encoding="utf-8").count("\n") + 1
        assert pre_append_lines >= 490, (
            f"Pre-append file must be near/over threshold; got {pre_append_lines}"
        )

        result = append_args(
            p,
            subject="Trigger entry",
            date="2026-04-20",
            commits="fff9999",
        )
        assert result.returncode == 0, result.stderr

        archive_dir = tmp_path / "history"
        assert (archive_dir / "2024.md").exists(), (
            "Archive file history/2024.md must be created by auto-rotation. "
            f"stdout: {result.stdout!r} stderr: {result.stderr!r}"
        )
        assert (archive_dir / "index.md").exists(), (
            "history/index.md must be rebuilt by auto-rotation. "
            f"stdout: {result.stdout!r} stderr: {result.stderr!r}"
        )

        body = p.read_text(encoding="utf-8")
        body_lines = body.count("\n") + 1
        assert body_lines < 200, (
            f"Resulting history body should be much smaller after rotation; "
            f"got {body_lines} lines"
        )
        # Newly-appended entry should be among the kept floor=20 most recent
        assert "Trigger entry" in body, (
            "Newly-appended Trigger entry must remain in body (within floor=20)"
        )

    def test_n2_auto_rotate_silent_under_threshold(self, tmp_path):
        """N2: small file under threshold → no archive dir created."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        result = append_args(
            p,
            subject="Small append",
            date="2026-04-20",
            commits="aaa1111",
        )
        assert result.returncode == 0, result.stderr
        assert not (tmp_path / "history").exists(), (
            "No rotation should occur for a small file; history/ dir must not exist"
        )

    def test_g1_auto_rotate_keeps_appended_entry_in_body(self, tmp_path):
        """G1: appended Trigger entry stays in body and is the most recent."""
        p = make_big_history(tmp_path, n_entries=125)
        result = append_args(
            p,
            subject="Trigger entry",
            date="2026-04-20",
            commits="fff9999",
        )
        assert result.returncode == 0, result.stderr

        # Rotation must have triggered (precondition for this G1 check)
        archive_dir = tmp_path / "history"
        assert (archive_dir / "2024.md").exists(), (
            "Auto-rotation precondition: history/2024.md must exist"
        )

        body = p.read_text(encoding="utf-8")
        assert "Trigger entry" in body, "Appended entry must remain in body"

        # Trigger entry should be the most recent — appear after all kept old
        # entries. Find the position of the Trigger entry header and confirm
        # that no later "### " header exists after it (excluding the
        # ## Archived block, which uses ## not ###).
        trigger_pos = body.index("Trigger entry")
        # All `### ` headers in body
        header_positions = []
        idx = 0
        while True:
            i = body.find("### ", idx)
            if i == -1:
                break
            header_positions.append(i)
            idx = i + 1
        assert header_positions, "Body must contain at least one ### header"
        # Find the header position that corresponds to the Trigger entry
        # (the ### header immediately preceding "Trigger entry").
        trigger_header_pos = max(h for h in header_positions if h <= trigger_pos)
        # No ### header may appear after the Trigger entry's header
        later_headers = [h for h in header_positions if h > trigger_header_pos]
        assert not later_headers, (
            f"Trigger entry must be the LAST ### entry in body, but found "
            f"{len(later_headers)} later headers at positions {later_headers}"
        )


# ===========================================================================
# doc-rotate tests
# ===========================================================================

# Build a history with a 2-year span: entries from 2024 and 2025+
OLD_ENTRIES = """\
### BUGFIX: Old entry one (2024-01-10, aaa0001)
Background: old background one
Changes: old changes one

### FEATURE: Old entry two (2024-06-15, aaa0002)
Background: old background two
Changes: old changes two

"""

RECENT_ENTRIES = """\
### FEATURE: Recent entry one (2026-01-01, bbb0001)
Background: recent background one
Changes: recent changes one

### BUGFIX: Recent entry two (2026-03-01, bbb0002)
Background: recent background two
Changes: recent changes two

"""

TWO_YEAR_HISTORY = OLD_ENTRIES + RECENT_ENTRIES


class TestDocRotateNormal:
    def test_n1_old_archived_recent_kept(self, tmp_path):
        """N1: floor=2 keeps 2 most recent, archives 2 oldest."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        result = run_rotate(p, "--floor", "2")
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        # Recent entries must remain in body
        assert "### FEATURE: Recent entry one" in body
        assert "### BUGFIX: Recent entry two" in body
        # Old entries must NOT remain in body (archived)
        assert "### BUGFIX: Old entry one" not in body
        assert "### FEATURE: Old entry two" not in body
        # Archive file for 2024 must exist
        archive_dir = tmp_path / "history"
        assert (archive_dir / "2024.md").exists()
        archive_content = (archive_dir / "2024.md").read_text(encoding="utf-8")
        assert "Old entry one" in archive_content
        assert "Old entry two" in archive_content
        # index.md must exist
        assert (archive_dir / "index.md").exists()
        # Body should have Archived section
        assert "## Archived" in body

    def test_n2_dry_run_no_writes(self, tmp_path):
        """N2: --dry-run → no files written, stdout shows plan."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        original = p.read_bytes()
        result = run_rotate(p, "--floor", "2", "--dry-run")
        assert result.returncode == 0, result.stderr
        assert p.read_bytes() == original  # file unchanged
        archive_dir = tmp_path / "history"
        assert not archive_dir.exists()  # no archive dir created
        assert result.stdout.strip() != ""  # plan printed to stdout

    def test_n3_idempotent_on_already_rotated(self, tmp_path):
        """N3: Re-run on already-rotated file → no change (idempotent)."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        r1 = run_rotate(p, "--floor", "2")
        assert r1.returncode == 0, r1.stderr
        body_after_first = p.read_text(encoding="utf-8")
        archive_dir = tmp_path / "history"
        archive_2024_after_first = (archive_dir / "2024.md").read_bytes()

        r2 = run_rotate(p, "--floor", "2")
        assert r2.returncode == 0, r2.stderr
        body_after_second = p.read_text(encoding="utf-8")
        archive_2024_after_second = (archive_dir / "2024.md").read_bytes()

        assert body_after_first == body_after_second
        assert archive_2024_after_first == archive_2024_after_second

    def test_n4_index_shows_category_badge(self, tmp_path):
        """N4: index.md shows ## Category Distribution and per-entry badges."""
        import re

        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        result = run_rotate(p, "--floor", "2")
        assert result.returncode == 0, result.stderr
        archive_dir = tmp_path / "history"
        index_path = archive_dir / "index.md"
        assert index_path.exists(), "index.md must exist after rotation"
        index_text = index_path.read_text(encoding="utf-8")
        # Distribution section header present
        assert "## Category Distribution" in index_text, (
            f"Category Distribution section not found in index. Content: {index_text!r}"
        )
        # Distribution line contains a category and a count
        assert re.search(r"(BUGFIX|FEATURE):\s*\d+", index_text), (
            f"Category count line not found in index. Content: {index_text!r}"
        )
        # At least one entry line has a category prefix (colon style)
        assert re.search(r"^- \d{4}-\d{2}-\d{2}: (BUGFIX|FEATURE):", index_text, re.MULTILINE), (
            f"No category prefix found on entry lines. Content: {index_text!r}"
        )

    def test_n5_rebuild_index_only(self, tmp_path):
        """N5: --rebuild-index rebuilds index.md without touching history.md body."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        r1 = run_rotate(p, "--floor", "2")
        assert r1.returncode == 0, r1.stderr
        body_after_rotate = p.read_bytes()
        archive_dir = tmp_path / "history"
        index_path = archive_dir / "index.md"
        # Stomp the index.md with garbage
        index_path.write_text("STALE\n", encoding="utf-8")
        # Rebuild
        r2 = run_rotate(p, "--rebuild-index")
        assert r2.returncode == 0, r2.stderr
        new_index = index_path.read_text(encoding="utf-8")
        assert "STALE" not in new_index, "Stale content must be replaced"
        assert "# History Index" in new_index, (
            f"Rebuilt index missing # History Index header. Content: {new_index!r}"
        )
        # history.md body must be byte-identical
        assert p.read_bytes() == body_after_rotate, (
            "history.md body must NOT be touched by --rebuild-index"
        )

    def test_n6_index_strips_category_from_link_text(self, tmp_path):
        """N6: index link text shows subject only, with CATEGORY: prefix stripped."""
        content = """\
### FEATURE: Some subject (2026-01-01, abc1234)
Background: bg
Changes: ch

### FEATURE: Another (2024-01-01, def5678)
Background: bg2
Changes: ch2

"""
        p = make_history(tmp_path, content)
        result = run_rotate(p, "--floor", "1")
        assert result.returncode == 0, result.stderr
        archive_dir = tmp_path / "history"
        index_path = archive_dir / "index.md"
        assert index_path.exists()
        index_text = index_path.read_text(encoding="utf-8")
        # Find the bullet line for "Another" (the archived entry)
        bullet_line = None
        for line in index_text.splitlines():
            if "Another" in line and line.lstrip().startswith("-"):
                bullet_line = line
                break
        assert bullet_line is not None, (
            f"No bullet line found for archived entry. Index: {index_text!r}"
        )
        # The bracketed link text should contain "Another" but NOT "FEATURE:"
        import re
        m = re.search(r"\[([^\]]+)\]", bullet_line)
        assert m is not None, f"No [link text] found in bullet: {bullet_line!r}"
        link_text = m.group(1)
        assert "Another" in link_text, f"Subject missing from link text: {link_text!r}"
        assert "FEATURE:" not in link_text, (
            f"FEATURE: prefix not stripped from link text: {link_text!r}"
        )

    def test_n7_index_handles_incident_format(self, tmp_path):
        """N7: INCIDENT entries get badge; link strips INCIDENT: and #N: prefixes."""
        content = """\
### INCIDENT: #42: Crash on startup (2024-02-01, abc1234)
Cause: bad init
Fix: reorder init

### FEATURE: Newer (2026-01-01, def5678)
Background: bg
Changes: ch

"""
        p = make_history(tmp_path, content)
        result = run_rotate(p, "--floor", "1")
        assert result.returncode == 0, result.stderr
        archive_dir = tmp_path / "history"
        index_path = archive_dir / "index.md"
        assert index_path.exists()
        index_text = index_path.read_text(encoding="utf-8")
        # Find the bullet line for the incident
        bullet_line = None
        for line in index_text.splitlines():
            if "Crash on startup" in line and line.lstrip().startswith("-"):
                bullet_line = line
                break
        assert bullet_line is not None, (
            f"No bullet line found for incident. Index: {index_text!r}"
        )
        assert "INCIDENT:" in bullet_line, (
            f"INCIDENT: prefix missing from bullet: {bullet_line!r}"
        )
        import re
        m = re.search(r"\[([^\]]+)\]", bullet_line)
        assert m is not None, f"No [link text] found in bullet: {bullet_line!r}"
        link_text = m.group(1)
        assert "Crash on startup" in link_text, (
            f"Subject missing from link text: {link_text!r}"
        )
        assert "INCIDENT:" not in link_text, (
            f"INCIDENT: prefix not stripped: {link_text!r}"
        )
        assert "#42:" not in link_text, (
            f"#42: prefix not stripped: {link_text!r}"
        )

    def test_n8_index_uncategorized_entries_no_badge(self, tmp_path):
        """N8: bare/uncategorized entries get no badge; not counted in distribution."""
        content = """\
### FEATURE: Categorized one (2024-01-01, abc1234)
Background: bg
Changes: ch

### Plain entry (2024-02-01, def5678)
Background: bg2
Changes: ch2

### FEATURE: Recent kept (2026-01-01, eee0001)
Background: bg3
Changes: ch3

"""
        p = make_history(tmp_path, content)
        result = run_rotate(p, "--floor", "1")
        assert result.returncode == 0, result.stderr
        archive_dir = tmp_path / "history"
        index_path = archive_dir / "index.md"
        assert index_path.exists()
        index_text = index_path.read_text(encoding="utf-8")
        # Find bare entry's bullet
        bare_line = None
        for line in index_text.splitlines():
            if "Plain entry" in line and line.lstrip().startswith("-"):
                bare_line = line
                break
        assert bare_line is not None, (
            f"No bullet line found for bare entry. Index: {index_text!r}"
        )
        # Make sure bare entry has no category prefix between date and link
        import re
        # Format for uncategorized: "- YYYY-MM-DD: [Subject](...)" (no WORD: between date and link)
        before_link = bare_line.split("[", 1)[0]
        assert not re.search(r"[A-Z]{4,9}:", before_link.split(":", 2)[-1].strip()), (
            f"Bare entry should have no category prefix, but found one in: {bare_line!r}"
        )
        # Distribution: FEATURE: 1 (only the archived FEATURE entry counts;
        # the recent FEATURE is kept in body and not in archive index).
        # We require FEATURE count is present and "uncategorized" is not used.
        assert re.search(r"FEATURE:\s*\d+", index_text), (
            f"FEATURE count missing from distribution: {index_text!r}"
        )
        assert "uncategorized" not in index_text.lower(), (
            f"Distribution must not include 'uncategorized': {index_text!r}"
        )

    def test_n9_rebuild_index_idempotent(self, tmp_path):
        """N9: --rebuild-index produces identical bytes when run twice."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        r1 = run_rotate(p, "--floor", "2")
        assert r1.returncode == 0, r1.stderr
        archive_dir = tmp_path / "history"
        index_path = archive_dir / "index.md"

        r2 = run_rotate(p, "--rebuild-index")
        assert r2.returncode == 0, r2.stderr
        first = index_path.read_bytes()

        r3 = run_rotate(p, "--rebuild-index")
        assert r3.returncode == 0, r3.stderr
        second = index_path.read_bytes()

        assert first == second, "Rebuild must be idempotent (byte-identical output)"


class TestDocRotateError:
    def test_e1_file_not_found(self, tmp_path):
        """E1: File not found → nonzero exit (tool must exist and reject missing input)."""
        p = tmp_path / "does_not_exist.md"
        result = run_rotate(p)
        # Must fail, and the failure must come from the tool logic (not tool missing)
        assert_tool_error(result)

    def test_e2_malformed_date_treated_as_undated(self, tmp_path):
        """E2: Entry with malformed date → treated as undated, kept in body."""
        content = """\
### Good entry (2026-01-01, abc1234)
Background: good
Changes: good

### Bad date entry (2024-99-99, def5678)
Background: bad date
Changes: bad date

"""
        p = make_history(tmp_path, content)
        result = run_rotate(p)
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        # Undated/malformed stays in body
        assert "### Bad date entry" in body


class TestDocRotateEdge:
    def test_g1_all_entries_fit_in_floor(self, tmp_path):
        """G1: All entries fit within floor (2 entries < floor=20) → no rotation."""
        content = """\
### Entry A (2026-01-01, aaa0001)
Background: a
Changes: a

### Entry B (2026-03-01, bbb0002)
Background: b
Changes: b

"""
        p = make_history(tmp_path, content)
        original = p.read_bytes()
        result = run_rotate(p)
        assert result.returncode == 0, result.stderr
        assert p.read_bytes() == original
        assert not (tmp_path / "history").exists()

    def test_g2_all_old_floor_keeps_20(self, tmp_path):
        """G2: All entries older than 365d → floor keeps 20 most recent in body."""
        # 25 entries all from 2023
        entries = ""
        for i in range(1, 26):
            day = str(i).zfill(2)
            entries += f"""\
### Entry {i:02d} (2023-01-{day}, abc{i:04d})
Background: background {i}
Changes: changes {i}

"""
        p = make_history(tmp_path, entries)
        result = run_rotate(p)
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        # Count ### entries remaining in body (exclude ## Archived header)
        kept = [line for line in body.splitlines() if line.startswith("### ")]
        assert len(kept) >= 20
        # The most recent 20 (entries 06..25) should be present
        assert "### Entry 25" in body

    def test_g3_single_entry_kept(self, tmp_path):
        """G3: Single entry → floor keeps it in body."""
        content = """\
### Only entry (2023-01-01, abc1234)
Background: only
Changes: only

"""
        p = make_history(tmp_path, content)
        result = run_rotate(p)
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        assert "### Only entry" in body

    def test_g4_ai_specs_path_rotates_correctly(self, tmp_path):
        """G4: Path contains 'ai-specs' → rotation works with English headers."""
        ai_specs_dir = tmp_path / "ai-specs" / "project"
        ai_specs_dir.mkdir(parents=True)
        p = ai_specs_dir / "history.md"
        p.write_text(TWO_YEAR_HISTORY, encoding="utf-8")
        result = run_rotate(p, "--floor", "2")
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        assert "## Archived" in body
        assert "### BUGFIX: Old entry one" not in body
        assert "### FEATURE: Recent entry one" in body


class TestDocRotateIdempotency:
    def test_i1_rotate_twice_same_result(self, tmp_path):
        """I1: rotate twice → same result as once."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        r1 = run_rotate(p, "--floor", "2")
        assert r1.returncode == 0, r1.stderr
        snapshot_body = p.read_text(encoding="utf-8")
        archive_dir = tmp_path / "history"
        snapshot_archive = {
            f.name: f.read_bytes() for f in archive_dir.iterdir() if f.is_file()
        }

        r2 = run_rotate(p, "--floor", "2")
        assert r2.returncode == 0, r2.stderr
        assert p.read_text(encoding="utf-8") == snapshot_body
        for name, data in snapshot_archive.items():
            assert (archive_dir / name).read_bytes() == data


# ===========================================================================
# doc-rotate: line-count threshold and --max-age-days tests
# ===========================================================================


def make_long_history(tmp_path, n_entries: int, days_old: int) -> Path:
    """Generate history.md with n_entries all dated days_old days ago."""
    from datetime import date, timedelta

    entry_date = (date.today() - timedelta(days=days_old)).isoformat()
    lines = []
    for i in range(n_entries):
        lines.append(f"### Entry {i} ({entry_date}, abc{i:04d})")
        lines.append("Background: filler")
        lines.append("Changes: filler")
        lines.append("")
    p = tmp_path / "history.md"
    p.write_text("\n".join(lines), encoding="utf-8")
    return p


UNDATED_HISTORY = """\
### Old feature (abc0001)
Background: pre-dating convention
Changes: initial work

### Another old feature (abc0002)
Background: also undated
Changes: more old work

"""


class TestDocRotateThreshold:
    def test_t1_under_threshold_no_rotation(self, tmp_path):
        """T-THRESH-1: file under threshold-warn → no rotation."""
        # 5 entries = small file, well under default warn=500
        p = make_long_history(tmp_path, n_entries=5, days_old=400)
        original = p.read_bytes()
        result = run_rotate(p, "--threshold-warn", "500")
        assert result.returncode == 0, result.stderr
        assert p.read_bytes() == original, "File must not change when under threshold"
        archive_dir = tmp_path / "history"
        assert not archive_dir.exists(), "No archive dir must be created when under threshold"

    def test_t2_over_threshold_triggers_rotation(self, tmp_path):
        """T-THRESH-2: file over threshold-warn → old entries archived."""
        # 20 entries, threshold-warn=10, floor=5 → 15 entries archived
        p = make_long_history(tmp_path, n_entries=20, days_old=400)
        result = run_rotate(p, "--threshold-warn", "10", "--floor", "5")
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        assert "## Archived" in body
        archive_dir = tmp_path / "history"
        assert archive_dir.exists(), "Archive dir must be created when over threshold"

    def test_t3_floor_controls_what_is_kept(self, tmp_path):
        """T-THRESH-3: --floor keeps N most recent entries, archives the rest."""
        from datetime import date, timedelta

        today = date.today()
        entries = ""
        for i, days in enumerate([400, 300, 200]):
            d = (today - timedelta(days=days)).isoformat()
            entries += f"### Entry {i} ({d}, abc{i:04d})\nBackground: b\nChanges: c\n\n"
        p = tmp_path / "history.md"
        p.write_text(entries, encoding="utf-8")
        # floor=1 → keep most recent (200d ago), archive 2 older ones
        result = run_rotate(p, "--floor", "1")
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        assert "### Entry 2" in body, "Most recent entry must be kept"
        assert "### Entry 0" not in body, "Oldest entry must be archived"
        assert "### Entry 1" not in body, "Middle entry must be archived"

    def test_t4_undated_entries_archived_when_threshold_exceeded(self, tmp_path):
        """T-THRESH-4: undated entries archived when line-count threshold exceeded."""
        p = tmp_path / "history.md"
        p.write_text(UNDATED_HISTORY, encoding="utf-8")
        # floor=1 → archive first undated entry, keep second
        result = run_rotate(p, "--threshold-warn", "1", "--floor", "1")
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        assert "### Old feature" not in body, "First undated entry must be archived"
        archive_dir = tmp_path / "history"
        assert archive_dir.exists(), "Archive dir must be created for undated entries"
        legacy = archive_dir / "legacy.md"
        assert legacy.exists(), "Undated entries must go to legacy.md"
        assert "Old feature" in legacy.read_text(encoding="utf-8")

    def test_t5_dry_run_reports_threshold_status(self, tmp_path):
        """T-THRESH-5: --dry-run reports whether threshold is exceeded."""
        p = make_long_history(tmp_path, n_entries=20, days_old=400)
        result = run_rotate(p, "--dry-run", "--threshold-warn", "10", "--floor", "5")
        assert result.returncode == 0, result.stderr
        assert result.stdout, "dry-run must print something when threshold exceeded"
        assert "threshold-warn" in result.stdout, "dry-run must report threshold info"
