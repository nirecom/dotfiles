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
        args.append("--incident")
        args += ["--cause", cause or "some cause"]
        args += ["--fix", fix or "some fix"]
    else:
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
        assert "### New entry (2025-12-01, fff9999)" in content
        assert "Background: some background" in content
        assert "Changes: some changes" in content
        # New entry must be after existing entries
        pos_second = content.index("### Second entry")
        pos_new = content.index("### New entry")
        assert pos_new > pos_second

    def test_n2_incident_numbered_after_last(self, tmp_path):
        """N2: Append incident to file with #2: → new entry is #3:."""
        p = make_history(tmp_path, SAMPLE_WITH_INCIDENTS)
        result = run_append(
            p,
            "--incident",
            "--subject", "Third incident",
            "--date", "2025-12-01",
            "--commits", "ccc0003",
            "--cause", "yet another bug",
            "--fix", "fixed again",
        )
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### #3: Third incident (2025-12-01, ccc0003)" in content

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
            "--incident",
            "--subject", "Fourth incident",
            "--date", "2025-12-01",
            "--commits", "ddd0004",
            "--cause", "bug4",
            "--fix", "fix4",
        )
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### #4: Fourth incident (2025-12-01, ddd0004)" in content


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
        assert "### New entry (2025-12-01, fff9999)" in content
        assert "Background: some background" in content

    def test_g2_no_trailing_newline(self, tmp_path):
        """G2: No trailing newline → newline added before entry."""
        content_no_nl = SAMPLE_HISTORY.rstrip("\n")
        p = make_history(tmp_path, content_no_nl)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        raw = p.read_text(encoding="utf-8")
        # New entry should be on its own line, not concatenated to previous
        assert "\n### New entry" in raw

    def test_g3_crlf_file(self, tmp_path):
        """G3: CRLF file → appended entry uses CRLF."""
        p = make_history(tmp_path, SAMPLE_HISTORY, crlf=True)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        raw = p.read_bytes()
        # The appended section should also use CRLF
        new_entry_header = b"### New entry"
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
        assert "### New entry (2025-12-01, fff9999)" in text

    def test_g5_first_incident_no_prior_numbers(self, tmp_path):
        """G5: First incident (no #N: anywhere) → #1:, stderr warning."""
        p = make_history(tmp_path, SAMPLE_HISTORY)
        result = run_append(
            p,
            "--incident",
            "--subject", "First incident",
            "--date", "2025-12-01",
            "--commits", "zzz0001",
            "--cause", "cause",
            "--fix", "fix",
        )
        assert result.returncode == 0, result.stderr
        content = p.read_text(encoding="utf-8")
        assert "### #1: First incident (2025-12-01, zzz0001)" in content
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
        assert "### Entry A (2025-12-01, aaa0001)" in content
        assert "### Entry B (2025-12-02, bbb0002)" in content
        # Entry A must appear before Entry B
        assert content.index("### Entry A") < content.index("### Entry B")

    def test_i2_multiple_trailing_blanks(self, tmp_path):
        """I2: File with multiple trailing blank lines → no extra blank gap."""
        content_extra = SAMPLE_HISTORY + "\n\n\n"
        p = make_history(tmp_path, content_extra)
        result = append_args(p)
        assert result.returncode == 0, result.stderr
        text = p.read_text(encoding="utf-8")
        # Should not have 3+ consecutive blank lines before new entry
        assert "\n\n\n\n### New entry" not in text


# ===========================================================================
# doc-rotate tests
# ===========================================================================

# Build a history with a 2-year span: entries from 2024 and 2025+
OLD_ENTRIES = """\
### Old entry one (2024-01-10, aaa0001)
Background: old background one
Changes: old changes one

### Old entry two (2024-06-15, aaa0002)
Background: old background two
Changes: old changes two

"""

RECENT_ENTRIES = """\
### Recent entry one (2026-01-01, bbb0001)
Background: recent background one
Changes: recent changes one

### Recent entry two (2026-03-01, bbb0002)
Background: recent background two
Changes: recent changes two

"""

TWO_YEAR_HISTORY = OLD_ENTRIES + RECENT_ENTRIES


class TestDocRotateNormal:
    def test_n1_old_archived_recent_kept(self, tmp_path):
        """N1: 2-year span, older > 365d → archived, recent kept."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        result = run_rotate(p)
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        # Recent entries must remain in body
        assert "### Recent entry one" in body
        assert "### Recent entry two" in body
        # Old entries must NOT remain in body (archived)
        assert "### Old entry one" not in body
        assert "### Old entry two" not in body
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
        result = run_rotate(p, "--dry-run")
        assert result.returncode == 0, result.stderr
        assert p.read_bytes() == original  # file unchanged
        archive_dir = tmp_path / "history"
        assert not archive_dir.exists()  # no archive dir created
        assert result.stdout.strip() != ""  # plan printed to stdout

    def test_n3_idempotent_on_already_rotated(self, tmp_path):
        """N3: Re-run on already-rotated file → no change (idempotent)."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        r1 = run_rotate(p)
        assert r1.returncode == 0, r1.stderr
        body_after_first = p.read_text(encoding="utf-8")
        archive_dir = tmp_path / "history"
        archive_2024_after_first = (archive_dir / "2024.md").read_bytes()

        r2 = run_rotate(p)
        assert r2.returncode == 0, r2.stderr
        body_after_second = p.read_text(encoding="utf-8")
        archive_2024_after_second = (archive_dir / "2024.md").read_bytes()

        assert body_after_first == body_after_second
        assert archive_2024_after_first == archive_2024_after_second


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
    def test_g1_all_entries_within_365d(self, tmp_path):
        """G1: All entries within 365d → no rotation, body unchanged."""
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

    def test_g4_ai_specs_path_japanese_headings(self, tmp_path):
        """G4: Path contains 'ai-specs' → Japanese headings."""
        # Create a subdirectory named to include ai-specs in its path
        ai_specs_dir = tmp_path / "ai-specs" / "project"
        ai_specs_dir.mkdir(parents=True)
        p = ai_specs_dir / "history.md"
        p.write_text(TWO_YEAR_HISTORY, encoding="utf-8")
        result = run_rotate(p)
        assert result.returncode == 0, result.stderr
        body = p.read_text(encoding="utf-8")
        assert "## アーカイブ" in body
        # index.md undated section should use Japanese
        index_path = ai_specs_dir / "history" / "index.md"
        if index_path.exists():
            index_text = index_path.read_text(encoding="utf-8")
            assert "## (日付なし)" in index_text or "アーカイブ" in index_text


class TestDocRotateIdempotency:
    def test_i1_rotate_twice_same_result(self, tmp_path):
        """I1: rotate twice → same result as once."""
        p = make_history(tmp_path, TWO_YEAR_HISTORY)
        r1 = run_rotate(p)
        assert r1.returncode == 0, r1.stderr
        snapshot_body = p.read_text(encoding="utf-8")
        archive_dir = tmp_path / "history"
        snapshot_archive = {
            f.name: f.read_bytes() for f in archive_dir.iterdir() if f.is_file()
        }

        r2 = run_rotate(p)
        assert r2.returncode == 0, r2.stderr
        assert p.read_text(encoding="utf-8") == snapshot_body
        for name, data in snapshot_archive.items():
            assert (archive_dir / name).read_bytes() == data
