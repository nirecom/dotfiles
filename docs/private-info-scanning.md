# Private Information Scanning

Automated scanning to prevent private information from being committed to public repositories.

## How It Works

Two checkpoints scan for private information:

| Checkpoint | When | Mechanism |
|:---|:---|:---|
| Git commit | Every `git commit` | Pre-commit hook (`claude-global/hooks/pre-commit`) |
| Claude Code edit | Every Edit/Write tool call | PreToolUse hook (`claude-global/hooks/check-private-info.js`) |
| Claude Code commit | Every `git commit` via Bash tool | PreToolUse hook (`claude-global/hooks/check-private-info.js`) |

Both call `bin/check-private-info.sh` as the scanner (single source of truth for patterns).

**Private repos are skipped**: repos listed in `.context-private/private-repos.txt` are not scanned. Unregistered repos are scanned by default (safe default).

## Detection Patterns

| Type | Pattern | Examples |
|:---|:---|:---|
| RFC 1918 IPv4 | `10.x.x.x`, `172.16-31.x.x`, `192.168.x.x` | `192.168.1.1`, `10.0.0.1` |
| Email addresses | `user@domain.tld` | `user@example.com` |
| MAC addresses | `XX:XX:XX:XX:XX:XX` / `XX-XX-XX-XX-XX-XX` | `aa:bb:cc:dd:ee:ff` |
| Absolute local paths | `/Users/<name>`, `/home/<name>`, `C:\Users\<name>` | `/Users/john/docs` |
| Blocklist patterns | User-defined in `.private-info-blocklist` | Hostnames, domain names |

## Setup

Automatically enabled after running `install.sh` / `install.ps1`. The global git config (`.config/git/config`) sets `core.hooksPath` to `~/dotfiles/claude-global/hooks`, activating the pre-commit hook for all repos.

### Initial Private Repo Whitelist

Run once to generate the whitelist (re-run when you create new private repos):

```bash
./bin/update-private-repos.sh
```

This creates `.context-private/private-repos.txt` containing all your private GitHub repos.

## Allowlist (Exception Patterns)

Add exceptions to `.private-info-allowlist`, one pattern per line:

```
# Global pattern (applies to all files)
git@github.com
noreply.github.com

# Per-file pattern (format: filepath:pattern)
docs/networking.md:192.168
```

## Blocklist (Additional Detection Patterns)

Add patterns to `.private-info-blocklist`, one regex per line:

```
# Hostname patterns
myhost\.local
internal\.example\.com

# Block unwanted commit message content
Co-Authored-By
```

## Manual Scanning

Scan specific files:

```bash
bin/check-private-info.sh path/to/file1 path/to/file2
```

Scan from stdin:

```bash
echo "some content" | bin/check-private-info.sh --stdin
echo "some content" | bin/check-private-info.sh --stdin filename-label
```

Scan all tracked files in a repo:

```bash
git ls-files | while read f; do [ -f "$f" ] && bin/check-private-info.sh "$f"; done
```

## Updating the Private Repo Whitelist

Re-run when you create new private repos on GitHub:

```bash
bin/update-private-repos.sh
```

## Troubleshooting

### Commit blocked by false positive

Add the pattern to `.private-info-allowlist` and re-commit.

### Pre-commit hook not running

Verify that `core.hooksPath` is set:

```bash
git config --get core.hooksPath
# Should show: ~/dotfiles/claude-global/hooks
```

### Claude Code hook not blocking

Ensure `settings.json` has the hooks section (check `~/.claude/settings.json`).

## Files

| File | Purpose |
|:---|:---|
| `bin/check-private-info.sh` | Scanner script (detection patterns) |
| `bin/update-private-repos.sh` | Private repo whitelist generator |
| `claude-global/hooks/pre-commit` | Git pre-commit hook |
| `claude-global/hooks/check-private-info.js` | Claude Code PreToolUse hook |
| `.private-info-allowlist` | Exception patterns |
| `.private-info-blocklist` | Additional detection patterns |
| `.context-private/private-repos.txt` | Private repo whitelist (gitignored) |
