# Global Claude Code Instructions

## Private task instructions

Work instructions received from Claude.ai or other external sources that contain private information must be saved under
`.context-private/`, not in `CLAUDE.md` or `context/`.

## Coding Guidelines

### Private Information

Never include the following in any file that may be committed to a public repository.
Use generic placeholders or descriptions instead.

**Personal information**
- Name, address, phone number, email address
- PII, My Number

**Local network information**
- IP addresses and subnet patterns
- Hostnames and domain names
- Internal URLs
- MAC addresses

**Hardware assets**
- Specific manufacturer names and hardware model numbers

**Local filesystem paths**
- Absolute paths to local directories (e.g., `\Projects`, `/home/user/work`)
- Use relative paths (`../sibling-repo/`) or generic placeholders instead

### Code State Claims

"A field exists" and "a field holds a meaningful value" are distinct claims — never conflate them.

### Public GitHub Rules

- Never commit private information. Use generic expressions when context is needed.
- Always add `.env` to `.gitignore` to exclude secrets from version control.
- Source code and config file messages/comments must be in English.

## Workflow Preferences

### Shell Commands

When providing shell commands (curl, docker, etc.):
- Always write commands on a single line — do NOT use backslash `\` line continuation

**curl commands MUST follow all three rules (PowerShell compatibility):**
1. Use `curl.exe` — NEVER bare `curl` (PowerShell aliases it to `Invoke-WebRequest`)
2. Use single quotes for JSON body — NEVER escaped double quotes:
   CORRECT: `curl.exe -d '{"key":"value"}'`
   WRONG:   `curl -d "{\"key\":\"value\"}"`
3. No line continuation — single line only

### Git Commands for Other Directories

When running git commands outside the current working directory, always use
`git -C <path>` instead of `cd <path> && git ...`.

CORRECT: `git -C /path/to/repo log --oneline -5`
WRONG:   `cd /path/to/repo && git log --oneline -5`

### File Edits

- Always show a diff before applying file edits. Do not apply edits without showing the diff first.

## Session Workflow

A list of target files is injected via SessionStart hook at the beginning of each session
(from `.claude/session-checklist.md`).

### Pre-processing (before starting work)
- Read all files listed in the injected checklist to understand current context

### Post-processing (after completing work)
- Re-read `.claude/session-checklist.md` to confirm the target file list
- Update all listed files if the work changed their relevant content
- Confirm with user before commit/push
