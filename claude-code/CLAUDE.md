# Global Claude Code Instructions

## Git-Tracked vs Ignored Files

### Never commit these (already in ~/.config/git/ignore)

| File / Directory | Reason |
|---|---|
| `**/CLAUDE.local.md` | Personal preferences and local overrides |
| `**/.context-private/` | Private task instructions, client notes, draft specs |
| `**/.claude/settings.local.json` | Local Claude Code settings |

### Private task instructions

Work instructions received from Claude.ai or other external sources that contain
sensitive information (client names, internal URLs, PII) must be saved under
`.context-private/`, not in `CLAUDE.md` or `context/`.

## Coding Guidelines

### Private Information

Never include the following in any file that may be committed to a public repository.
Use generic placeholders or descriptions instead.

**Personal information**
- Name, address, phone number, email address

**Local network information**
- IP addresses and subnet patterns
- Hostnames and domain names
- MAC addresses

**Hardware assets**
- Specific manufacturer names and hardware model numbers

### Code State Claims

"A field exists" and "a field holds a meaningful value" are distinct claims — never conflate them.

### Public GitHub Rules

- Never commit private information. Use generic expressions when context is needed.
- Always add `.env` to `.gitignore` to exclude secrets from version control.
- Source code and config file messages/comments must be in English.

## Workflow Preferences

### File Edits

- Always show a diff before applying file edits. Do not apply edits without showing the diff first.
