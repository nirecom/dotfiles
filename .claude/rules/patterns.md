# Coding Patterns

- **Defensive loading:** All external tools/files must be checked before use (`type cmd >/dev/null 2>&1`, `[ -e $FILE ]`)
- **Cross-platform branching:** Feature setup in `.profile_common` must branch on `$OSDIST` via `case` statements — always maintain this pattern when adding new tool configurations
