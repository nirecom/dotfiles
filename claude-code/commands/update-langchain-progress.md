Update `langchain-progress.md` to reflect the latest phase progress of the LangChain LLM-as-a-Judge project

## Target files

- `langchain-progress.md` is searched in this order:
  - Local directory `../ai-specs/projects/engineering/`
  - git repo `git@github.com:nirecom/ai-specs.git` (path: `projects/engineering/langchain-progress.md`)

- Source repos (for detecting progress):
  - langchain-stack: `../langchain-stack/` → `git@github.com:nirecom/langchain-stack.git`
  - litellm-stack: `../litellm-stack/` → `git@github.com:nirecom/litellm-stack.git`
  - open-webui-stack: `../open-webui-stack/` → `git@github.com:nirecom/open-webui-stack.git`
  - portable-llm-server: `../portable-llm-server/` → `git@github.com:nirecom/portable-llm-server.git`

- Phase definitions: `langchain-design.md` §11 (same directory as progress.md)

## Procedure

1. **Gather recent commits**: For each source repo, run `git log --oneline -20`
2. **Read current progress**: Read `langchain-progress.md`
3. **Read phase definitions**: Read `langchain-design.md` §11 to understand phase scope
4. **Identify updates**: Compare commits against the progress document:
   - New phase completions or partial progress
   - Task items that have been completed
   - New implementation notes worth recording
5. **Propose updates**: Present changes to the user:
   - Phase Status Summary table updates
   - New or updated Completed Phase Details sections
   - New Implementation Notes
6. **Apply after confirmation**: Edit the file only after user approval

## Rules

- Language: English (the document is written in English)
- Use checkbox format for task lists: `- [x]` completed, `- [ ]` pending
- Phase Status Summary table must stay in sync with detailed sections
- Include implementation notes only for decisions that affect future phases
