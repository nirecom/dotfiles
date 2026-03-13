#!/usr/bin/env node
// Claude Code PreToolUse hook: check Edit/Write content for private information
// Skips scanning for repos listed in .context-private/private-repos.txt

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");

// Read stdin (cross-platform: fs.readSync for Windows compatibility)
function readStdin() {
  const chunks = [];
  const buf = Buffer.alloc(4096);
  let bytesRead;
  try {
    while (true) {
      bytesRead = fs.readSync(0, buf, 0, buf.length);
      if (bytesRead === 0) break;
      chunks.push(buf.slice(0, bytesRead));
    }
  } catch (e) {
    // EOF or error
  }
  return Buffer.concat(chunks).toString("utf8");
}

function approve() {
  console.log(JSON.stringify({ decision: "approve" }));
  process.exit(0);
}

function block(reason) {
  console.log(JSON.stringify({ decision: "block", reason }));
  process.exit(0);
}

// Determine dotfiles directory (this script lives in dotfiles/claude-code/hooks/)
const DOTFILES_DIR = path.resolve(__dirname, "..", "..");
const SCANNER = path.join(DOTFILES_DIR, "bin", "check-private-info.sh");
const PRIVATE_REPOS = path.join(
  DOTFILES_DIR,
  ".context-private",
  "private-repos.txt"
);

// Parse stdin
const input = JSON.parse(readStdin());
const toolName = input.tool_name;
const toolInput = input.tool_input || {};

// Only check Edit and Write tools
if (toolName !== "Edit" && toolName !== "Write") {
  approve();
}

// Extract file path and content to scan
const filePath = toolInput.file_path || "";
let content = "";

if (toolName === "Write") {
  content = toolInput.content || "";
} else if (toolName === "Edit") {
  content = toolInput.new_string || "";
}

if (!content) {
  approve();
}

// Check if the target file is in a private repo
if (filePath && fs.existsSync(PRIVATE_REPOS)) {
  try {
    const repoRoot = execSync(`git -C "${path.dirname(filePath)}" rev-parse --show-toplevel 2>/dev/null`, {
      encoding: "utf8",
      timeout: 5000,
    }).trim();

    if (repoRoot) {
      const remoteUrl = execSync(`git -C "${repoRoot}" remote get-url origin 2>/dev/null`, {
        encoding: "utf8",
        timeout: 5000,
      }).trim();

      if (remoteUrl) {
        // Extract owner/repo from SSH or HTTPS URL
        const match = remoteUrl.match(/[/:]([^/]+\/[^/]+?)(?:\.git)?$/);
        if (match) {
          const repoId = match[1];
          const privateRepos = fs
            .readFileSync(PRIVATE_REPOS, "utf8")
            .split(/\r?\n/)
            .filter((l) => l.trim());
          if (privateRepos.includes(repoId)) {
            approve();
          }
        }
      }
    }
  } catch (e) {
    // If git commands fail, continue with scan (safe default)
  }
}

// Run scanner on the content
try {
  const label = filePath || "stdin";
  execSync(`bash "${SCANNER}" --stdin "${label}"`, {
    input: content,
    encoding: "utf8",
    timeout: 10000,
    stdio: ["pipe", "pipe", "pipe"],
  });
  // Scanner exited 0 = clean
  approve();
} catch (e) {
  // Scanner exited non-zero = violations found
  const output = (e.stdout || "").trim();
  block(`Private information detected:\n${output}`);
}
