# claude-code.ps1 - Install Claude Code CLI via native installer
# Usage: Called by install.ps1

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (Get-Command claude -ErrorAction SilentlyContinue) {
    Write-Host "Claude Code is already installed." -ForegroundColor DarkGray
    return
}

if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    Write-Warning "Git for Windows is required. Install from https://git-scm.com/downloads/win"
    return
}

Write-Host "Installing Claude Code..."
irm https://claude.ai/install.ps1 | iex
Write-Host "Claude Code installed." -ForegroundColor Green
