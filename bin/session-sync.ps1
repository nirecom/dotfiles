# session-sync.ps1 - Sync Claude Code session history across machines
# Usage:
#   .\session-sync.ps1 push    # Commit and push session data
#   .\session-sync.ps1 pull    # Pull latest session data
#   .\session-sync.ps1 status  # Show sync status

param(
    [Parameter(Mandatory, Position = 0)]
    [ValidateSet("push", "pull", "status")]
    [string]$Action,

    [string]$ClaudeDir = (Join-Path $env:USERPROFILE ".claude")
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (-not (Test-Path (Join-Path $ClaudeDir ".git"))) {
    Write-Error "Session sync not initialized. Run install.ps1 or install\win\session-sync-init.ps1 first."
    return
}

switch ($Action) {
    "push" {
        # Warn if Claude Code is running (JSONL may not be flushed)
        $claudeProcs = Get-Process -Name "claude" -ErrorAction SilentlyContinue
        if ($claudeProcs) {
            Write-Warning "Claude Code is running. Close all sessions before push to ensure latest data is saved."
        }

        git -C $ClaudeDir add projects/
        $status = git -C $ClaudeDir status --porcelain projects/
        if (-not $status) {
            Write-Host "No changes to push."
            return
        }

        $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm"
        git -C $ClaudeDir commit -m "sync: $env:COMPUTERNAME $timestamp"
        git -C $ClaudeDir push
        Write-Host "Pushed session data." -ForegroundColor Green
    }
    "pull" {
        git -C $ClaudeDir pull --rebase
        Write-Host "Pulled session data." -ForegroundColor Green
    }
    "status" {
        git -C $ClaudeDir status projects/
    }
}
