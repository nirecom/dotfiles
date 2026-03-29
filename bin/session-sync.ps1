# session-sync.ps1 - Sync Claude Code session history across machines
# Usage:
#   .\session-sync.ps1 push    # Commit and push session data
#   .\session-sync.ps1 pull    # Pull latest session data
#   .\session-sync.ps1 status  # Show sync status
#   .\session-sync.ps1 reset   # Force-sync local to remote (for initial setup or recovery)

param(
    [Parameter(Mandatory, Position = 0)]
    [ValidateSet("push", "pull", "status", "reset")]
    [string]$Action,

    [string]$ClaudeDir = (Join-Path $env:USERPROFILE ".claude")
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ProjectsDir = Join-Path $ClaudeDir "projects"

if (-not (Test-Path (Join-Path $ProjectsDir ".git"))) {
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

        # Copy history.jsonl into sync area
        Copy-Item (Join-Path $ClaudeDir "history.jsonl") (Join-Path $ProjectsDir ".history.jsonl") -ErrorAction SilentlyContinue
        git -C $ProjectsDir add .
        $status = git -C $ProjectsDir status --porcelain
        if (-not $status) {
            Write-Host "No changes to push."
            return
        }

        $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm"
        git -C $ProjectsDir commit -m "sync: $env:COMPUTERNAME $timestamp"
        $ErrorActionPreference = "Continue"
        git -C $ProjectsDir pull --rebase origin main 2>&1 | Out-Null
        $ErrorActionPreference = "Stop"
        git -C $ProjectsDir push -u origin main
        Write-Host "Pushed session data." -ForegroundColor Green
    }
    "pull" {
        git -C $ProjectsDir pull --rebase
        # Merge remote history with local (dedup, preserve order)
        $syncHistory = Join-Path $ProjectsDir ".history.jsonl"
        $localHistory = Join-Path $ClaudeDir "history.jsonl"
        if (Test-Path $syncHistory) {
            $remote = @(Get-Content $syncHistory -ErrorAction SilentlyContinue)
            $local = @(Get-Content $localHistory -ErrorAction SilentlyContinue)
            $seen = [ordered]@{}
            foreach ($line in $remote + $local) { if ($line -and -not $seen.Contains($line)) { $seen[$line] = $true } }
            $seen.Keys | Set-Content $localHistory
        }
        Write-Host "Pulled session data." -ForegroundColor Green
    }
    "status" {
        git -C $ProjectsDir status
    }
    "reset" {
        $ErrorActionPreference = "Continue"
        git -C $ProjectsDir fetch origin main 2>&1 | Out-Null
        $ErrorActionPreference = "Stop"
        git -C $ProjectsDir reset --hard origin/main
        # Restore mtime from JSONL timestamps (git doesn't preserve mtime)
        Get-ChildItem -Path $ProjectsDir -Recurse -Filter "*.jsonl" | Where-Object { $_.Name -ne ".history.jsonl" } | ForEach-Object {
            $last = Get-Content $_.FullName -Tail 1 -ErrorAction SilentlyContinue
            if (-not ($last -match '"timestamp":"([^"]+)"')) {
                $last = Get-Content $_.FullName -TotalCount 1 -ErrorAction SilentlyContinue
            }
            if ($last -match '"timestamp":"([^"]+)"') {
                try { $_.LastWriteTime = [datetime]::Parse($Matches[1]).ToLocalTime() } catch {}
            }
        }
        # Merge remote history with local (dedup, preserve order)
        $syncHistory = Join-Path $ProjectsDir ".history.jsonl"
        $localHistory = Join-Path $ClaudeDir "history.jsonl"
        if (Test-Path $syncHistory) {
            $remote = @(Get-Content $syncHistory -ErrorAction SilentlyContinue)
            $local = @(Get-Content $localHistory -ErrorAction SilentlyContinue)
            $seen = [ordered]@{}
            foreach ($line in $remote + $local) { if ($line -and -not $seen.Contains($line)) { $seen[$line] = $true } }
            $seen.Keys | Set-Content $localHistory
        }
        Write-Host "Reset to remote state." -ForegroundColor Green
    }
}
