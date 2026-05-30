# install-obsolete.ps1 - Remove obsolete files and shortcuts (Windows)
# Usage: Called by install.ps1 (always, not only -Full)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# --- Remove old AutoHotkey startup shortcuts ---
# Detect by TargetPath pattern rather than filename (environment-independent)
$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path))
$startupDir = [Environment]::GetFolderPath('Startup')
$shell = New-Object -ComObject WScript.Shell
Get-ChildItem -Path $startupDir -Filter "*.lnk" -ErrorAction SilentlyContinue | ForEach-Object {
    $lnk = $shell.CreateShortcut($_.FullName)
    if ($lnk.TargetPath -like "*\AutoHotKey\*.ahk" -and $lnk.TargetPath -notlike "$DotfilesDir\*") {
        Write-Host "Removing obsolete startup shortcut: $($_.Name)"
        Remove-Item $_.FullName
    }
}

# --- Remove old AutoHotkey scripts from OneDrive ---
Get-ChildItem -Path "$env:USERPROFILE\OneDrive*\AutoHotKey\*.ahk" -ErrorAction SilentlyContinue | ForEach-Object {
    Write-Host "Removing obsolete AHK script from OneDrive: $($_.FullName)"
    Remove-Item $_.FullName
}

# --- Remove claude-code compatibility symlink ---
$oldClaude = Join-Path $DotfilesDir "claude-code"
if ((Test-Path $oldClaude) -and ((Get-Item $oldClaude -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    Write-Host "Removing obsolete symlink: claude-code (renamed to claude-global)"
    Remove-Item $oldClaude -Force
}

# --- Remove obsolete ~/.claude/* symlinks (claude-global moved to agents repo) ---
$obsoleteClaudeLinks = @("CLAUDE.md", "settings.json", "skills", "rules", "agents")
foreach ($name in $obsoleteClaudeLinks) {
    $link = Join-Path "$HOME\.claude" $name
    if (Test-Path $link -PathType Any) {
        $item = Get-Item $link -Force
        if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
            $target = $item.Target
            if ($target -like "*\dotfiles\claude-global\*" -or $target -like "*/dotfiles/claude-global/*") {
                Write-Host "Removing obsolete symlink: $link (was: $target)" -ForegroundColor Yellow
                Remove-Item $link -Force
            }
        }
    }
}

# --- Remove leftover dotfiles\claude-global\ directory (post-agents-split) ---
# Only when HEAD confirms claude-global\ is no longer tracked. Untracked leftover
# files survive `git reset --hard` and need explicit cleanup.
$cgDir = Join-Path $DotfilesDir 'claude-global'
$dfGit = Join-Path $DotfilesDir '.git'
if ((Test-Path $cgDir) -and (Test-Path $dfGit)) {
    $tracked = git -C $DotfilesDir ls-tree -r HEAD claude-global 2>$null
    if (-not $tracked) {
        Write-Host "Removing obsolete dotfiles\claude-global\ (post-agents-split leftover)" -ForegroundColor Yellow
        Remove-Item -Recurse -Force $cgDir
    }
}

# --- Remove obsolete doc-append.cmd (bin/doc-append.py moved to agents repo) ---
$docAppendCmd = "$HOME\.local\bin\doc-append.cmd"
if (Test-Path $docAppendCmd) {
    $content = Get-Content $docAppendCmd -Raw -ErrorAction SilentlyContinue
    if ($content -like "*dotfiles\bin\doc-append.py*") {
        Write-Host "Removing obsolete doc-append.cmd (bin/ moved to agents repo)" -ForegroundColor Yellow
        Remove-Item $docAppendCmd -Force
    }
}

# --- Unset obsolete core.hooksPath in config.local (claude-global removed) ---
$gitConfigLocal = Join-Path $DotfilesDir ".config\git\config.local"
if (Test-Path $gitConfigLocal) {
    $hooksPath = git config --file $gitConfigLocal core.hooksPath 2>$null
    if ($hooksPath -like "*\dotfiles\claude-global\hooks" -or $hooksPath -like "*/dotfiles/claude-global/hooks") {
        git config --file $gitConfigLocal --unset core.hooksPath 2>$null | Out-Null
        Write-Host "Removed obsolete core.hooksPath from config.local (claude-global removed)" -ForegroundColor Yellow
    }
}

# --- Remove obsolete core.hooksPath from config.local (now managed by agents installer via ~/.gitconfig) ---
$gitConfigLocal = Join-Path $DotfilesDir ".config\git\config.local"
if (Test-Path $gitConfigLocal) {
    $hooksPath = git config --file $gitConfigLocal core.hooksPath 2>$null
    if ($hooksPath -like "*\agents\hooks" -or $hooksPath -like "*/agents/hooks") {
        Write-Host ""
        Write-Host "core.hooksPath is set in config.local: $hooksPath"
        Write-Host "This is now written to ~/.gitconfig by the agents installer and is no longer needed here."
        $ans = Read-Host "Remove core.hooksPath from config.local? [y/N]"
        if ($ans -match '^[yY]') {
            git config --file $gitConfigLocal --unset core.hooksPath 2>$null | Out-Null
            Write-Host "Removed core.hooksPath from config.local." -ForegroundColor Yellow
        } else {
            Write-Host "Skipped."
        }
    }
}

# --- BEGIN temporary: ~/dotfiles,~/git → C:\git migration ---
# Remove old dotfiles directories after migration to C:\git
$migrationTargets = @(
    @{ Old = "$env:USERPROFILE\dotfiles"; New = "C:\git\dotfiles" },
    @{ Old = "$env:USERPROFILE\dotfiles-private"; New = "C:\git\dotfiles-private" },
    @{ Old = "$env:USERPROFILE\git"; New = "C:\git" }
)
foreach ($t in $migrationTargets) {
    if ((Test-Path $t.Old) -and (Test-Path $t.New)) {
        Remove-Item $t.Old -Recurse -Force -ErrorAction SilentlyContinue
        if (-not (Test-Path $t.Old)) {
            Write-Host "Removed old migration source: $($t.Old)" -ForegroundColor DarkGray
        } else {
            Write-Warning "Could not fully remove: $($t.Old) (files may be in use)"
        }
    }
}
# --- END temporary: ~/dotfiles,~/git → C:\git migration ---

# --- BEGIN temporary: .git/workflow → ~/.claude/projects/workflow migration ---
function Remove-OldGitWorkflow {
    param(
        [string]$OldDir,
        [string]$NewDir,
        [datetime]$Cutoff
    )
    if (-not (Test-Path $OldDir)) { return }
    Write-Host "Found old workflow dir: $OldDir"
    $n = 0
    Get-ChildItem -Path $OldDir -Filter "*.json" -ErrorAction SilentlyContinue | ForEach-Object {
        if ($_.LastWriteTime -ge $Cutoff) {
            New-Item -ItemType Directory -Path $NewDir -Force | Out-Null
            Copy-Item $_.FullName -Destination $NewDir -Force
            Write-Host "  Salvaged: $($_.Name)"
            $n++
        }
    }
    Remove-Item $OldDir -Recurse -Force -ErrorAction SilentlyContinue
    Write-Host "  Removed: $OldDir (salvaged $n file(s))"
}

$wfNewDir = Join-Path $HOME ".claude\projects\workflow"
$wfCutoff = (Get-Date).AddDays(-7)
$gitRoot = Split-Path $DotfilesDir -Parent
Get-ChildItem -Path $gitRoot -Directory -ErrorAction SilentlyContinue | ForEach-Object {
    Remove-OldGitWorkflow -OldDir (Join-Path $_.FullName ".git\workflow") -NewDir $wfNewDir -Cutoff $wfCutoff
}
# --- END temporary: .git/workflow → ~/.claude/projects/workflow migration ---
