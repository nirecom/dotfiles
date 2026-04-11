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

# --- Remove ~/.gitconfig if it overrides dotfiles git config ---
$gitconfig = "$HOME\.gitconfig"
if ((Test-Path $gitconfig) -and -not ((Get-Item $gitconfig -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    Write-Host "~/.gitconfig exists and overrides dotfiles git config (XDG)."
    $response = Read-Host "Delete it? [y/N]"
    if ($response -eq 'y' -or $response -eq 'Y') {
        Remove-Item $gitconfig
        Write-Host "Deleted: $gitconfig" -ForegroundColor Green
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
