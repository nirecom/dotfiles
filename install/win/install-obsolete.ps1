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
