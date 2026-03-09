# install.ps1 - Unified installer for dotfiles (Windows)
# Usage:
#   .\install.ps1          # Symlinks only
#   .\install.ps1 -Full    # Symlinks + additional setup

param(
    [switch]$Full
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "=== dotfiles installer (Windows) ===" -ForegroundColor Cyan

# Step 1: Create symlinks
Write-Host ""
Write-Host "--- Creating symlinks ---"
& "$DotfilesDir\install\win\dotfileslink.ps1"

# Step 2: Clean up obsolete files
Write-Host ""
Write-Host "--- Cleaning up obsolete files ---"
& "$DotfilesDir\install\win\home-obsolete.ps1"

if ($Full) {
    # Step 3: Install Claude Code
    Write-Host ""
    Write-Host "--- Installing Claude Code ---"
    & "$DotfilesDir\install\win\claude-code.ps1"

    # Step 4: Install packages
    Write-Host ""
    Write-Host "--- Installing packages ---"
    & "$DotfilesDir\install\win\starship.ps1"
    & "$DotfilesDir\install\win\fnm.ps1"
    & "$DotfilesDir\install\win\uv.ps1"

    # Step 5: Install AutoHotkey (Japanese layout enforcer)
    Write-Host ""
    Write-Host "--- Setting up AutoHotkey ---"
    & "$DotfilesDir\install\win\autohotkey.ps1"
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
