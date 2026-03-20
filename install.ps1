# install.ps1 - Unified installer for dotfiles (Windows)
# Usage:
#   .\install.ps1            # Symlinks only
#   .\install.ps1 -Base      # Symlinks + base packages
#   .\install.ps1 -Develop   # Symlinks + dev tools
#   .\install.ps1 -Full      # Symlinks + base + dev tools

param(
    [switch]$Base,
    [switch]$Develop,
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

# Step 2: Install Claude Code
Write-Host ""
Write-Host "--- Installing Claude Code ---"
& "$DotfilesDir\install\win\claude-code.ps1"

# Step 3: Clean up obsolete files
Write-Host ""
Write-Host "--- Cleaning up obsolete files ---"
& "$DotfilesDir\install\win\install-obsolete.ps1"

# Step 4: Configure notification sounds
Write-Host ""
Write-Host "--- Configuring notification sounds ---"
& "$DotfilesDir\install\win\sounds.ps1"

if ($Base -or $Full) {
    # Step 5: Install base packages
    Write-Host ""
    Write-Host "--- Installing base packages ---"
    & "$DotfilesDir\install\win\starship.ps1"
    & "$DotfilesDir\install\win\uv.ps1"
    & "$DotfilesDir\install\win\google-japanese-input.ps1"
    & "$DotfilesDir\install\win\autohotkey.ps1"
    & "$DotfilesDir\install\win\powertoys.ps1"

    # Step 6: Install Rize
    Write-Host ""
    Write-Host "--- Installing Rize ---"
    & "$DotfilesDir\install\win\rize.ps1"

    # Step 7: Install Claude Usage Widget
    Write-Host ""
    Write-Host "--- Installing Claude Usage Widget ---"
    & "$DotfilesDir\install\win\claude-usage-widget.ps1"
}

if ($Develop -or $Full) {
    # Step 8: Install development tools
    Write-Host ""
    Write-Host "--- Installing development tools ---"
    & "$DotfilesDir\install\win\fnm.ps1"
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
