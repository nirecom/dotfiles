# install.ps1 - Unified installer for dotfiles (Windows)
# Usage:
#   .\install.ps1          # Symlinks only
#   .\install.ps1 -Full    # Symlinks + additional setup

param(
    [switch]$Full
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = "$HOME\dotfiles"

Write-Host "=== dotfiles installer (Windows) ===" -ForegroundColor Cyan

# Step 1: Create symlinks
Write-Host ""
Write-Host "--- Creating symlinks ---"
& "$DotfilesDir\install\win\dotfileslink.ps1"

if ($Full) {
    # Step 2: Install packages
    Write-Host ""
    Write-Host "--- Installing packages ---"
    & "$DotfilesDir\install\win\starship.ps1"
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
