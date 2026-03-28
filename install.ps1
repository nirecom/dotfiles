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

# Set DOTFILES_DIR as persistent user environment variable (used by Claude Code hooks)
[Environment]::SetEnvironmentVariable('DOTFILES_DIR', $DotfilesDir, 'User')
$env:DOTFILES_DIR = $DotfilesDir

# Step 1: Create symlinks
Write-Host ""
Write-Host "--- Creating symlinks ---"
& "$DotfilesDir\install\win\dotfileslink.ps1"

# Step 2: Install Claude Code and Node.js (required for hooks)
Write-Host ""
Write-Host "--- Installing Claude Code ---"
& "$DotfilesDir\install\win\claude-code.ps1"
if (Get-Command claude -ErrorAction SilentlyContinue) {
    Write-Host ""
    Write-Host "--- Initializing Claude Code session sync ---"
    & "$DotfilesDir\install\win\session-sync-init.ps1"
}
Write-Host ""
Write-Host "--- Installing fnm (Node.js) ---"
& "$DotfilesDir\install\win\fnm.ps1"

# Step 3: Clean up obsolete files
Write-Host ""
Write-Host "--- Cleaning up obsolete files ---"
& "$DotfilesDir\install\win\install-obsolete.ps1"

# Step 4: Configure notification sounds
Write-Host ""
Write-Host "--- Configuring notification sounds ---"
& "$DotfilesDir\install\win\sounds.ps1"

# Step 5: Disable Snipping Tool notifications
Write-Host ""
Write-Host "--- Disabling Snipping Tool notifications ---"
& "$DotfilesDir\install\win\snipping-tool.ps1"

if ($Base -or $Full) {
    # Step 6: Install base packages
    Write-Host ""
    Write-Host "--- Installing base packages ---"
    & "$DotfilesDir\install\win\starship.ps1"
    & "$DotfilesDir\install\win\uv.ps1"
    & "$DotfilesDir\install\win\google-japanese-input.ps1"
    & "$DotfilesDir\install\win\autohotkey.ps1"
    & "$DotfilesDir\install\win\powertoys.ps1"

    # Step 7: Install Rize
    Write-Host ""
    Write-Host "--- Installing Rize ---"
    & "$DotfilesDir\install\win\rize.ps1"

    # Step 8: Install Claude Usage Widget
    Write-Host ""
    Write-Host "--- Installing Claude Usage Widget ---"
    & "$DotfilesDir\install\win\claude-usage-widget.ps1"

    # Step 9: Install Claude Tabs
    Write-Host ""
    Write-Host "--- Installing Claude Tabs ---"
    & "$DotfilesDir\install\win\claude-tabs.ps1"
}

if ($Develop -or $Full) {
    # Step 10: Install development tools
    Write-Host ""
    Write-Host "--- Installing development tools ---"
    & "$DotfilesDir\install\win\vs-cpp.ps1"
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
