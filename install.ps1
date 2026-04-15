# install.ps1 - Unified installer for dotfiles (Windows)
# Usage:
#   .\install.ps1            # Symlinks only
#   .\install.ps1 -Base      # + base packages (starship, uv, etc.)
#   .\install.ps1 -Develop   # + Base + dev tools (awscli, vscode)
#   .\install.ps1 -Toolchain # + Develop + toolchain (VS C++)
#   .\install.ps1 -Full      # All of the above

param(
    [switch]$Base,
    [switch]$Develop,
    [switch]$Toolchain,
    [switch]$Full
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "=== dotfiles installer (Windows) ===" -ForegroundColor Cyan

# Set DOTFILES_DIR as persistent user environment variable (used by Claude Code hooks)
[Environment]::SetEnvironmentVariable('DOTFILES_DIR', $DotfilesDir, 'User')
$env:DOTFILES_DIR = $DotfilesDir

# Wait for any running MSI installer before proceeding
function Wait-MsiMutex {
    $maxWait = 120
    $waited = 0
    while (Get-Process msiexec -ErrorAction SilentlyContinue) {
        if ($waited -eq 0) {
            Write-Host "Waiting for another installer to finish..." -ForegroundColor Yellow
        }
        Start-Sleep -Seconds 5
        $waited += 5
        if ($waited -ge $maxWait) {
            Write-Warning "Installer still running after ${maxWait}s — proceeding anyway"
            break
        }
    }
}
Wait-MsiMutex

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

# Step 6: Disable Ctrl+Shift input language hotkey
Write-Host ""
Write-Host "--- Disabling input language hotkeys ---"
$toggleKey = 'HKCU:\Keyboard Layout\Toggle'
$keyItem        = Get-Item -Path $toggleKey -ErrorAction SilentlyContinue
$languageHotkey = if ($keyItem) { $keyItem.GetValue('Language Hotkey') } else { $null }
$layoutHotkey   = if ($keyItem) { $keyItem.GetValue('Layout Hotkey') } else { $null }
$hotkeyValue    = if ($keyItem) { $keyItem.GetValue('Hotkey') } else { $null }
if ($languageHotkey -ne '3' -or $layoutHotkey -ne '3' -or $hotkeyValue -ne '3') {
    Set-ItemProperty $toggleKey -Name 'Language Hotkey' -Value '3'
    Set-ItemProperty $toggleKey -Name 'Layout Hotkey' -Value '3'
    Set-ItemProperty $toggleKey -Name 'Hotkey' -Value '3'
    Write-Host "Disabled Ctrl+Shift / Alt+Shift language hotkeys." -ForegroundColor Green
} else {
    Write-Host "Input language hotkeys already disabled." -ForegroundColor DarkGray
}

if ($Base -or $Develop -or $Toolchain -or $Full) {
    # Step 7: Install base packages
    Write-Host ""
    Write-Host "--- Installing base packages ---"
    & "$DotfilesDir\install\win\starship.ps1"
    & "$DotfilesDir\install\win\uv.ps1"
    & "$DotfilesDir\install\win\google-japanese-input.ps1"
    & "$DotfilesDir\install\win\autohotkey.ps1"
    & "$DotfilesDir\install\win\powertoys.ps1"

    # Step 8: Install Claude Usage Widget
    Write-Host ""
    Write-Host "--- Installing Claude Usage Widget ---"
    & "$DotfilesDir\install\win\claude-usage-widget.ps1"

    # Step 9: Install Claude Tabs
    Write-Host ""
    Write-Host "--- Installing Claude Tabs ---"
    & "$DotfilesDir\install\win\claude-tabs.ps1"
}

if ($Develop -or $Toolchain -or $Full) {
    # Step 10: Install development tools
    Write-Host ""
    Write-Host "--- Installing development tools ---"
    & "$DotfilesDir\install\win\awscli.ps1"

    # Step 11: Install VS Code and extensions
    Write-Host ""
    Write-Host "--- Installing Visual Studio Code ---"
    & "$DotfilesDir\install\win\vscode.ps1"
}

if ($Toolchain -or $Full) {
    # Step 12: Install toolchain
    Write-Host ""
    Write-Host "--- Installing toolchain ---"
    & "$DotfilesDir\install\win\vs-cpp.ps1"
}

# Run dotfiles-private installer if available
$PrivateInstaller = Join-Path (Split-Path -Parent $DotfilesDir) "dotfiles-private\install.ps1"
if (Test-Path $PrivateInstaller) {
    Write-Host ""
    Write-Host "--- Running dotfiles-private installer ---"
    $privateArgs = @{}
    if ($Base) { $privateArgs['Base'] = $true }
    if ($Develop) { $privateArgs['Develop'] = $true }
    if ($Toolchain) { $privateArgs['Toolchain'] = $true }
    if ($Full) { $privateArgs['Full'] = $true }
    & $PrivateInstaller @privateArgs
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
