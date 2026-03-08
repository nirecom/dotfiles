# autohotkey.ps1 - Install AutoHotkey v2 and configure Japanese layout enforcer
# Usage: Called by install.ps1 -Full
# Prerequisite: English UI display language + Japanese as first preferred language

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path))

# --- Step 1: Language environment check (strict) ---
$uiLang = (Get-UICulture).Name
$firstLang = (Get-WinUserLanguageList)[0].LanguageTag

if (-not ($uiLang -like "en*")) {
    Write-Host "Skipping AutoHotkey setup: UI language is '$uiLang', not English." -ForegroundColor DarkGray
    return
}
if (-not ($firstLang -like "ja*")) {
    Write-Host "Skipping AutoHotkey setup: first preferred language is '$firstLang', not Japanese." -ForegroundColor DarkGray
    return
}

# --- Step 2: Install AutoHotkey v2 via winget ---
if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install AutoHotkey manually: https://www.autohotkey.com/"
    return
}
winget list --id AutoHotkey.AutoHotkey 2>$null | Out-Null
$ahkInstalled = $LASTEXITCODE -eq 0
if (-not $ahkInstalled) {
    Write-Host "Installing AutoHotkey v2 via winget..."
    winget install --id AutoHotkey.AutoHotkey --accept-source-agreements --accept-package-agreements
    Write-Host "AutoHotkey v2 installed." -ForegroundColor Green
} else {
    Write-Host "AutoHotkey v2 is already installed." -ForegroundColor DarkGray
}

# --- Step 3: Create startup shortcut ---
$ahkScript = Join-Path $DotfilesDir "win\config\autohotkey\force-japanese-layout.ahk"
if (-not (Test-Path $ahkScript)) {
    Write-Warning "AHK script not found: $ahkScript"
    return
}

$startupDir = [Environment]::GetFolderPath('Startup')
$shortcutPath = Join-Path $startupDir "force-japanese-layout.lnk"

if (Test-Path $shortcutPath) {
    Write-Host "Startup shortcut already exists: $shortcutPath" -ForegroundColor DarkGray
    return
}

$shell = New-Object -ComObject WScript.Shell
$shortcut = $shell.CreateShortcut($shortcutPath)
$shortcut.TargetPath = $ahkScript
$shortcut.WorkingDirectory = Split-Path $ahkScript -Parent
$shortcut.Save()
Write-Host "Created startup shortcut: $shortcutPath" -ForegroundColor Green
Write-Host "The AHK script will run automatically on next login." -ForegroundColor Yellow
