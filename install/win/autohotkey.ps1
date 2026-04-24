# autohotkey.ps1 - Install AutoHotkey v2 and configure Japanese layout enforcer
# Usage: Called by install.ps1 -Full
# Prerequisite: English UI display language + Japanese as first preferred language

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path))

# --- Step 1: Language environment check (strict) ---
$uiLang = (Get-UICulture).Name
try {
    $savedWP = $WarningPreference
    $WarningPreference = 'SilentlyContinue'
    Import-Module International -UseWindowsPowerShell -ErrorAction Stop
    $WarningPreference = $savedWP
    $firstLang = (Get-WinUserLanguageList)[0].LanguageTag
} catch {
    Write-Host "Skipping AutoHotkey setup: International module unavailable ($_)" -ForegroundColor DarkGray
    return
}

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
    if ($LASTEXITCODE -eq 0) {
        Write-Host "AutoHotkey v2 installed." -ForegroundColor Green
    } else {
        Write-Warning "AutoHotkey v2 installation failed (exit code: $LASTEXITCODE). Re-run install.ps1 to retry."
    }
} else {
    Write-Host "AutoHotkey v2 is already installed." -ForegroundColor DarkGray
}

# --- Step 3: Create startup shortcut ---
$ahkScript = Join-Path $DotfilesDir "config\win\autohotkey\force-japanese-layout.ahk"
if (-not (Test-Path $ahkScript)) {
    Write-Warning "AHK script not found: $ahkScript"
    return
}

$ahkExe = "C:\Program Files\AutoHotkey\v2\AutoHotkey64.exe"
if (-not (Test-Path $ahkExe)) {
    $ahkExe = Join-Path $env:LOCALAPPDATA "Programs\AutoHotkey\v2\AutoHotkey64.exe"
}
if (-not (Test-Path $ahkExe)) {
    Write-Warning "AutoHotkey executable not found in per-machine or per-user paths"
    return
}

# --- Step 3a: Restart AHK if running from old dotfiles location ---
$ahkProc = Get-CimInstance Win32_Process -Filter "Name = 'AutoHotkey64.exe'" -ErrorAction SilentlyContinue |
    Where-Object { $_.CommandLine -like '*force-japanese-layout.ahk*' -and $_.CommandLine -notlike "*$ahkScript*" }
if ($ahkProc) {
    Write-Host "Restarting AHK from new dotfiles location..." -ForegroundColor Yellow
    $ahkProc | ForEach-Object { Stop-Process -Id $_.ProcessId -Force }
    Start-Process -FilePath $ahkExe -ArgumentList "`"$ahkScript`""
    Write-Host "AHK restarted: $ahkScript" -ForegroundColor Green
}

$startupDir = [Environment]::GetFolderPath('Startup')
$shortcutPath = Join-Path $startupDir "force-japanese-layout.lnk"

if (Test-Path $shortcutPath) {
    $shell = New-Object -ComObject WScript.Shell
    $existing = $shell.CreateShortcut($shortcutPath)
    if ($existing.TargetPath -eq $ahkExe -and $existing.Arguments -eq "`"$ahkScript`"") {
        Write-Host "Startup shortcut already up to date: $shortcutPath" -ForegroundColor DarkGray
    } elseif ($existing.TargetPath -eq $ahkExe) {
        $existing.Arguments = "`"$ahkScript`""
        $existing.WorkingDirectory = Split-Path $ahkScript -Parent
        $existing.Save()
        Write-Host "Updated shortcut arguments: $shortcutPath" -ForegroundColor Green
    } elseif ($existing.TargetPath -like "*.ahk") {
        $existing.TargetPath = $ahkExe
        $existing.Arguments = "`"$ahkScript`""
        $existing.WorkingDirectory = Split-Path $ahkScript -Parent
        $existing.Save()
        Write-Host "Migrated shortcut to exe format: $shortcutPath" -ForegroundColor Green
    } else {
        Write-Host "Startup shortcut has unexpected target: $($existing.TargetPath)" -ForegroundColor Yellow
    }
    return
}

$shell = New-Object -ComObject WScript.Shell
$shortcut = $shell.CreateShortcut($shortcutPath)
$shortcut.TargetPath = $ahkExe
$shortcut.Arguments = "`"$ahkScript`""
$shortcut.WorkingDirectory = Split-Path $ahkScript -Parent
$shortcut.Save()
Write-Host "Created startup shortcut: $shortcutPath" -ForegroundColor Green
Write-Host "The AHK script will run automatically on next login." -ForegroundColor Yellow
