# claude-usage-widget.ps1 - Install Claude Usage Widget and configure autostart
# Usage: Called by install.ps1 -Full
# Source: https://github.com/SlavomirDurej/claude-usage-widget

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$AppName = "Claude Usage Widget"
$ExeName = "Claude-Usage-Widget.exe"

# Normalize version for comparison: strip trailing .0 components so
# ProductVersion (4-part "1.7.2.0") and tag_name (3-part "1.7.2") compare equal.
function ConvertTo-NormalizedWidgetVersion([string]$v) {
    return ($v -replace '(\.0)+$', '')
}

# Check both per-user and per-machine install paths
$candidates = @(
    Join-Path $env:LOCALAPPDATA "Programs\Claude-Usage-Widget\$ExeName"
    Join-Path $env:ProgramFiles "Claude-Usage-Widget\$ExeName"
)
$ExePath = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1

# Fetch latest release info up-front (needed for both install and update-check paths)
Write-Host "Fetching latest release info..."
$releaseInfo = Invoke-RestMethod -Uri "https://api.github.com/repos/SlavomirDurej/claude-usage-widget/releases/latest"
$latestVersion = $releaseInfo.tag_name.TrimStart('v')

if ($ExePath) {
    $installedVersion = (Get-Item $ExePath).VersionInfo.ProductVersion
    if ((ConvertTo-NormalizedWidgetVersion $installedVersion) -eq (ConvertTo-NormalizedWidgetVersion $latestVersion)) {
        Write-Host "$AppName is up to date (v$installedVersion)." -ForegroundColor DarkGray
        return
    }
    Write-Host "$AppName update available: v$installedVersion -> v$latestVersion" -ForegroundColor Yellow
} else {
    Write-Host "Installing $AppName v$latestVersion..."
}

$setupAsset = $releaseInfo.assets | Where-Object { $_.name -match "win-Setup\.exe$" } | Select-Object -First 1
if (-not $setupAsset) {
    Write-Warning "Could not find Windows Setup exe in latest release."
    return
}

$downloadUrl = $setupAsset.browser_download_url
$tmpFile = Join-Path $env:TEMP "claude-usage-widget-setup.exe"

Write-Host "Downloading $AppName from $downloadUrl ..."
Invoke-WebRequest -Uri $downloadUrl -OutFile $tmpFile -UseBasicParsing

# Widget locks its exe while running; stop it before install so the installer can overwrite.
$widgetProcs = Get-Process -Name "Claude-Usage-Widget" -ErrorAction SilentlyContinue
if ($widgetProcs) {
    Write-Host "Stopping running $AppName to allow update..." -ForegroundColor DarkGray
    $widgetProcs | Stop-Process -Force
    Start-Sleep -Seconds 2
}

Write-Host "Installing $AppName (silent, per-user)..."
Start-Process -FilePath $tmpFile -ArgumentList "/S" -Wait

Remove-Item $tmpFile -Force -ErrorAction SilentlyContinue

# Installer spawns a child process; poll until version matches latest or timeout.
# On update, the old exe already exists, so we must check version equality — not just existence.
$timeout = 90
$success = $false
for ($i = 0; $i -lt $timeout; $i += 5) {
    $ExePath = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1
    if ($ExePath) {
        $currentVersion = (Get-Item $ExePath).VersionInfo.ProductVersion
        if ((ConvertTo-NormalizedWidgetVersion $currentVersion) -eq (ConvertTo-NormalizedWidgetVersion $latestVersion)) { $success = $true; break }
    }
    Write-Host "  Waiting for install to complete... ($i s)" -ForegroundColor DarkGray
    Start-Sleep -Seconds 5
}
if ($success) {
    Write-Host "$AppName installed (v$latestVersion)." -ForegroundColor Green
} else {
    Write-Warning "$AppName install may have failed. Exe not found or version mismatch."
    return
}
