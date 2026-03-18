# claude-usage-widget.ps1 - Install Claude Usage Widget and configure autostart
# Usage: Called by install.ps1 -Full
# Source: https://github.com/SlavomirDurej/claude-usage-widget

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$AppName = "Claude Usage Widget"
$InstallDir = Join-Path $env:ProgramFiles "Claude-Usage-Widget"
$ExePath = Join-Path $InstallDir "Claude-Usage-Widget.exe"

# Check if already installed
if (Test-Path $ExePath) {
    Write-Host "$AppName is already installed." -ForegroundColor DarkGray
} else {
    # Get latest release download URL from GitHub API
    Write-Host "Fetching latest release info..."
    $releaseInfo = Invoke-RestMethod -Uri "https://api.github.com/repos/SlavomirDurej/claude-usage-widget/releases/latest"
    $setupAsset = $releaseInfo.assets | Where-Object { $_.name -match "win-Setup\.exe$" } | Select-Object -First 1
    if (-not $setupAsset) {
        Write-Warning "Could not find Windows Setup exe in latest release."
        return
    }

    $downloadUrl = $setupAsset.browser_download_url
    $tmpFile = Join-Path $env:TEMP "claude-usage-widget-setup.exe"

    Write-Host "Downloading $AppName from $downloadUrl ..."
    Invoke-WebRequest -Uri $downloadUrl -OutFile $tmpFile -UseBasicParsing

    Write-Host "Installing $AppName (silent)..."
    Start-Process -FilePath $tmpFile -ArgumentList "/S" -Wait

    Remove-Item $tmpFile -Force -ErrorAction SilentlyContinue

    if (Test-Path $ExePath) {
        Write-Host "$AppName installed." -ForegroundColor Green
    } else {
        Write-Warning "$AppName install may have failed. Exe not found at: $ExePath"
        return
    }
}

# Configure autostart via registry Run key (same method the app uses for "Launch at startup")
$regPath = "HKCU:\Software\Microsoft\Windows\CurrentVersion\Run"
$regName = "electron.app.Claude-Usage-Widget"
$regEntry = Get-ItemProperty -Path $regPath -Name $regName -ErrorAction SilentlyContinue

if ($regEntry) {
    Write-Host "Autostart already configured (registry Run key)." -ForegroundColor DarkGray
} else {
    New-ItemProperty -Path $regPath -Name $regName -Value $ExePath -PropertyType String -Force | Out-Null
    Write-Host "Registered autostart in registry Run key." -ForegroundColor Green
    Write-Host "$AppName will run automatically on next login." -ForegroundColor Yellow
}
