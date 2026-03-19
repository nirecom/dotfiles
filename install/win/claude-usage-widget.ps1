# claude-usage-widget.ps1 - Install Claude Usage Widget and configure autostart
# Usage: Called by install.ps1 -Full
# Source: https://github.com/SlavomirDurej/claude-usage-widget

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$AppName = "Claude Usage Widget"
$ExeName = "Claude-Usage-Widget.exe"

# Check both per-user and per-machine install paths
$candidates = @(
    Join-Path $env:LOCALAPPDATA "Programs\Claude-Usage-Widget\$ExeName"
    Join-Path $env:ProgramFiles "Claude-Usage-Widget\$ExeName"
)
$ExePath = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1

if ($ExePath) {
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

    Write-Host "Installing $AppName (silent, per-user)..."
    Start-Process -FilePath $tmpFile -ArgumentList "/S" -Wait

    Remove-Item $tmpFile -Force -ErrorAction SilentlyContinue

    # Installer spawns a child process; poll until exe appears or timeout
    $timeout = 90
    for ($i = 0; $i -lt $timeout; $i += 5) {
        $ExePath = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1
        if ($ExePath) { break }
        Write-Host "  Waiting for install to complete... ($i s)" -ForegroundColor DarkGray
        Start-Sleep -Seconds 5
    }
    if ($ExePath) {
        Write-Host "$AppName installed." -ForegroundColor Green
    } else {
        Write-Warning "$AppName install may have failed. Exe not found in expected locations."
        return
    }
}
