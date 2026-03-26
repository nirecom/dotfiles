# claude-tabs.ps1 - Install Claude Tabs
# Usage: Called by install.ps1 -Base or -Full
# Source: https://github.com/gunba/claude-tabs

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$AppName = "Claude Tabs"
$ExeName = "claude-tabs.exe"

# Check both per-user and per-machine install paths
$candidates = @(
    Join-Path $env:LOCALAPPDATA "Claude Tabs\$ExeName"
    Join-Path $env:ProgramFiles "Claude Tabs\$ExeName"
)
$ExePath = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1

if ($ExePath) {
    Write-Host "$AppName is already installed." -ForegroundColor DarkGray
} else {
    # Get latest release download URL from GitHub API
    # /releases/latest may point to a release with no assets (e.g. draft/prerelease).
    # Walk recent releases to find the first one with a Windows setup exe.
    Write-Host "Fetching latest release info..."
    $releases = Invoke-RestMethod -Uri "https://api.github.com/repos/gunba/claude-tabs/releases?per_page=10"
    $setupAsset = $null
    foreach ($rel in $releases) {
        $setupAsset = $rel.assets | Where-Object { $_.name -match "x64-setup\.exe$" } | Select-Object -First 1
        if ($setupAsset) { break }
    }
    if (-not $setupAsset) {
        Write-Warning "Could not find Windows Setup exe in latest release."
        return
    }

    $downloadUrl = $setupAsset.browser_download_url
    $tmpFile = Join-Path $env:TEMP "claude-tabs-setup.exe"

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
