# rize.ps1 - Install Rize time tracker via winget (MS Store)
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install Rize manually: https://app.rize.io/downloads"
    return
}

winget list --name Rize 2>$null | Out-Null
$installed = $LASTEXITCODE -eq 0
if (-not $installed) {
    Write-Host "Installing Rize via winget (MS Store)..."
    winget install --id XPDC57FTXPM1P5 --accept-source-agreements --accept-package-agreements
    Write-Host "Rize installed." -ForegroundColor Green
} else {
    Write-Host "Rize is already installed." -ForegroundColor DarkGray
}
