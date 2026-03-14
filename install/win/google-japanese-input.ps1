# google-japanese-input.ps1 - Install Google Japanese Input
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# --- Install Google Japanese Input via winget ---
if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install Google Japanese Input manually."
    return
}
winget list --id Google.JapaneseInput 2>$null | Out-Null
$installed = $LASTEXITCODE -eq 0
if (-not $installed) {
    Write-Host "Installing Google Japanese Input via winget..."
    winget install --id Google.JapaneseInput --accept-source-agreements --accept-package-agreements
    Write-Host "Google Japanese Input installed." -ForegroundColor Green
} else {
    Write-Host "Google Japanese Input is already installed." -ForegroundColor DarkGray
}
