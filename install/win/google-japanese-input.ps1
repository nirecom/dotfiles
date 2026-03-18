# google-japanese-input.ps1 - Install Google Japanese Input
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# --- Install Google Japanese Input via winget ---
if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install Google Japanese Input manually."
    return
}
winget list --id Google.JapaneseIME 2>$null | Out-Null
$installed = $LASTEXITCODE -eq 0
if (-not $installed) {
    Write-Host "Installing Google Japanese Input (Google.JapaneseIME) via winget..."
    winget install --id Google.JapaneseIME --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Google Japanese Input installed." -ForegroundColor Green
    } else {
        Write-Warning "Google Japanese Input installation failed (exit code: $LASTEXITCODE)."
    }
} else {
    Write-Host "Google Japanese Input is already installed." -ForegroundColor DarkGray
}
