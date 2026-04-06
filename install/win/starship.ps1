# starship.ps1 - Install Starship prompt via winget
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install Starship manually: https://starship.rs"
    return
}
winget list --id Starship.Starship 2>$null | Out-Null
$starshipInstalled = $LASTEXITCODE -eq 0
if (-not $starshipInstalled) {
    Write-Host "Installing Starship via winget..."
    winget install --id Starship.Starship --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -eq 0) {
        Write-Host "Starship installed." -ForegroundColor Green
        Write-Host "Restart your terminal to activate the Starship prompt." -ForegroundColor Yellow
    } else {
        Write-Warning "Starship installation failed (exit code: $LASTEXITCODE). Re-run install.ps1 to retry."
    }
} else {
    Write-Host "Starship is already installed." -ForegroundColor DarkGray
}
