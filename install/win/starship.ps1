# starship.ps1 - Install Starship prompt via winget
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$starshipExe = Join-Path $env:ProgramFiles "starship\bin\starship.exe"
if (-not ((Get-Command starship -ErrorAction SilentlyContinue) -or (Test-Path $starshipExe))) {
    if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
        Write-Warning "winget not found. Install Starship manually: https://starship.rs"
        return
    }
    Write-Host "Installing Starship via winget..."
    winget install --id Starship.Starship --accept-source-agreements --accept-package-agreements
    Write-Host "Starship installed." -ForegroundColor Green
    Write-Host "Restart your terminal to activate the Starship prompt." -ForegroundColor Yellow
} else {
    Write-Host "Starship is already installed." -ForegroundColor DarkGray
}
