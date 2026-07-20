# gh.ps1 - Install GitHub CLI
# Usage: Called by install.ps1 -Base or run independently
# Sibling: agents/install/win/gh.ps1 (same pattern; kept separate for self-sufficiency)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (Get-Command gh -ErrorAction SilentlyContinue) {
    Write-Host "gh is already installed: $(gh --version | Select-Object -First 1)" -ForegroundColor DarkGray
    gh auth status 2>$null | Out-Null
    if ($LASTEXITCODE -eq 0) { exit 0 }
} else {
    Write-Host "Installing gh (GitHub CLI)..."
    winget install GitHub.cli --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -ne 0) {
        if (Get-Command gh -ErrorAction SilentlyContinue) {
            Write-Host "gh installed." -ForegroundColor Green
        } else {
            Write-Warning "gh installation failed (exit code $LASTEXITCODE). Re-run install.ps1 to retry."
            exit 1
        }
    } else {
        Write-Host "gh installed." -ForegroundColor Green
    }
}

Write-Host "gh: not authenticated — running gh auth login..." -ForegroundColor Yellow
gh auth login
