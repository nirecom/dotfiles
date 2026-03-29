# awscli.ps1 - Install AWS CLI via winget
# Usage: Called by install.ps1 -Develop

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install AWS CLI manually: https://aws.amazon.com/cli/"
    return
}
winget list --id Amazon.AWSCLI 2>$null | Out-Null
$awscliInstalled = $LASTEXITCODE -eq 0
if (-not $awscliInstalled) {
    Write-Host "Installing AWS CLI via winget..."
    winget install --id Amazon.AWSCLI --accept-source-agreements --accept-package-agreements
    Write-Host "AWS CLI installed." -ForegroundColor Green
    Write-Host "Restart your terminal to use the aws command." -ForegroundColor Yellow
} else {
    Write-Host "AWS CLI is already installed." -ForegroundColor DarkGray
}
