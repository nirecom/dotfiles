# uv.ps1 - Install uv (Python package manager)
# Usage: Called by install.ps1 -Full or run independently

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (Get-Command uv -ErrorAction SilentlyContinue) {
    Write-Host "uv is already installed: $(uv --version)" -ForegroundColor DarkGray
} else {
    Write-Host "Installing uv..."
    irm https://astral.sh/uv/install.ps1 | iex
    Write-Host "uv installed: $(uv --version)" -ForegroundColor Green
}
