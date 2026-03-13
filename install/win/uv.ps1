# uv.ps1 - Install uv (Python package manager)
# Usage: Called by install.ps1 -Full or run independently

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (Get-Command uv -ErrorAction SilentlyContinue) {
    Write-Host "uv is already installed: $(uv --version)" -ForegroundColor DarkGray
} else {
    Write-Host "Installing uv..."
    irm https://astral.sh/uv/install.ps1 | iex
    # Refresh PATH so the newly installed uv is found
    $uvBin = Join-Path $HOME ".local\bin"
    if ($env:Path -notlike "*$uvBin*") {
        $env:Path = "$uvBin;$env:Path"
    }
    Write-Host "uv installed: $(uv --version)" -ForegroundColor Green
}
