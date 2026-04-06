# fnm.ps1 - Install fnm (Fast Node Manager) via winget
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install fnm manually: https://github.com/Schniz/fnm"
    return
}
winget list --id Schniz.fnm 2>$null | Out-Null
$fnmInstalled = $LASTEXITCODE -eq 0
if (-not $fnmInstalled) {
    Write-Host "Installing fnm via winget..."
    winget install --id Schniz.fnm --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -eq 0) {
        Write-Host "fnm installed." -ForegroundColor Green
        Write-Host "Restart your terminal to activate fnm." -ForegroundColor Yellow
    } else {
        Write-Warning "fnm installation failed (exit code: $LASTEXITCODE). Re-run install.ps1 to retry."
    }
} else {
    Write-Host "fnm is already installed." -ForegroundColor DarkGray
}
