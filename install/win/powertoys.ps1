# powertoys.ps1 - Install PowerToys via winget and deploy Keyboard Manager settings
# Usage: Called by install.ps1 -Full

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path))

# --- Install PowerToys ---

if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    Write-Warning "winget not found. Install PowerToys manually: https://github.com/microsoft/PowerToys"
    return
}

winget list --id Microsoft.PowerToys 2>$null | Out-Null
$installed = $LASTEXITCODE -eq 0

if (-not $installed) {
    Write-Host "Installing PowerToys via winget..."
    winget install --id Microsoft.PowerToys --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -eq 0) {
        Write-Host "PowerToys installed." -ForegroundColor Green
    } else {
        Write-Warning "PowerToys installation failed (exit code: $LASTEXITCODE). Re-run install.ps1 to retry."
    }
} else {
    Write-Host "PowerToys is already installed." -ForegroundColor DarkGray
}

# --- Deploy Keyboard Manager settings ---

$sourceDir = Join-Path $DotfilesDir "config\win\powertoys\keyboard-manager"
$destDir = Join-Path $env:LOCALAPPDATA "Microsoft\PowerToys\Keyboard Manager"

if (-not (Test-Path $destDir)) {
    New-Item -ItemType Directory -Path $destDir -Force | Out-Null
}

$configFiles = @("default.json", "settings.json")
$needsRestart = $false

foreach ($file in $configFiles) {
    $source = Join-Path $sourceDir $file
    $dest = Join-Path $destDir $file

    if (-not (Test-Path $source)) {
        Write-Warning "Config source not found: $source (skipping)"
        continue
    }

    if (Test-Path $dest) {
        $sourceContent = Get-Content $source -Raw
        $destContent = Get-Content $dest -Raw
        if ($sourceContent -eq $destContent) {
            Write-Host "Keyboard Manager config already up to date: $file" -ForegroundColor DarkGray
            continue
        }
        # Back up existing config
        $backup = "$dest.bak"
        Copy-Item $dest $backup -Force
        Write-Host "Backed up: $dest -> $backup" -ForegroundColor Yellow
    }

    Copy-Item $source $dest -Force
    Write-Host "Deployed: $file" -ForegroundColor Green
    $needsRestart = $true
}

if ($needsRestart) {
    Write-Host "Restart PowerToys for Keyboard Manager settings to take effect." -ForegroundColor Yellow
}
