# vscode.ps1 - Install Visual Studio Code and extensions
# Usage: Called by install.ps1 -Develop

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path))

# Install VS Code via winget
if (-not (Get-Command code -ErrorAction SilentlyContinue)) {
    if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
        Write-Warning "winget not found. Install VS Code manually: https://code.visualstudio.com/"
        return
    }
    Write-Host "Installing Visual Studio Code via winget..."
    winget install --id Microsoft.VisualStudioCode --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -ne 0) {
        Write-Warning "VS Code installation failed (exit code: $LASTEXITCODE). Re-run install.ps1 to retry."
        return
    }
    # Refresh PATH so code command is available
    $env:Path = [Environment]::GetEnvironmentVariable('Path', 'Machine') + ';' + [Environment]::GetEnvironmentVariable('Path', 'User')
    if (-not (Get-Command code -ErrorAction SilentlyContinue)) {
        Write-Warning "VS Code installed but 'code' command not found. Restart your terminal."
        return
    }
    Write-Host "Visual Studio Code installed." -ForegroundColor Green
} else {
    Write-Host "Visual Studio Code is already installed." -ForegroundColor DarkGray
}

# Install extensions
$extFile = Join-Path $DotfilesDir "config\vscode-extensions.txt"
if (-not (Test-Path $extFile)) {
    Write-Warning "Extension list not found: $extFile"
    return
}

$installedExts = (code --list-extensions 2>$null) -join "`n"
$extensions = Get-Content $extFile | Where-Object { $_ -match '\S' -and $_ -notmatch '^\s*#' }

foreach ($ext in $extensions) {
    $ext = $ext.Trim()
    if ($installedExts -match "(?i)^$([regex]::Escape($ext))$") {
        Write-Host "  Extension already installed: $ext" -ForegroundColor DarkGray
    } else {
        Write-Host "  Installing extension: $ext"
        code --install-extension $ext --force 2>$null
    }
}
Write-Host "VS Code extensions synced." -ForegroundColor Green
