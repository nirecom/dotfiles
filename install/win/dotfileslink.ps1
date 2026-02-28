# dotfileslink.ps1 - Create symlinks for Windows-relevant dotfiles
# Usage: Run in PowerShell from any directory
# Requires: Developer Mode enabled (or run as Administrator)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = "$HOME\dotfiles"

# Check PowerShell version
if ($PSVersionTable.PSVersion.Major -le 5) {
    Write-Warning "PowerShell 5.x detected. Symlink creation may require Administrator privileges."
    Write-Warning "Recommend: Install PowerShell 7+ (https://aka.ms/powershell) or run as Administrator."
}

# Check Developer Mode
$devMode = (Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue).AllowDevelopmentWithoutDevLicense
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $devMode -and -not $isAdmin) {
    Write-Error "Developer Mode is not enabled and not running as Administrator. Enable Developer Mode (Settings > System > For developers) or run as Administrator."
    exit 1
}

# Symlink targets: source (in repo) -> destination (in $HOME)
$links = @(
    @{ Source = ".config\git"; Dest = "$HOME\.config\git"; IsDir = $true }
    @{ Source = ".editorconfig"; Dest = "$HOME\.editorconfig"; IsDir = $false }
)

# Ensure parent directories exist
if (-not (Test-Path "$HOME\.config")) {
    New-Item -ItemType Directory -Path "$HOME\.config" -Force | Out-Null
}

foreach ($link in $links) {
    $source = Join-Path $DotfilesDir $link.Source
    $dest = $link.Dest

    if (-not (Test-Path $source)) {
        Write-Warning "Source not found: $source (skipping)"
        continue
    }

    if (Test-Path $dest) {
        $item = Get-Item $dest -Force
        if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
            Write-Host "Already linked: $dest" -ForegroundColor DarkGray
            continue
        }
        Write-Warning "Exists (not a symlink): $dest (skipping)"
        continue
    }

    New-Item -ItemType SymbolicLink -Path $dest -Target $source | Out-Null
    Write-Host "Linked: $dest -> $source" -ForegroundColor Green
}

Write-Host ""
Write-Host "=== Done ===" -ForegroundColor Cyan
