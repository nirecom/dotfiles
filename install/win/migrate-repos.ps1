# migrate-repos.ps1 - Migrate git repos from user-specific paths to C:\git\
# Usage: Called by install.ps1 (one-time migration)
#
# Moves:
#   ~/dotfiles         → C:\git\dotfiles        (if exists)
#   ~/dotfiles-private → C:\git\dotfiles-private (if exists)
#   ~/git/*            → C:\git\*               (all subdirectories, tree preserved)
#
# Skips if destination already exists. Idempotent.

param(
    [string]$HomeDir = $env:USERPROFILE,
    [string]$DestDir = "C:\git"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Move-RepoIfNeeded {
    param(
        [string]$Source,
        [string]$Destination
    )
    if (-not (Test-Path $Source)) { return }

    $name = Split-Path -Leaf $Source
    $target = Join-Path $Destination $name

    if (Test-Path $target) {
        Write-Warning "Skipped: $target already exists"
        return
    }

    if (-not (Test-Path $Destination)) {
        New-Item -ItemType Directory -Path $Destination -Force | Out-Null
    }

    # Unlock current directory if we're moving the directory we're running from
    $resolved = (Resolve-Path $Source).Path
    $cwd = (Get-Location).Path
    $pushedLocation = $false
    if ($cwd -eq $resolved -or $cwd.StartsWith("$resolved\")) {
        Push-Location $env:TEMP
        $pushedLocation = $true
    }

    Move-Item -Path $Source -Destination $target
    Write-Host "Moved: $Source -> $target" -ForegroundColor Green

    if ($pushedLocation) { Set-Location $target }
}

# Move ~/dotfiles and ~/dotfiles-private
Move-RepoIfNeeded -Source (Join-Path $HomeDir "dotfiles") -Destination $DestDir
Move-RepoIfNeeded -Source (Join-Path $HomeDir "dotfiles-private") -Destination $DestDir

# Move all subdirectories under ~/git/
$gitDir = Join-Path $HomeDir "git"
if (Test-Path $gitDir) {
    Get-ChildItem -Path $gitDir -Directory | ForEach-Object {
        Move-RepoIfNeeded -Source $_.FullName -Destination $DestDir
    }

    # Remove ~/git/ if empty
    if (-not (Get-ChildItem $gitDir -ErrorAction SilentlyContinue)) {
        Remove-Item $gitDir
        Write-Host "Removed empty: $gitDir" -ForegroundColor DarkGray
    }
}
