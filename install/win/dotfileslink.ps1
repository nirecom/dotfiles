# dotfileslink.ps1 - Create symlinks for Windows-relevant dotfiles
# Usage: Run in PowerShell from any directory
# Requires: Developer Mode enabled (or run as Administrator)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path))

# Check PowerShell version
if ($PSVersionTable.PSVersion.Major -le 5) {
    Write-Warning "PowerShell 5.x detected. Symlink creation may require Administrator privileges."
    Write-Warning "Recommend: Install PowerShell 7+ (https://aka.ms/powershell) or run as Administrator."
}

# Check Developer Mode / Admin for symlink capability
$regKey = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue
$devMode = if ($regKey -and ($regKey.PSObject.Properties.Name -contains "AllowDevelopmentWithoutDevLicense")) {
    $regKey.AllowDevelopmentWithoutDevLicense
} else {
    $false
}
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
$canSymlink = $devMode -or $isAdmin

if (-not $canSymlink) {
    Write-Warning "Cannot create symlinks: Developer Mode is not enabled and not running as Administrator."
    Write-Warning "Enable Developer Mode in Settings > For developers, or run as Administrator."
    exit 1
}

# Symlink targets: source (in repo) -> destination (in $HOME)
$links = @(
    @{ Source = ".bash_profile"; Dest = "$HOME\.bash_profile"; IsDir = $false }
    @{ Source = ".config\git"; Dest = "$HOME\.config\git"; IsDir = $true }
    @{ Source = ".editorconfig"; Dest = "$HOME\.editorconfig"; IsDir = $false }
    @{ Source = ".config\starship.toml"; Dest = "$HOME\.config\starship.toml"; IsDir = $false }
    @{ Source = ".config\starship-powershell.toml"; Dest = "$HOME\.config\starship-powershell.toml"; IsDir = $false }
    @{ Source = "claude-code\CLAUDE.md"; Dest = "$HOME\.claude\CLAUDE.md"; IsDir = $false }
    @{ Source = "claude-code\settings.json"; Dest = "$HOME\.claude\settings.json"; IsDir = $false }
    @{ Source = "claude-code\commands"; Dest = "$HOME\.claude\commands"; IsDir = $true }
)

# Private context directory (gitignored)
if (-not (Test-Path "$DotfilesDir\.context-private")) {
    New-Item -ItemType Directory -Path "$DotfilesDir\.context-private" -Force | Out-Null
}

# Ensure parent directories exist
if (-not (Test-Path "$HOME\.config")) {
    New-Item -ItemType Directory -Path "$HOME\.config" -Force | Out-Null
}

# Ensure ~/.claude directory exists
if (-not (Test-Path "$HOME\.claude")) {
    New-Item -ItemType Directory -Path "$HOME\.claude" -Force | Out-Null
}
if (Test-Path "$HOME\.claude\.git") {
    Write-Warning "~/.claude is a git repo (dotclaude). Remove .git dir to enable symlinks."
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

    if (-not $canSymlink) {
        Write-Warning "Cannot create symlink (no permission): $dest"
        continue
    }
    New-Item -ItemType SymbolicLink -Path $dest -Target $source | Out-Null
    Write-Host "Linked: $dest -> $source" -ForegroundColor Green
}

# Generate Windows-specific git config.local
$gitConfigLocal = Join-Path $DotfilesDir ".config\git\config.local"
if (-not (Test-Path $gitConfigLocal)) {
    @"
[core]
    sshCommand = C:/Windows/System32/OpenSSH/ssh.exe
"@ | Set-Content -Path $gitConfigLocal -Encoding UTF8
    Write-Host "Generated: $gitConfigLocal" -ForegroundColor Green
}

# Set execution policy for Windows PowerShell 5.x (defaults to Restricted)
# Write directly to the PS5 registry key since PS7's Set-ExecutionPolicy won't affect PS5
$ps5RegPath = "HKCU:\SOFTWARE\Microsoft\PowerShell\1\ShellIds\Microsoft.PowerShell"
$ps5RegItem = Get-ItemProperty -Path $ps5RegPath -ErrorAction SilentlyContinue
$ps5Policy = if ($ps5RegItem -and $ps5RegItem.PSObject.Properties.Name -contains 'ExecutionPolicy') { $ps5RegItem.ExecutionPolicy } else { $null }
if (-not $ps5Policy -or $ps5Policy -eq 'Restricted') {
    if (-not (Test-Path $ps5RegPath)) {
        New-Item -Path $ps5RegPath -Force | Out-Null
    }
    Set-ItemProperty -Path $ps5RegPath -Name ExecutionPolicy -Value "RemoteSigned"
    Write-Host "Set execution policy to RemoteSigned for Windows PowerShell (CurrentUser)" -ForegroundColor Green
}

# PowerShell profile symlinks
$profileSource = Join-Path $DotfilesDir "install\win\profile.ps1"
if (Test-Path $profileSource) {
    # Use MyDocuments to handle OneDrive redirection and non-English locale
    $myDocs = [Environment]::GetFolderPath('MyDocuments')
    $profileTargets = @()

    # PowerShell 5 (Windows PowerShell)
    if (Get-Command powershell.exe -ErrorAction SilentlyContinue) {
        $profileTargets += "$myDocs\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"
    }

    # PowerShell 7+ (pwsh)
    if (Get-Command pwsh -ErrorAction SilentlyContinue) {
        $profileTargets += "$myDocs\PowerShell\Microsoft.PowerShell_profile.ps1"
    }

    foreach ($dest in $profileTargets) {
        $parentDir = Split-Path $dest -Parent
        if (-not (Test-Path $parentDir)) {
            New-Item -ItemType Directory -Path $parentDir -Force | Out-Null
        }

        if (Test-Path $dest) {
            $item = Get-Item $dest -Force
            if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
                Write-Host "Already linked: $dest" -ForegroundColor DarkGray
                continue
            }
            Write-Warning "Exists (not a symlink): $dest (skipping - merge manually)"
            continue
        }

        if (-not $canSymlink) {
            Write-Warning "Cannot create symlink (no permission): $dest"
            continue
        }
        New-Item -ItemType SymbolicLink -Path $dest -Target $profileSource | Out-Null
        Write-Host "Linked: $dest -> $profileSource" -ForegroundColor Green
    }
}
