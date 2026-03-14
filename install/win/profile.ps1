# PowerShell profile managed by ~/dotfiles
# Symlinked by install/win/dotfileslink.ps1

# Start ssh-agent and load keys if not already loaded
$sshAgent = Get-Service ssh-agent -ErrorAction SilentlyContinue
if ($sshAgent -and $sshAgent.StartType -ne 'Automatic') {
    Write-Warning "ssh-agent is not set to auto-start. Run as admin: Set-Service -Name ssh-agent -StartupType Automatic"
}
if ($sshAgent -and $sshAgent.Status -ne 'Running') {
    Start-Service ssh-agent -ErrorAction SilentlyContinue
}
$loadedKeys = ssh-add -l 2>&1
if ($loadedKeys -match 'no identities|agent.*not running|error') {
    Get-ChildItem "$HOME\.ssh\id_*" -File -ErrorAction SilentlyContinue |
        Where-Object { $_.Extension -ne '.pub' } |
        ForEach-Object { ssh-add $_.FullName 2>&1 | Out-Null }
}

# Auto-pull dotfiles on startup
$DotfilesDir = "$HOME\dotfiles"
if ((Get-Command git -ErrorAction SilentlyContinue) -and (Test-Path "$DotfilesDir\.git")) {
    Write-Host "git fetch $DotfilesDir ..."
    $fetchProcess = Start-Process -FilePath git -ArgumentList "-C $DotfilesDir fetch" -NoNewWindow -PassThru
    if (-not $fetchProcess.WaitForExit(3000)) {
        $fetchProcess.Kill()
        Write-Warning "git fetch timed out after 3s — skipped"
    } else {
        git -C $DotfilesDir merge --ff-only FETCH_HEAD 2>&1 | Out-Null
    }
}

# --- BEGIN temporary: claude-code → claude-global migration ---
$oldClaude = Join-Path $DotfilesDir "claude-code"
$newClaude = Join-Path $DotfilesDir "claude-global"
if ((Test-Path $newClaude) -and -not ((Test-Path $oldClaude) -and (Get-Item $oldClaude -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    # Remove empty directory left by git (git doesn't clean up empty dirs after rename)
    if ((Test-Path $oldClaude) -and (Test-Path $oldClaude -PathType Container)) {
        Remove-Item $oldClaude -Recurse -Force -ErrorAction SilentlyContinue
    }
    # Check symlink capability (Developer Mode or Administrator)
    $regKey = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue
    $devMode = if ($regKey -and ($regKey.PSObject.Properties.Name -contains "AllowDevelopmentWithoutDevLicense")) { $regKey.AllowDevelopmentWithoutDevLicense } else { $false }
    $isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
    if ($devMode -or $isAdmin) {
        try {
            New-Item -ItemType SymbolicLink -Path $oldClaude -Target $newClaude -ErrorAction Stop | Out-Null
        } catch {
            # PS5 requires admin even with Developer Mode; silently skip
        }
    }
}
$claudeSettings = "$HOME\.claude\settings.json"
if ((Test-Path $claudeSettings) -and ((Get-Item $claudeSettings -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    $target = (Get-Item $claudeSettings -Force).Target
    if ($target -like "*claude-code*") {
        Write-Host "Migrating Claude Code symlinks (claude-code -> claude-global)..."
        & "$DotfilesDir\install\win\dotfileslink.ps1"
    }
}
# --- END temporary: claude-code → claude-global migration ---

# --- BEGIN temporary: commands → skills migration ---
$oldSkills = "$HOME\.claude\commands"
$newSkills = "$HOME\.claude\skills"
if ((Test-Path $oldSkills) -and -not (Test-Path $newSkills)) {
    Write-Host "Migrating Claude Code symlinks (commands -> skills)..."
    Remove-Item $oldSkills -Force -ErrorAction SilentlyContinue
    & "$DotfilesDir\install\win\dotfileslink.ps1"
}
# --- END temporary: commands → skills migration ---

# Add ~/.local/bin to PATH (used by Claude Code and other user-installed tools)
$localBin = Join-Path $HOME ".local\bin"
if ((Test-Path $localBin) -and ($env:PATH -notlike "*$localBin*")) {
    $env:PATH = "$localBin;$env:PATH"
}

# Initialize Starship prompt (if installed)
if (Get-Command starship -ErrorAction SilentlyContinue) {
    $ENV:STARSHIP_CONFIG = "$HOME\.config\starship-powershell.toml"
    Invoke-Expression (&starship init powershell)
}

# Initialize fnm (Fast Node Manager) if installed
if (Get-Command fnm -ErrorAction SilentlyContinue) {
    try {
        fnm env --use-on-cd --shell powershell | Out-String | Invoke-Expression
        # Override fnm's cd wrapper to produce clean error messages
        function global:Set-LocationWithFnm {
            param($path)
            if ($null -eq $path) {
                Set-Location $HOME
            } elseif (Test-Path -LiteralPath $path) {
                Set-Location -LiteralPath $path
            } else {
                Write-Error "cd: no such directory: $path" -ErrorAction Continue
                return
            }
            Set-FnmOnLoad
        }
    } catch {
        Write-Warning "fnm: blocked by App Control policy — skipped"
    }
}
