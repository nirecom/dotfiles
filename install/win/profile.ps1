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
    $sshKey = "$HOME\.ssh\id_ed25519"
    if (-not (Test-Path $sshKey)) { $sshKey = "$HOME\.ssh\id_rsa" }
    if (Test-Path $sshKey) {
        ssh-add $sshKey 2>&1 | Out-Null
    }
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

# One-time migration: claude-code → claude-global (compat symlink + relink)
$oldClaude = Join-Path $DotfilesDir "claude-code"
$newClaude = Join-Path $DotfilesDir "claude-global"
if ((Test-Path $newClaude) -and -not ((Test-Path $oldClaude) -and (Get-Item $oldClaude -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    # Remove empty directory left by git (git doesn't clean up empty dirs after rename)
    if ((Test-Path $oldClaude) -and (Test-Path $oldClaude -PathType Container)) {
        Remove-Item $oldClaude -Recurse -Force -ErrorAction SilentlyContinue
    }
    New-Item -ItemType SymbolicLink -Path $oldClaude -Target $newClaude | Out-Null
}
$claudeSettings = "$HOME\.claude\settings.json"
if ((Test-Path $claudeSettings) -and ((Get-Item $claudeSettings -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    $target = (Get-Item $claudeSettings -Force).Target
    if ($target -like "*claude-code*") {
        Write-Host "Migrating Claude Code symlinks (claude-code -> claude-global)..."
        & "$DotfilesDir\install\win\dotfileslink.ps1"
    }
}

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
    } catch {
        Write-Warning "fnm: blocked by App Control policy — skipped"
    }
}
