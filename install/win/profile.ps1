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

# Ensure git uses Windows OpenSSH (not Git-bundled SSH) to work with ssh-agent
$winSsh = "C:/Windows/System32/OpenSSH/ssh.exe"
if (Test-Path $winSsh) {
    $currentSshCmd = git config --global core.sshCommand 2>$null
    if (-not $currentSshCmd -or $currentSshCmd -notmatch 'System32\\OpenSSH') {
        git config --global core.sshCommand $winSsh
        Write-Host "git core.sshCommand set to Windows OpenSSH"
    }
}

# Auto-pull dotfiles on startup
$DotfilesDir = "$HOME\dotfiles"
if ((Get-Command git -ErrorAction SilentlyContinue) -and (Test-Path "$DotfilesDir\.git")) {
    Write-Host "git pull $DotfilesDir ..."
    git -C $DotfilesDir pull
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
