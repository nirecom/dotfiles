# PowerShell profile managed by ~/dotfiles
# Symlinked by install/win/dotfileslink.ps1

# Auto-pull dotfiles on startup
$DotfilesDir = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent (Resolve-Path $MyInvocation.MyCommand.Path)))
if ((Get-Command git -ErrorAction SilentlyContinue) -and (Test-Path $DotfilesDir)) {
    Write-Host "git pull $DotfilesDir ..."
    git -C $DotfilesDir pull
}

# Initialize Starship prompt (if installed)
if (Get-Command starship -ErrorAction SilentlyContinue) {
    $ENV:STARSHIP_CONFIG = "$HOME\.config\starship-powershell.toml"
    Invoke-Expression (&starship init powershell)
}
