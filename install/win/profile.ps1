# PowerShell profile managed by ~/dotfiles
# Symlinked by install/win/dotfileslink.ps1

# Initialize Starship prompt (if installed)
if (Get-Command starship -ErrorAction SilentlyContinue) {
    $ENV:STARSHIP_CONFIG = "$HOME\.config\starship-powershell.toml"
    Invoke-Expression (&starship init powershell)
}
