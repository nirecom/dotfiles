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
$DotfilesDir = "C:\git\dotfiles"
$env:DOTFILES_DIR = $DotfilesDir
if ((Get-Command git -ErrorAction SilentlyContinue) -and (Test-Path "$DotfilesDir\.git")) {
    Write-Host "git fetch $DotfilesDir ..."
    $fetchProcess = Start-Process -FilePath git -ArgumentList "-C $DotfilesDir fetch" -NoNewWindow -PassThru
    if (-not $fetchProcess.WaitForExit(3000)) {
        $fetchProcess.Kill()
        Write-Warning "git fetch timed out after 3s — skipped"
    } elseif ($fetchProcess.ExitCode -eq 0) {
        git -C $DotfilesDir merge --ff-only FETCH_HEAD
    }
}

# Pull session sync on startup (from other PCs)
$SessionDir = "$HOME\.claude\projects"
if ((Test-Path "$SessionDir\.git") -and (Get-Command git -ErrorAction SilentlyContinue)) {
    Write-Host "git fetch Claude session sync ..."
    $fetchProc = Start-Process -FilePath git -ArgumentList "-C $SessionDir fetch" -NoNewWindow -PassThru
    if (-not $fetchProc.WaitForExit(3000)) {
        $fetchProc.Kill()
    } elseif ($fetchProc.ExitCode -eq 0) {
        git -C $SessionDir merge --ff-only FETCH_HEAD 2>$null
    }
}

# Repair broken symlinks (Windows atomic save replaces symlinks with regular files)
$symlinkFiles = @("$HOME\.bash_profile", "$HOME\.editorconfig", "$HOME\.claude\CLAUDE.md", "$HOME\.claude\settings.json")
$broken = $symlinkFiles | Where-Object { (Test-Path $_) -and -not ((Get-Item $_ -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) }
if ($broken) {
    Write-Host "Repairing $($broken.Count) broken symlink(s)..." -ForegroundColor Yellow
    & "$DotfilesDir\install\win\dotfileslink.ps1"
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

# Launch VS Code with session sync (push on window close via title polling)
function codes {
    $syncScript = "$DotfilesDir\bin\session-sync.ps1"
    $waitScript = "$DotfilesDir\bin\wait-vscode-window.ps1"
    $target = if ($args.Count -gt 0) { $args[0] } else { '.' }
    $codeArgs = $args -join ' '
    if ($target -match '\.code-workspace$') {
        $name = [IO.Path]::GetFileNameWithoutExtension((Resolve-Path $target).Path)
    } else {
        $name = Split-Path -Leaf (Resolve-Path $target).Path
    }
    Start-Process pwsh -ArgumentList "-NoProfile", "-WindowStyle", "Hidden", "-Command",
        "code.cmd --new-window $codeArgs; & '$waitScript' '$name'; & '$syncScript' push" -WindowStyle Hidden
}
