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

# Auto-pull dotfiles on startup.
# Honor $env:DOTFILES_DIR if set (e.g. by install/win/dotfileslink.ps1); fall back to C:\git\dotfiles.
if ($env:DOTFILES_DIR) {
    $DotfilesDir = $env:DOTFILES_DIR
} else {
    $DotfilesDir = "C:\git\dotfiles"
    $env:DOTFILES_DIR = $DotfilesDir
}
$PrivateDir = Join-Path (Split-Path -Parent $DotfilesDir) "dotfiles-private"
$AgentsDir = Join-Path (Split-Path -Parent $DotfilesDir) "agents"
if (Test-Path "$AgentsDir\profile-snippet.ps1") { . "$AgentsDir\profile-snippet.ps1" }
$SessionDir = "$HOME\.claude\projects"

if (Get-Command git -ErrorAction SilentlyContinue) {
    # Launch all fetches in parallel
    $fetchDf = $null
    if (Test-Path "$DotfilesDir\.git") {
        Write-Host "git fetch $DotfilesDir ..."
        $fetchDf = Start-Process -FilePath git -ArgumentList "-C $DotfilesDir fetch" -NoNewWindow -PassThru
    }
    $fetchPrv = $null
    if (Test-Path "$PrivateDir\.git") {
        Write-Host "git fetch dotfiles-private ..."
        $fetchPrv = Start-Process -FilePath git -ArgumentList "-C $PrivateDir fetch" -NoNewWindow -PassThru
    }
    $fetchAg = $null
    if (Test-Path "$AgentsDir\.git") {
        Write-Host "git fetch agents ..."
        $fetchAg = Start-Process -FilePath git -ArgumentList "-C $AgentsDir fetch" -NoNewWindow -PassThru
    }
    $fetchSs = $null
    if (Test-Path "$SessionDir\.git") {
        Write-Host "git fetch Claude session sync ..."
        $fetchSs = Start-Process -FilePath git -ArgumentList "-C $SessionDir fetch" -NoNewWindow -PassThru
    }

    # Wait for dotfiles fetch and merge
    if ($fetchDf) {
        if (-not $fetchDf.WaitForExit(3000)) {
            $fetchDf.Kill()
            Write-Warning "git fetch timed out after 3s — skipped"
        } elseif ($fetchDf.ExitCode -eq 0) {
            git -C $DotfilesDir merge --ff-only --no-summary FETCH_HEAD 2>$null
            if ($LASTEXITCODE -ne 0) {
                # ff-only failed — check if diverged (force push scenario)
                git -C $DotfilesDir merge-base --is-ancestor HEAD FETCH_HEAD 2>$null
                if ($LASTEXITCODE -ne 0) {
                    if (Test-Path "$HOME\.dotfiles-no-auto-reset") {
                        Write-Warning "dotfiles diverged from origin. Run: git -C $DotfilesDir reset --hard origin/main"
                    } elseif ([Environment]::UserInteractive) {
                        Write-Host "dotfiles diverged from origin (force push detected)."
                        Write-Host "Reset to origin/main? Local changes will be lost. [y/N]"
                        $ans = $null
                        $task = [System.Threading.Tasks.Task]::Run([Func[string]]{ [Console]::ReadLine() })
                        if ($task.Wait(10000)) { $ans = $task.Result }
                        if ($ans -match '^[Yy]$') {
                            git -C $DotfilesDir reset --hard origin/main
                        } else {
                            Write-Host "Skipped. Run manually: git -C $DotfilesDir reset --hard origin/main"
                        }
                    }
                }
            }
        }
    }

    # Wait for optional fetches and merge
    if ($fetchPrv) {
        if (-not $fetchPrv.WaitForExit(3000)) { $fetchPrv.Kill() }
        elseif ($fetchPrv.ExitCode -eq 0) { git -C $PrivateDir merge --ff-only --no-summary FETCH_HEAD 2>$null }
    }
    if ($fetchAg) {
        if (-not $fetchAg.WaitForExit(3000)) { $fetchAg.Kill() }
        elseif ($fetchAg.ExitCode -eq 0) { git -C $AgentsDir merge --ff-only --no-summary FETCH_HEAD 2>$null }
    }
    if ($fetchSs) {
        if (-not $fetchSs.WaitForExit(3000)) { $fetchSs.Kill() }
        elseif ($fetchSs.ExitCode -eq 0) { git -C $SessionDir merge --ff-only --no-summary FETCH_HEAD 2>$null }
    }
}

# Repair broken symlinks (Windows atomic save replaces symlinks with regular files)
$symlinkFiles = @("$HOME\.bash_profile", "$HOME\.editorconfig")
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
    $syncScript = "$AgentsDir\bin\session-sync.ps1"
    $waitScript = "$DotfilesDir\bin\wait-vscode-window.ps1"
    $target = if ($args.Count -gt 0) { $args[0] } else { '.' }
    $codeArgs = $args -join ' '
    if ($target -match '\.code-workspace$') {
        $name = [IO.Path]::GetFileNameWithoutExtension((Resolve-Path $target).Path)
    } else {
        $name = Split-Path -Leaf (Resolve-Path $target).Path
    }
    Start-Process pwsh -ArgumentList "-NoProfile", "-WindowStyle", "Hidden", "-Command",
        "code.cmd --new-window $codeArgs; & '$waitScript' '$name'; & '$syncScript' push -Quiet" -WindowStyle Hidden
}
