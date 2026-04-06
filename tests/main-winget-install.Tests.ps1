# Tests for winget install error checking and MSI mutex wait
Describe 'winget install scripts' {
    BeforeAll {
        $root = Split-Path -Parent $PSScriptRoot
        $winDir = Join-Path $root 'install\win'
        $wingetScripts = @(
            'autohotkey.ps1', 'awscli.ps1', 'fnm.ps1',
            'google-japanese-input.ps1', 'powertoys.ps1',
            'rize.ps1', 'starship.ps1', 'vscode.ps1'
        )
    }

    It 'all winget scripts check $LASTEXITCODE after winget install' {
        foreach ($script in $wingetScripts) {
            $path = Join-Path $winDir $script
            $content = Get-Content $path -Raw
            if ($content -match 'winget install') {
                $content | Should -Match 'LASTEXITCODE' -Because "$script should check exit code after winget install"
            }
        }
    }

    It 'no winget script prints success unconditionally after winget install' {
        foreach ($script in $wingetScripts) {
            $path = Join-Path $winDir $script
            $lines = Get-Content $path
            for ($i = 0; $i -lt $lines.Count; $i++) {
                if ($lines[$i] -match '^\s*winget install\b') {
                    # Next non-blank line should be if/else, not Write-Host success
                    $next = $lines[($i+1)..($i+3)] | Where-Object { $_ -match '\S' } | Select-Object -First 1
                    $next | Should -Match '^\s*if\b' -Because "$script line $($i+1): success message must be inside if block"
                }
            }
        }
    }

    It 'install.ps1 defines Wait-MsiMutex function' {
        $installPs1 = Join-Path $root 'install.ps1'
        $content = Get-Content $installPs1 -Raw
        $content | Should -Match 'function Wait-MsiMutex'
    }

    It 'install.ps1 calls Wait-MsiMutex before first winget-using script' {
        $installPs1 = Join-Path $root 'install.ps1'
        $content = Get-Content $installPs1 -Raw
        $content | Should -Match 'Wait-MsiMutex'
    }
}
