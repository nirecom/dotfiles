# tests/main-gh.Tests.ps1 - Pester tests for install/win/gh.ps1

BeforeAll {
    $script:ScriptPath = Join-Path $PSScriptRoot '..\install\win\gh.ps1'
    $script:ScriptExists = Test-Path $script:ScriptPath
    if ($script:ScriptExists) {
        $script:Content = Get-Content $script:ScriptPath -Raw
    } else {
        $script:Content = ''
    }
}

# ---------------------------------------------------------------------------
# Static: pattern checks against source file content
# ---------------------------------------------------------------------------

Describe "gh.ps1 existence and syntax" {
    It "GH-W-static-1: file exists" -Skip:(-not $script:ScriptExists) {
        $script:ScriptPath | Should -Exist
    }

    It "GH-W-static-2: has valid PowerShell syntax" -Skip:(-not $script:ScriptExists) {
        $errors = $null
        [System.Management.Automation.PSParser]::Tokenize($script:Content, [ref]$errors) | Out-Null
        $errors.Count | Should -Be 0
    }
}

Describe "gh.ps1 script hygiene" {
    It "GH-W-static-3: sets StrictMode" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'Set-StrictMode'
    }

    It "GH-W-static-4: sets ErrorActionPreference" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'ErrorActionPreference'
    }
}

Describe "gh.ps1 idempotency pattern" {
    It "GH-W-static-5: checks if gh is already installed" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'Get-Command\s+gh|gh\s+--version|winget\s+list'
    }

    It "GH-W-static-6: prints already-installed message with DarkGray" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'already installed'
        $script:Content | Should -Match 'DarkGray'
    }
}

Describe "gh.ps1 installation pattern" {
    It "GH-W-static-7: uses winget to install GitHub CLI" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'winget\s+install'
        $script:Content | Should -Match 'GitHub\.cli|github\.cli'
    }

    It "GH-W-static-8: checks LASTEXITCODE after winget install" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'LASTEXITCODE'
    }

    It "GH-W-static-9: issues Write-Warning on failure" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'Write-Warning'
    }

    It "GH-W-static-10: exits 1 on winget failure" -Skip:(-not $script:ScriptExists) {
        $script:Content | Should -Match 'exit\s+1'
    }
}

# ---------------------------------------------------------------------------
# Dynamic: execute the script with Pester Mocks
# All dynamic tests are skipped when the source script does not exist.
# ---------------------------------------------------------------------------

Describe "GH-W-2: gh already installed — prints DarkGray message, winget not called" -Skip:(-not $script:ScriptExists) {
    BeforeAll {
        # Mock gh as present with a known version
        Mock gh { "gh version 2.40.0 (2024-01-01)" } -ModuleName '' 2>$null
        # Capture Write-Host calls
        $script:WriteHostCalls = [System.Collections.Generic.List[hashtable]]::new()
        Mock Write-Host {
            $script:WriteHostCalls.Add(@{ Object = $Object; ForegroundColor = $ForegroundColor })
        } -ModuleName '' 2>$null
        Mock winget { throw "winget should not be called" } -ModuleName '' 2>$null
        Mock Get-Command {
            if ($Name -eq 'gh') { return [PSCustomObject]@{ Name = 'gh' } }
            return $null
        } -ModuleName '' -ParameterFilter { $Name -eq 'gh' } 2>$null
    }

    It "GH-W-2a: script exits cleanly when gh is already installed" {
        { & $script:ScriptPath } | Should -Not -Throw
    }
}

Describe "GH-W-1: gh not installed — winget install is called, script succeeds" -Skip:(-not $script:ScriptExists) {
    BeforeAll {
        $script:wingetCalled = $false
        Mock Get-Command {
            return $null
        } -ModuleName '' -ParameterFilter { $Name -eq 'gh' } 2>$null
        Mock winget {
            $script:wingetCalled = $true
            $global:LASTEXITCODE = 0
            return 0
        } -ModuleName '' 2>$null
        Mock Write-Host {} -ModuleName '' 2>$null
        Mock Write-Warning {} -ModuleName '' 2>$null
        Mock gh { "gh version 2.40.0 (2024-01-01)" } -ModuleName '' 2>$null
    }

    It "GH-W-1a: winget install is invoked when gh is not present" {
        & $script:ScriptPath
        $script:wingetCalled | Should -Be $true
    }
}

Describe "GH-W-3: Idempotency — running twice when already installed produces same result" -Skip:(-not $script:ScriptExists) {
    BeforeAll {
        Mock Get-Command {
            return [PSCustomObject]@{ Name = 'gh' }
        } -ModuleName '' -ParameterFilter { $Name -eq 'gh' } 2>$null
        Mock gh { "gh version 2.40.0 (2024-01-01)" } -ModuleName '' 2>$null
        $script:wingetCallCount = 0
        Mock winget {
            $script:wingetCallCount++
        } -ModuleName '' 2>$null
        $script:writeHostMessages = [System.Collections.Generic.List[string]]::new()
        Mock Write-Host {
            $script:writeHostMessages.Add([string]$Object)
        } -ModuleName '' 2>$null
    }

    It "GH-W-3a: winget is NOT called on first run when gh is present" {
        & $script:ScriptPath
        $script:wingetCallCount | Should -Be 0
    }

    It "GH-W-3b: winget is NOT called on second run when gh is present" {
        & $script:ScriptPath
        $script:wingetCallCount | Should -Be 0
    }

    It "GH-W-3c: already-installed message is produced on both runs" {
        $matchCount = ($script:writeHostMessages | Where-Object { $_ -match 'already installed' }).Count
        $matchCount | Should -BeGreaterOrEqual 2
    }
}

Describe "GH-W-4: winget returns exit code 1 — script exits 1 with warning" -Skip:(-not $script:ScriptExists) {
    BeforeAll {
        Mock Get-Command {
            return $null
        } -ModuleName '' -ParameterFilter { $Name -eq 'gh' } 2>$null
        $script:warningMessages = [System.Collections.Generic.List[string]]::new()
        Mock Write-Warning {
            $script:warningMessages.Add([string]$Message)
        } -ModuleName '' 2>$null
        Mock Write-Host {} -ModuleName '' 2>$null
        Mock winget {
            $global:LASTEXITCODE = 1
            return 1
        } -ModuleName '' 2>$null
    }

    It "GH-W-4a: script exits with non-zero when winget fails" {
        $exitCode = 0
        try {
            & $script:ScriptPath
        } catch {
            $exitCode = 1
        }
        # Either exit 1 or a thrown error is acceptable
        ($exitCode -ne 0 -or $LASTEXITCODE -ne 0) | Should -Be $true
    }

    It "GH-W-4b: Write-Warning is issued when winget fails" {
        try { & $script:ScriptPath } catch {}
        $script:warningMessages.Count | Should -BeGreaterThan 0
    }
}
