# Tests for install/win/claude-usage-widget.ps1
# Validates script existence, syntax, and GitHub release-based installation logic

Describe "claude-usage-widget install script (Windows)" {
    BeforeAll {
        $ScriptPath = Join-Path $PSScriptRoot "..\install\win\claude-usage-widget.ps1"
        $ScriptContent = Get-Content $ScriptPath -Raw
    }

    Context "Normal cases" {
        It "script file exists" {
            Test-Path $ScriptPath | Should -Be $true
        }

        It "script has valid PowerShell syntax" {
            $errors = $null
            [System.Management.Automation.PSParser]::Tokenize(
                $ScriptContent, [ref]$errors
            ) | Out-Null
            $errors.Count | Should -Be 0
        }

        It "fetches latest release from GitHub API" {
            $ScriptContent | Should -Match "Invoke-RestMethod"
            $ScriptContent | Should -Match "github\.com.*SlavomirDurej/claude-usage-widget/releases/latest"
        }

        It "filters for win-Setup.exe asset" {
            $ScriptContent | Should -Match 'win-Setup\\\.exe'
        }

        It "checks if already installed before downloading" {
            $ScriptContent | Should -Match 'if \(\$ExePath\)'
        }

        It "uses silent installer flag /S" {
            $ScriptContent | Should -Match '"/S"'
        }

        It "waits for installer child process to complete" {
            $ScriptContent | Should -Match 'Start-Sleep'
            $ScriptContent | Should -Match 'Test-Path'
        }

    }

    Context "Error cases" {
        It "warns and returns when no matching asset found in release" {
            $ScriptContent | Should -Match 'if \(-not \$setupAsset\)'
            $ScriptContent | Should -Match "Could not find Windows Setup exe"
        }

        It "warns and returns when exe not found after install" {
            $ScriptContent | Should -Match "install may have failed"
        }

        It "suppresses error on temp file cleanup failure" {
            $ScriptContent | Should -Match "Remove-Item.*-ErrorAction SilentlyContinue"
        }
    }

    Context "Edge cases" {
        It "skips download when already installed (idempotent)" {
            $ScriptContent | Should -Match "already installed"
        }


        It "checks both per-user and per-machine install paths" {
            $ScriptContent | Should -Match '\$env:LOCALAPPDATA'
            $ScriptContent | Should -Match '\$env:ProgramFiles'
        }


        It "cleans up temp file after download" {
            $ScriptContent | Should -Match "Remove-Item \`$tmpFile"
        }
    }
}
