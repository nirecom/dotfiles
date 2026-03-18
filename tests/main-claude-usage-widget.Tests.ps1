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
            $ScriptContent | Should -Match 'if \(Test-Path \$ExePath\)'
        }

        It "uses silent installer flag /S" {
            $ScriptContent | Should -Match '"/S"'
        }

        It "configures autostart via registry Run key" {
            $ScriptContent | Should -Match "HKCU:\\Software\\Microsoft\\Windows\\CurrentVersion\\Run"
            $ScriptContent | Should -Match "electron\.app\.Claude-Usage-Widget"
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

        It "skips autostart when registry key already exists (idempotent)" {
            $ScriptContent | Should -Match "Autostart already configured"
        }

        It "uses Program Files for install directory" {
            $ScriptContent | Should -Match '\$env:ProgramFiles'
        }

        It "cleans up temp file after download" {
            $ScriptContent | Should -Match "Remove-Item \`$tmpFile"
        }
    }
}
