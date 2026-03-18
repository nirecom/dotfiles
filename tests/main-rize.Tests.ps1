# Tests for install/win/rize.ps1
# Validates script existence, syntax, and winget-based installation logic

Describe "rize install script (Windows)" {
    BeforeAll {
        $ScriptPath = Join-Path $PSScriptRoot "..\install\win\rize.ps1"
    }

    Context "Normal cases" {
        It "script file exists" {
            Test-Path $ScriptPath | Should -Be $true
        }

        It "script has valid PowerShell syntax" {
            $errors = $null
            [System.Management.Automation.PSParser]::Tokenize(
                (Get-Content $ScriptPath -Raw), [ref]$errors
            ) | Out-Null
            $errors.Count | Should -Be 0
        }

        It "uses winget for installation" {
            $content = Get-Content $ScriptPath -Raw
            $content | Should -Match "winget"
        }

        It "checks if already installed before installing" {
            $content = Get-Content $ScriptPath -Raw
            $content | Should -Match "winget list"
        }

        It "uses MS Store ID XPDC57FTXPM1P5 for install" {
            $content = Get-Content $ScriptPath -Raw
            $content | Should -Match "XPDC57FTXPM1P5"
        }

        It "checks installed status by name (not ID)" {
            $content = Get-Content $ScriptPath -Raw
            $content | Should -Match 'winget list --name Rize'
        }
    }

    Context "Edge cases" {
        It "handles missing winget gracefully" {
            $content = Get-Content $ScriptPath -Raw
            $content | Should -Match "Get-Command winget"
        }
    }
}
