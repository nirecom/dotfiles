# Tests for install/win/powertoys.ps1
# Validates script existence, syntax, winget installation, and Keyboard Manager config deployment

Describe "PowerToys install script (Windows)" {
    BeforeAll {
        $ScriptPath = Join-Path $PSScriptRoot "..\install\win\powertoys.ps1"
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

        It "checks winget availability" {
            $ScriptContent | Should -Match "Get-Command winget"
        }

        It "checks if PowerToys is already installed via winget list" {
            $ScriptContent | Should -Match "winget list --id Microsoft\.PowerToys"
        }

        It "installs PowerToys via winget when not installed" {
            $ScriptContent | Should -Match "winget install --id Microsoft\.PowerToys"
        }

        It "deploys Keyboard Manager config files" {
            $ScriptContent | Should -Match "Keyboard Manager"
            $ScriptContent | Should -Match "default\.json"
        }
    }

    Context "Error cases" {
        It "warns and returns when winget is not found" {
            $ScriptContent | Should -Match "winget not found"
        }
    }

    Context "Edge cases" {
        It "skips install when PowerToys is already installed (idempotent)" {
            $ScriptContent | Should -Match "already installed"
        }

        It "skips config deploy when files are identical (idempotent)" {
            $ScriptContent | Should -Match "already up to date|identical|up-to-date"
        }

        It "backs up existing config before overwriting" {
            $ScriptContent | Should -Match "\.bak"
        }

        It "notifies user to restart PowerToys for settings to take effect" {
            $ScriptContent | Should -Match "[Rr]estart"
        }
    }
}

Describe "PowerToys Keyboard Manager config files" {
    BeforeAll {
        $ConfigDir = Join-Path $PSScriptRoot "..\config\win\powertoys\keyboard-manager"
    }

    Context "Normal cases" {
        It "default.json exists" {
            Test-Path (Join-Path $ConfigDir "default.json") | Should -Be $true
        }

        It "settings.json exists" {
            Test-Path (Join-Path $ConfigDir "settings.json") | Should -Be $true
        }

        It "default.json contains valid JSON" {
            $json = Get-Content (Join-Path $ConfigDir "default.json") -Raw
            { $json | ConvertFrom-Json } | Should -Not -Throw
        }

        It "settings.json contains valid JSON" {
            $json = Get-Content (Join-Path $ConfigDir "settings.json") -Raw
            { $json | ConvertFrom-Json } | Should -Not -Throw
        }

        It "default.json contains remapShortcuts section" {
            $json = Get-Content (Join-Path $ConfigDir "default.json") -Raw | ConvertFrom-Json
            $json.remapShortcuts | Should -Not -BeNullOrEmpty
        }

        It "default.json has global shortcut remappings" {
            $json = Get-Content (Join-Path $ConfigDir "default.json") -Raw | ConvertFrom-Json
            $json.remapShortcuts.global.Count | Should -BeGreaterThan 0
        }
    }

    Context "Edge cases" {
        It "default.json has no app-specific remappings (global only)" {
            $json = Get-Content (Join-Path $ConfigDir "default.json") -Raw | ConvertFrom-Json
            $json.remapShortcuts.appSpecific.Count | Should -Be 0
        }

        It "default.json has no key remappings (shortcuts only)" {
            $json = Get-Content (Join-Path $ConfigDir "default.json") -Raw | ConvertFrom-Json
            $json.remapKeys.inProcess.Count | Should -Be 0
        }
    }
}
