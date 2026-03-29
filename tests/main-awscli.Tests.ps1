# tests/main-awscli.Tests.ps1
# Pester tests for install/win/awscli.ps1

BeforeAll {
    $script:ScriptPath = "$PSScriptRoot\..\install\win\awscli.ps1"
    $script:Content = Get-Content $script:ScriptPath -Raw
}

Describe "awscli.ps1 existence and syntax" {
    It "file exists" {
        $script:ScriptPath | Should -Exist
    }
    It "has valid PowerShell syntax" {
        $errors = $null
        [System.Management.Automation.PSParser]::Tokenize($script:Content, [ref]$errors) | Out-Null
        $errors.Count | Should -Be 0
    }
}

Describe "awscli.ps1 script patterns" {
    It "sets StrictMode" {
        $script:Content | Should -Match 'Set-StrictMode'
    }
    It "sets ErrorActionPreference" {
        $script:Content | Should -Match 'ErrorActionPreference'
    }
}

Describe "awscli.ps1 winget prerequisite check" {
    It "checks for winget availability" {
        $script:Content | Should -Match 'Get-Command\s+winget'
    }
    It "shows warning when winget is missing" {
        $script:Content | Should -Match 'Write-Warning.*winget not found'
    }
}

Describe "awscli.ps1 idempotency" {
    It "checks if AWS CLI is already installed via winget list" {
        $script:Content | Should -Match 'winget\s+list\s+--id\s+Amazon\.AWSCLI'
    }
    It "skips install when already present" {
        $script:Content | Should -Match 'already installed'
    }
}

Describe "awscli.ps1 installation" {
    It "uses correct winget package ID" {
        $script:Content | Should -Match 'winget\s+install\s+--id\s+Amazon\.AWSCLI'
    }
    It "accepts source agreements" {
        $script:Content | Should -Match '--accept-source-agreements'
    }
    It "accepts package agreements" {
        $script:Content | Should -Match '--accept-package-agreements'
    }
}
