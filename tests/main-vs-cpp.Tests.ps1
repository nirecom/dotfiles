# tests/main-vs-cpp.Tests.ps1
# Pester tests for install/win/vs-cpp.ps1

BeforeAll {
    $script:ScriptPath = "$PSScriptRoot\..\install\win\vs-cpp.ps1"
    $script:Content = Get-Content $script:ScriptPath -Raw
}

Describe "vs-cpp.ps1 existence and syntax" {
    It "file exists" {
        $script:ScriptPath | Should -Exist
    }
    It "has valid PowerShell syntax" {
        $errors = $null
        [System.Management.Automation.PSParser]::Tokenize($script:Content, [ref]$errors) | Out-Null
        $errors.Count | Should -Be 0
    }
}

Describe "vs-cpp.ps1 script patterns" {
    It "sets StrictMode" {
        $script:Content | Should -Match 'Set-StrictMode'
    }
    It "sets ErrorActionPreference" {
        $script:Content | Should -Match 'ErrorActionPreference'
    }
}

Describe "vs-cpp.ps1 idempotency" {
    It "uses vswhere to detect existing VS installation" {
        $script:Content | Should -Match 'vswhere'
    }
    It "checks for NativeDesktop workload" {
        $script:Content | Should -Match 'NativeDesktop'
    }
    It "skips install when VS is already present" {
        $script:Content | Should -Match 'already installed'
    }
}

Describe "vs-cpp.ps1 installer details" {
    It "downloads vs_community.exe bootstrapper" {
        $script:Content | Should -Match 'vs_community\.exe'
    }
    It "uses --passive flag for unattended install" {
        $script:Content | Should -Match '--passive'
    }
    It "uses --norestart flag" {
        $script:Content | Should -Match '--norestart'
    }
    It "handles admin elevation" {
        $script:Content | Should -Match 'RunAs'
    }
}
