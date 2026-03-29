# Tests for the codes function in install/win/profile.ps1
# Verifies that codes uses Start-Process (not Start-Job) for terminal independence

BeforeAll {
    $DotfilesDir = Split-Path -Parent $PSScriptRoot
    $ProfileScript = Join-Path $DotfilesDir "install\win\profile.ps1"
    $script:ProfileContent = Get-Content $ProfileScript -Raw
}

Describe "codes function (profile.ps1)" {
    Context "Normal cases" {
        It "uses Start-Process (not Start-Job)" {
            $ProfileContent | Should -Match 'Start-Process'
            $ProfileContent | Should -Not -Match 'Start-Job'
        }

        It "uses -WindowStyle Hidden for background execution" {
            $ProfileContent | Should -Match '-WindowStyle\s+Hidden'
        }

        It "includes code.cmd --wait to await VS Code close" {
            $ProfileContent | Should -Match 'code\.cmd\s+--wait'
        }

        It "runs session-sync push after code.cmd" {
            # In the command string, code.cmd --wait must precede session-sync push
            $codesBlock = ($ProfileContent -split 'function codes')[1] -split 'function ' | Select-Object -First 1
            $codesBlock | Should -Match 'code\.cmd --wait.*;\s*&\s*''\$syncScript''\s*push' `
                -Because "session-sync push must follow code.cmd --wait in the command string"
        }
    }

    Context "Edge cases" {
        It "args are joined with space (handles empty args without breaking)" {
            # Verify the join pattern is used — empty args produce empty string, not error
            $ProfileContent | Should -Match '\$args\s+-join\s+'
        }

        It "args are passed into the command string" {
            $codesBlock = ($ProfileContent -split 'function codes')[1] -split 'function ' | Select-Object -First 1
            $codesBlock | Should -Match '\$codeArgs' -Because "codeArgs variable must be referenced in command"
        }
    }
}
