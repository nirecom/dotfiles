# Tests for PowerShell profile diverge detection (port of .profile_common logic)
Describe 'profile.ps1 force-push diverge detection' {
    BeforeAll {
        $root = Split-Path -Parent $PSScriptRoot
        $profile = Get-Content (Join-Path $root 'install\win\profile.ps1') -Raw
    }

    It 'checks merge-base --is-ancestor after ff-only fails' {
        $profile | Should -Match 'merge-base\s+--is-ancestor'
    }

    It 'checks .dotfiles-no-auto-reset marker file' {
        $profile | Should -Match '\.dotfiles-no-auto-reset'
    }

    It 'prompts user with [y/N]' {
        $profile | Should -Match '\[y/N\]'
    }

    It 'runs git reset --hard origin/main on confirmation' {
        $profile | Should -Match 'reset\s+--hard\s+origin/main'
    }

    It 'shows manual command when user declines' {
        $profile | Should -Match 'Skipped.*reset --hard'
    }

    It 'has timeout for user input' {
        $profile | Should -Match '10000|10\s*seconds'
    }
}
