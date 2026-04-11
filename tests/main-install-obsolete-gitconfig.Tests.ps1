# Test: install-obsolete.ps1 — ~/.gitconfig deletion logic
# Verifies the Remove-GitconfigIfRegularFile logic used in install-obsolete.ps1
# (deletes ~/.gitconfig if it is a regular file, not a symlink, and user confirms)

BeforeAll {
    # Same logic as install-obsolete.ps1 gitconfig section, with $Response param
    # to avoid interactive Read-Host
    function Remove-GitconfigIfRegularFile {
        param([string]$GitconfigPath, [string]$Response)
        if ((Test-Path $GitconfigPath) -and -not ((Get-Item $GitconfigPath -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
            if ($Response -eq 'y' -or $Response -eq 'Y') {
                Remove-Item $GitconfigPath
            }
        }
    }
}

Describe "gitconfig cleanup — Remove-GitconfigIfRegularFile" {
    BeforeEach {
        $script:TempDir = Join-Path $env:TEMP "gitconfig-test-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TempDir -Force | Out-Null
        $script:GitconfigPath = Join-Path $script:TempDir ".gitconfig"
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TempDir -ErrorAction SilentlyContinue
    }

    It "deletes regular file when user answers 'y'" {
        Set-Content -Path $script:GitconfigPath -Value "[user]`n    name = Test"

        Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'y'

        Test-Path $script:GitconfigPath | Should -BeFalse
    }

    It "deletes regular file when user answers 'Y' (uppercase)" {
        Set-Content -Path $script:GitconfigPath -Value "[user]`n    name = Test"

        Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'Y'

        Test-Path $script:GitconfigPath | Should -BeFalse
    }

    It "keeps regular file when user answers 'N'" {
        Set-Content -Path $script:GitconfigPath -Value "[user]`n    name = Test"

        Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'N'

        Test-Path $script:GitconfigPath | Should -BeTrue
    }

    It "keeps regular file when user answers empty string (default no)" {
        Set-Content -Path $script:GitconfigPath -Value "[user]`n    name = Test"

        Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response ''

        Test-Path $script:GitconfigPath | Should -BeTrue
    }

    It "does not error when file does not exist" {
        { Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'y' } | Should -Not -Throw
    }

    It "skips deletion when path is a symlink" {
        $targetFile = Join-Path $script:TempDir ".gitconfig-real"
        Set-Content -Path $targetFile -Value "[user]`n    name = Real"

        try {
            New-Item -ItemType SymbolicLink -Path $script:GitconfigPath -Target $targetFile -ErrorAction Stop | Out-Null
        } catch {
            Set-ItResult -Skipped -Because "symlink creation failed (no Dev Mode or admin): $_"
            return
        }

        Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'y'

        # Symlink itself should still exist (not deleted)
        Test-Path $script:GitconfigPath | Should -BeTrue
        # Target should still exist
        Test-Path $targetFile | Should -BeTrue
    }

    It "is idempotent — no error when file was already deleted, run again" {
        Set-Content -Path $script:GitconfigPath -Value "[user]`n    name = Test"

        Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'y'
        { Remove-GitconfigIfRegularFile -GitconfigPath $script:GitconfigPath -Response 'y' } | Should -Not -Throw
        Test-Path $script:GitconfigPath | Should -BeFalse
    }
}
