# Test: install-obsolete.ps1 — old dotfiles directory cleanup after migration
# Verifies the Remove-MigratedSource logic used in install-obsolete.ps1
# (cleanup of ~/dotfiles, ~/dotfiles-private after migration to C:\git\)

BeforeAll {
    # Same logic as install-obsolete.ps1 migration cleanup section
    function Remove-MigratedSource {
        param([string]$OldPath, [string]$NewPath)
        if (-not (Test-Path $OldPath)) { return }
        if (-not (Test-Path $NewPath)) { return }
        Remove-Item $OldPath -Recurse -Force -ErrorAction SilentlyContinue
        if (-not (Test-Path $OldPath)) {
            Write-Host "Removed old migration source: $OldPath" -ForegroundColor DarkGray
        } else {
            Write-Warning "Could not fully remove: $OldPath (files may be in use)"
        }
    }
}

Describe "migration cleanup — Remove-MigratedSource" {
    BeforeEach {
        $script:FakeHome = Join-Path $env:TEMP "obsolete-test-home-$(Get-Random)"
        $script:FakeDest = Join-Path $env:TEMP "obsolete-test-dest-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:FakeHome -Force | Out-Null
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:FakeHome -ErrorAction SilentlyContinue
        Remove-Item -Recurse -Force $script:FakeDest -ErrorAction SilentlyContinue
    }

    It "removes old directory when migration target exists" {
        $old = Join-Path $script:FakeHome "dotfiles"
        New-Item -ItemType Directory -Path $old -Force | Out-Null
        Set-Content -Path (Join-Path $old "file.txt") -Value "old"
        $new = Join-Path $script:FakeDest "dotfiles"
        New-Item -ItemType Directory -Path $new -Force | Out-Null

        Remove-MigratedSource -OldPath $old -NewPath $new
        Test-Path $old | Should -BeFalse
    }

    It "does NOT remove old directory when migration target does NOT exist" {
        $old = Join-Path $script:FakeHome "dotfiles"
        New-Item -ItemType Directory -Path $old -Force | Out-Null
        Set-Content -Path (Join-Path $old "file.txt") -Value "important"
        $new = Join-Path $script:FakeDest "dotfiles"

        Remove-MigratedSource -OldPath $old -NewPath $new
        Test-Path $old | Should -BeTrue
    }

    It "removes old ~/git when C:\git exists" {
        $old = Join-Path $script:FakeHome "git"
        New-Item -ItemType Directory -Path $old -Force | Out-Null
        Set-Content -Path (Join-Path $old "leftover.txt") -Value "stale"
        $new = Join-Path $script:FakeDest "git"
        New-Item -ItemType Directory -Path $new -Force | Out-Null

        Remove-MigratedSource -OldPath $old -NewPath $new
        Test-Path $old | Should -BeFalse
    }

    It "is idempotent — no error when old directory already gone" {
        $old = Join-Path $script:FakeHome "dotfiles"
        $new = Join-Path $script:FakeDest "dotfiles"
        New-Item -ItemType Directory -Path $new -Force | Out-Null

        { Remove-MigratedSource -OldPath $old -NewPath $new } | Should -Not -Throw
    }
}
