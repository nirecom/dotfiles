# Test: install-obsolete.ps1 — old dotfiles directory cleanup after migration
# Verifies the Remove-MigratedSource logic used in install-obsolete.ps1
# (cleanup of ~/dotfiles, ~/my-private-repo after migration to C:\git\)

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

Describe "startup fetch list rename — fetch-repos -> fetch-repos.txt" {
    BeforeAll {
        $script:UninstallPs1 = Join-Path (Split-Path -Parent $PSScriptRoot) "install\win\uninstall-obsolete.ps1"
        $script:Block = (Get-Content $script:UninstallPs1 -Raw) -replace '(?s)^.*?(# --- BEGIN temporary: fetch-repos.*?# --- END temporary: fetch-repos.*?---).*$', '$1'

        # Run the shipped block verbatim except for $HOME, which is redirected at
        # the source level so the real ~/.config/dotfiles is never touched.
        function Invoke-FetchReposMigration {
            param([string]$FakeHome)
            $script = $script:Block -replace '\$HOME', "'$FakeHome'"
            Invoke-Expression $script | Out-Null
        }
    }

    BeforeEach {
        $script:FakeHome = Join-Path $TestDrive "home-$(Get-Random)"
        $script:ConfigDir = Join-Path $script:FakeHome ".config\dotfiles"
        New-Item -ItemType Directory -Path $script:ConfigDir -Force | Out-Null
        $script:Legacy = Join-Path $script:ConfigDir "fetch-repos"
        $script:Current = Join-Path $script:ConfigDir "fetch-repos.txt"
    }

    Context "A. Block extraction" {
        It "the migration block is present in uninstall-obsolete.ps1" {
            $script:Block | Should -Match 'BEGIN temporary: fetch-repos'
            $script:Block | Should -Match 'Move-Item'
        }
    }

    Context "B. Legacy list present, new name absent" {
        It "renames fetch-repos to fetch-repos.txt" {
            Set-Content -Path $script:Legacy -Value "repo-a"
            Invoke-FetchReposMigration -FakeHome $script:FakeHome
            Test-Path $script:Legacy | Should -BeFalse
            Test-Path $script:Current | Should -BeTrue
        }

        It "preserves the list content across the rename" {
            Set-Content -Path $script:Legacy -Value "repo-a"
            Invoke-FetchReposMigration -FakeHome $script:FakeHome
            (Get-Content $script:Current -Raw).Trim() | Should -Be "repo-a"
        }
    }

    Context "C. New name already exists" {
        It "does NOT overwrite an existing fetch-repos.txt" {
            Set-Content -Path $script:Legacy -Value "legacy"
            Set-Content -Path $script:Current -Value "current"
            Invoke-FetchReposMigration -FakeHome $script:FakeHome
            (Get-Content $script:Current -Raw).Trim() | Should -Be "current"
        }

        It "leaves the legacy file in place when the new name exists" {
            Set-Content -Path $script:Legacy -Value "legacy"
            Set-Content -Path $script:Current -Value "current"
            Invoke-FetchReposMigration -FakeHome $script:FakeHome
            Test-Path $script:Legacy | Should -BeTrue
        }
    }

    Context "D. Nothing to migrate" {
        It "is a no-op when neither file exists" {
            { Invoke-FetchReposMigration -FakeHome $script:FakeHome } | Should -Not -Throw
            Test-Path $script:Current | Should -BeFalse
        }

        It "is idempotent — a second run leaves the migrated list intact" {
            Set-Content -Path $script:Legacy -Value "repo-a"
            Invoke-FetchReposMigration -FakeHome $script:FakeHome
            Invoke-FetchReposMigration -FakeHome $script:FakeHome
            (Get-Content $script:Current -Raw).Trim() | Should -Be "repo-a"
            Test-Path $script:Legacy | Should -BeFalse
        }
    }
}
