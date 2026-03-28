# Test: migrate-repos.ps1
# Uses temp directories to simulate $HOME and C:\git

BeforeAll {
    $DotfilesDir = Split-Path -Parent $PSScriptRoot
    $MigrateScript = Join-Path (Join-Path $DotfilesDir "install") "win\migrate-repos.ps1"
}

Describe "migrate-repos.ps1" {
    BeforeEach {
        $script:FakeHome = Join-Path $env:TEMP "migrate-test-home-$(Get-Random)"
        $script:FakeDest = Join-Path $env:TEMP "migrate-test-dest-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:FakeHome -Force | Out-Null
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:FakeHome -ErrorAction SilentlyContinue
        Remove-Item -Recurse -Force $script:FakeDest -ErrorAction SilentlyContinue
    }

    It "moves ~/dotfiles to destination when it exists" {
        $src = Join-Path $script:FakeHome "dotfiles"
        New-Item -ItemType Directory -Path $src -Force | Out-Null
        Set-Content -Path (Join-Path $src "test.txt") -Value "data"

        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        Test-Path $src | Should -BeFalse
        Test-Path (Join-Path $script:FakeDest "dotfiles\test.txt") | Should -BeTrue
    }

    It "moves ~/dotfiles-private to destination when it exists" {
        $src = Join-Path $script:FakeHome "dotfiles-private"
        New-Item -ItemType Directory -Path $src -Force | Out-Null
        Set-Content -Path (Join-Path $src "secret.txt") -Value "private"

        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        Test-Path $src | Should -BeFalse
        Test-Path (Join-Path $script:FakeDest "dotfiles-private\secret.txt") | Should -BeTrue
    }

    It "skips ~/dotfiles when it does not exist" {
        # No dotfiles dir created — should not error
        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        # Script completes without error
        $true | Should -BeTrue
    }

    It "moves all subdirectories under ~/git/ preserving tree structure" {
        $gitDir = Join-Path $script:FakeHome "git"
        # Create nested structure: ~/git/ai-prompts/subdir/file.txt
        $nested = Join-Path $gitDir "ai-prompts\subdir"
        New-Item -ItemType Directory -Path $nested -Force | Out-Null
        Set-Content -Path (Join-Path $nested "file.txt") -Value "nested"
        # Create another: ~/git/tools/file.txt
        $tools = Join-Path $gitDir "tools"
        New-Item -ItemType Directory -Path $tools -Force | Out-Null
        Set-Content -Path (Join-Path $tools "file.txt") -Value "tool"

        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest

        Test-Path (Join-Path $script:FakeDest "ai-prompts\subdir\file.txt") | Should -BeTrue
        Test-Path (Join-Path $script:FakeDest "tools\file.txt") | Should -BeTrue
    }

    It "removes ~/git/ after all children are moved" {
        $gitDir = Join-Path $script:FakeHome "git"
        $child = Join-Path $gitDir "repo1"
        New-Item -ItemType Directory -Path $child -Force | Out-Null
        Set-Content -Path (Join-Path $child "f.txt") -Value "x"

        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        Test-Path $gitDir | Should -BeFalse
    }

    It "skips when destination already has same-named directory" {
        # Create source
        $src = Join-Path $script:FakeHome "dotfiles"
        New-Item -ItemType Directory -Path $src -Force | Out-Null
        Set-Content -Path (Join-Path $src "original.txt") -Value "src"
        # Create destination with same name (already migrated)
        $dst = Join-Path $script:FakeDest "dotfiles"
        New-Item -ItemType Directory -Path $dst -Force | Out-Null
        Set-Content -Path (Join-Path $dst "existing.txt") -Value "dst"

        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest 3>&1 | Out-Null
        # Source should remain (not deleted)
        Test-Path $src | Should -BeTrue
        # Destination should keep original content (not overwritten)
        Test-Path (Join-Path $dst "existing.txt") | Should -BeTrue
    }

    It "is idempotent - all sources already gone" {
        # Nothing to migrate — should not error
        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        $true | Should -BeTrue
    }

    It "handles partial migration - some moved, some remaining" {
        # dotfiles already at dest, dotfiles-private still at home
        New-Item -ItemType Directory -Path (Join-Path $script:FakeDest "dotfiles") -Force | Out-Null
        $priv = Join-Path $script:FakeHome "dotfiles-private"
        New-Item -ItemType Directory -Path $priv -Force | Out-Null
        Set-Content -Path (Join-Path $priv "data.txt") -Value "private"

        & $MigrateScript -HomeDir $script:FakeHome -DestDir $script:FakeDest
        # dotfiles-private should be moved
        Test-Path $priv | Should -BeFalse
        Test-Path (Join-Path $script:FakeDest "dotfiles-private\data.txt") | Should -BeTrue
    }
}
