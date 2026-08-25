# Test: startup fetch list ~/.config/dotfiles/fetch-repos.txt — Windows side
# Tests: install/win/profile.ps1
# Tags: git-fetch, fetch-repos, fixture, pwsh-required, scope:win
# Mirrors the entry-resolution pipeline of profile.ps1; Context D pins the mirror
# to the shipped code so the two cannot drift apart silently.

BeforeAll {
    $script:DotfilesDir = Split-Path -Parent $PSScriptRoot
    $script:ProfilePs1  = Join-Path $script:DotfilesDir "install\win\profile.ps1"

    # Same pipeline as profile.ps1: skip comments/blanks, resolve relative entries
    # against the repo-holding directory, drop entries that are not cloned.
    function Resolve-FetchRepoList {
        param([string]$ListPath, [string]$RepoRoot)
        Get-Content $ListPath |
            Where-Object { $_ -notmatch '^\s*#' -and $_ -match '\S' } |
            ForEach-Object {
                $repo = $_.Trim()
                if (-not [System.IO.Path]::IsPathRooted($repo)) {
                    $repo = Join-Path $RepoRoot $repo
                }
                if (Test-Path "$repo\.git") { $repo }
            }
    }
}

Describe "startup fetch list — profile.ps1 entry resolution" {
    BeforeEach {
        $script:RepoRoot = Join-Path $TestDrive "git"
        New-Item -ItemType Directory -Path (Join-Path $script:RepoRoot "repo-a\.git") -Force | Out-Null
        New-Item -ItemType Directory -Path (Join-Path $script:RepoRoot "repo-b\.git") -Force | Out-Null
        New-Item -ItemType Directory -Path (Join-Path $script:RepoRoot "not-a-repo") -Force | Out-Null
        $script:AbsRepo = Join-Path $TestDrive "elsewhere\extra-repo"
        New-Item -ItemType Directory -Path (Join-Path $script:AbsRepo ".git") -Force | Out-Null
        $script:ListPath = Join-Path $TestDrive "fetch-repos.txt"
    }

    Context "A. Relative entries" {
        It "resolves a relative entry against the repo-holding directory" {
            Set-Content -Path $script:ListPath -Value @("repo-a")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 1
            $resolved[0] | Should -Be (Join-Path $script:RepoRoot "repo-a")
        }

        It "resolves every relative entry, preserving list order" {
            Set-Content -Path $script:ListPath -Value @("repo-a", "repo-b")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved[0] | Should -Be (Join-Path $script:RepoRoot "repo-a")
            $resolved[1] | Should -Be (Join-Path $script:RepoRoot "repo-b")
        }

        It "trims surrounding whitespace before resolving" {
            Set-Content -Path $script:ListPath -Value @("   repo-a   ")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved[0] | Should -Be (Join-Path $script:RepoRoot "repo-a")
        }
    }

    Context "B. Absolute entries" {
        It "uses a drive-rooted absolute entry as-is" {
            Set-Content -Path $script:ListPath -Value @($script:AbsRepo)
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 1
            $resolved[0] | Should -Be $script:AbsRepo
        }

        It "does not prepend the repo root to an absolute entry" {
            Set-Content -Path $script:ListPath -Value @($script:AbsRepo)
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved[0] | Should -Not -BeLike "$script:RepoRoot*"
        }
    }

    Context "C. Skipped entries" {
        It "skips comment lines and blank lines" {
            Set-Content -Path $script:ListPath -Value @("# comment", "", "   ", "repo-a")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 1
        }

        It "skips an indented comment line" {
            Set-Content -Path $script:ListPath -Value @("   # indented comment", "repo-a")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 1
        }

        It "silently skips an entry that is not cloned on this machine" {
            Set-Content -Path $script:ListPath -Value @("missing-repo", "repo-a")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 1
            $resolved[0] | Should -Be (Join-Path $script:RepoRoot "repo-a")
        }

        It "silently skips a directory that has no .git" {
            Set-Content -Path $script:ListPath -Value @("not-a-repo")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 0
        }

        It "yields nothing for a list that is only comments" {
            Set-Content -Path $script:ListPath -Value @("# nothing here")
            $resolved = @(Resolve-FetchRepoList -ListPath $script:ListPath -RepoRoot $script:RepoRoot)
            $resolved.Count | Should -Be 0
        }
    }

    Context "D. Mirror pinned to the shipped profile.ps1" {
        BeforeAll { $script:ProfileText = Get-Content $script:ProfilePs1 -Raw }

        It "reads the list from ~/.config/dotfiles/fetch-repos.txt" {
            $script:ProfileText | Should -Match '\.config\\dotfiles\\fetch-repos\.txt'
        }

        It "no longer references the legacy extension-less name" {
            $script:ProfileText | Should -Not -Match 'dotfiles\\fetch-repos(?!\.txt)'
        }

        It "derives the repo root from the parent of DotfilesDir" {
            $script:ProfileText | Should -Match 'Split-Path -Parent \$DotfilesDir'
        }

        It "joins relative entries onto the repo root" {
            $script:ProfileText | Should -Match 'IsPathRooted\(\$repo\)'
            $script:ProfileText | Should -Match 'Join-Path \$_repoRoot \$repo'
        }

        It "filters comments and blank lines like the mirror does" {
            $script:ProfileText | Should -Match '\$_ -notmatch'
            $script:ProfileText | Should -Match '\$_ -match'
        }

        It "requires a .git directory before fetching an entry" {
            $script:ProfileText | Should -Match 'Test-Path "\$repo\\\.git"'
        }
    }
}
