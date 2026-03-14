# Tests for claude-code -> claude-global migration logic in install/win/profile.ps1 (lines 32-47)
# Verifies that symlink creation is skipped when no Developer Mode / admin privilege

Describe "claude-code migration symlink permission check" {
    BeforeAll {
        # Detect current symlink capability (same logic as profile.ps1)
        $regKey = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue
        $script:devMode = if ($regKey -and ($regKey.PSObject.Properties.Name -contains "AllowDevelopmentWithoutDevLicense")) {
            [bool]$regKey.AllowDevelopmentWithoutDevLicense
        } else { $false }
        $script:isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
        $script:canSymlink = $script:devMode -or $script:isAdmin

        # Extract the migration logic as a testable function
        function Invoke-ClaudeMigration($DotfilesDir) {
            $oldClaude = Join-Path $DotfilesDir "claude-code"
            $newClaude = Join-Path $DotfilesDir "claude-global"
            if ((Test-Path $newClaude) -and -not ((Test-Path $oldClaude) -and (Get-Item $oldClaude -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
                if ((Test-Path $oldClaude) -and (Test-Path $oldClaude -PathType Container)) {
                    Remove-Item $oldClaude -Recurse -Force -ErrorAction SilentlyContinue
                }
                $regKey = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue
                $devMode = if ($regKey -and ($regKey.PSObject.Properties.Name -contains "AllowDevelopmentWithoutDevLicense")) { $regKey.AllowDevelopmentWithoutDevLicense } else { $false }
                $isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
                if ($devMode -or $isAdmin) {
                    try {
                        New-Item -ItemType SymbolicLink -Path $oldClaude -Target $newClaude -ErrorAction Stop | Out-Null
                        return "created"
                    } catch {
                        return "skipped-no-permission"
                    }
                }
                return "skipped-no-permission"
            }
            return "not-needed"
        }
    }

    Context "Normal cases" {
        It "creates symlink when claude-global exists and has permission" -Skip:(-not $script:canSymlink) {
            $fakeDotfiles = Join-Path $TestDrive "dotfiles-normal"
            New-Item -ItemType Directory -Path "$fakeDotfiles\claude-global" -Force | Out-Null
            $result = Invoke-ClaudeMigration $fakeDotfiles
            $result | Should -Be "created"
            $oldPath = Join-Path $fakeDotfiles "claude-code"
            (Get-Item $oldPath -Force).Attributes -band [IO.FileAttributes]::ReparsePoint | Should -BeTrue
        }

        It "skips when claude-code symlink already exists" -Skip:(-not $script:canSymlink) {
            $fakeDotfiles = Join-Path $TestDrive "dotfiles-already"
            New-Item -ItemType Directory -Path "$fakeDotfiles\claude-global" -Force | Out-Null
            New-Item -ItemType SymbolicLink -Path "$fakeDotfiles\claude-code" -Target "$fakeDotfiles\claude-global" | Out-Null
            $result = Invoke-ClaudeMigration $fakeDotfiles
            $result | Should -Be "not-needed"
        }

        It "removes leftover empty claude-code directory before creating symlink" -Skip:(-not $script:canSymlink) {
            $fakeDotfiles = Join-Path $TestDrive "dotfiles-leftover"
            New-Item -ItemType Directory -Path "$fakeDotfiles\claude-global" -Force | Out-Null
            New-Item -ItemType Directory -Path "$fakeDotfiles\claude-code" -Force | Out-Null
            $result = Invoke-ClaudeMigration $fakeDotfiles
            $result | Should -Be "created"
            # Old directory should be replaced by symlink, not a regular directory
            (Get-Item "$fakeDotfiles\claude-code" -Force).Attributes -band [IO.FileAttributes]::ReparsePoint | Should -BeTrue
        }
    }

    Context "Abnormal cases" {
        It "skips when claude-global does not exist" {
            $fakeDotfiles = Join-Path $TestDrive "dotfiles-noglobal"
            New-Item -ItemType Directory -Path $fakeDotfiles -Force | Out-Null
            $result = Invoke-ClaudeMigration $fakeDotfiles
            $result | Should -Be "not-needed"
        }

        It "does not throw an error regardless of permission level" {
            $fakeDotfiles = Join-Path $TestDrive "dotfiles-safe"
            New-Item -ItemType Directory -Path "$fakeDotfiles\claude-global" -Force | Out-Null
            { Invoke-ClaudeMigration $fakeDotfiles } | Should -Not -Throw
        }

        It "returns skipped-no-permission when no privilege (non-admin, no DevMode)" -Skip:($script:canSymlink) {
            $fakeDotfiles = Join-Path $TestDrive "dotfiles-noperm"
            New-Item -ItemType Directory -Path "$fakeDotfiles\claude-global" -Force | Out-Null
            $result = Invoke-ClaudeMigration $fakeDotfiles
            $result | Should -Be "skipped-no-permission"
        }
    }
}
