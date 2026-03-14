# Tests for directory backup & symlink replacement logic in install/win/dotfileslink.ps1
# When a regular directory exists where a symlink should be, it should be backed up to .bak and replaced

Describe "dotfileslink directory backup and symlink replacement" {
    BeforeAll {
        # Detect symlink capability (same logic as dotfileslink.ps1)
        $regKey = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue
        $script:devMode = if ($regKey -and ($regKey.PSObject.Properties.Name -contains "AllowDevelopmentWithoutDevLicense")) {
            [bool]$regKey.AllowDevelopmentWithoutDevLicense
        } else { $false }
        $script:isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
        $script:canSymlink = $script:devMode -or $script:isAdmin

        # Extract the backup+symlink logic as a testable function
        function Invoke-DirSymlink($Source, $Dest) {
            if (-not (Test-Path $Source)) {
                return "skipped-no-source"
            }

            if (Test-Path $Dest -PathType Any) {
                $item = Get-Item $Dest -Force
                if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
                    if ($item.Target -eq $Source) {
                        return "already-linked"
                    }
                    Remove-Item $Dest -Force
                } else {
                    # Regular directory exists — back up and replace
                    $backup = "$Dest.bak"
                    if (Test-Path $backup) { Remove-Item -Recurse -Force $backup }
                    Rename-Item $Dest $backup
                }
            }

            New-Item -ItemType SymbolicLink -Path $Dest -Target $Source | Out-Null
            return "created"
        }
    }

    Context "Normal cases" {
        It "backs up regular directory and creates symlink" -Skip:(-not $script:canSymlink) {
            $root = Join-Path $TestDrive "test-backup"
            $source = Join-Path $root "source-dir"
            $dest = Join-Path $root "dest-dir"
            New-Item -ItemType Directory -Path $source -Force | Out-Null
            New-Item -ItemType Directory -Path $dest -Force | Out-Null
            # Put a file in dest to verify backup
            "test content" | Set-Content (Join-Path $dest "file.txt")

            $result = Invoke-DirSymlink -Source $source -Dest $dest
            $result | Should -Be "created"
            # dest is now a symlink
            (Get-Item $dest -Force).Attributes -band [IO.FileAttributes]::ReparsePoint | Should -BeTrue
            # backup exists with original content
            Test-Path "$dest.bak" | Should -BeTrue
            Get-Content (Join-Path "$dest.bak" "file.txt") | Should -Be "test content"
        }

        It "overwrites existing .bak when backing up" -Skip:(-not $script:canSymlink) {
            $root = Join-Path $TestDrive "test-overwrite-bak"
            $source = Join-Path $root "source-dir"
            $dest = Join-Path $root "dest-dir"
            $backup = "$dest.bak"
            New-Item -ItemType Directory -Path $source -Force | Out-Null
            New-Item -ItemType Directory -Path $dest -Force | Out-Null
            New-Item -ItemType Directory -Path $backup -Force | Out-Null
            "old backup" | Set-Content (Join-Path $backup "old.txt")
            "new content" | Set-Content (Join-Path $dest "new.txt")

            $result = Invoke-DirSymlink -Source $source -Dest $dest
            $result | Should -Be "created"
            # Old .bak content should be gone
            Test-Path (Join-Path $backup "old.txt") | Should -BeFalse
            # New backup content should be there
            Get-Content (Join-Path $backup "new.txt") | Should -Be "new content"
        }

        It "skips when already correctly symlinked" -Skip:(-not $script:canSymlink) {
            $root = Join-Path $TestDrive "test-already"
            $source = Join-Path $root "source-dir"
            $dest = Join-Path $root "dest-dir"
            New-Item -ItemType Directory -Path $source -Force | Out-Null
            New-Item -ItemType SymbolicLink -Path $dest -Target $source | Out-Null

            $result = Invoke-DirSymlink -Source $source -Dest $dest
            $result | Should -Be "already-linked"
        }
    }

    Context "Abnormal cases" {
        It "skips when source does not exist" {
            $root = Join-Path $TestDrive "test-no-source"
            $source = Join-Path $root "nonexistent"
            $dest = Join-Path $root "dest-dir"

            $result = Invoke-DirSymlink -Source $source -Dest $dest
            $result | Should -Be "skipped-no-source"
        }

        It "does not throw an error regardless of scenario" -Skip:(-not $script:canSymlink) {
            $root = Join-Path $TestDrive "test-safe"
            $source = Join-Path $root "source-dir"
            $dest = Join-Path $root "dest-dir"
            New-Item -ItemType Directory -Path $source -Force | Out-Null
            New-Item -ItemType Directory -Path $dest -Force | Out-Null

            { Invoke-DirSymlink -Source $source -Dest $dest } | Should -Not -Throw
        }
    }
}
