# Tests for symlink repair logic in dotfileslink.ps1 and profile.ps1
# When a regular file exists where a symlink should be (broken by atomic save),
# it should be backed up to .bak and replaced with a symlink

# Detect symlink capability at script level (Pester 5 evaluates -Skip during discovery, before BeforeAll)
$regKey = Get-ItemProperty HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock -ErrorAction SilentlyContinue
$canSymlink = if ($regKey -and ($regKey.PSObject.Properties.Name -contains "AllowDevelopmentWithoutDevLicense")) {
    [bool]$regKey.AllowDevelopmentWithoutDevLicense
} else { $false }
if (-not $canSymlink) {
    $canSymlink = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

Describe "dotfileslink file backup and symlink replacement" {
    BeforeAll {
        # Extract the backup+symlink logic as a testable function
        # This mirrors the logic in dotfileslink.ps1 (after the fix)
        function Invoke-FileSymlink($Source, $Dest) {
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
                    # Regular file/directory exists — back up and replace
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
        It "backs up regular file and creates symlink" -Skip:(-not $canSymlink) {
            $root = Join-Path $TestDrive "test-file-backup"
            New-Item -ItemType Directory -Path $root -Force | Out-Null
            $source = Join-Path $root "source.txt"
            $dest = Join-Path $root "dest.txt"
            "source content" | Set-Content $source
            "dest content (broken symlink)" | Set-Content $dest

            $result = Invoke-FileSymlink -Source $source -Dest $dest
            $result | Should -Be "created"
            # dest is now a symlink
            (Get-Item $dest -Force).Attributes -band [IO.FileAttributes]::ReparsePoint | Should -BeTrue
            # backup exists with original content
            Test-Path "$dest.bak" | Should -BeTrue
            Get-Content "$dest.bak" | Should -Be "dest content (broken symlink)"
        }

        It "overwrites existing .bak when backing up" -Skip:(-not $canSymlink) {
            $root = Join-Path $TestDrive "test-overwrite-bak"
            New-Item -ItemType Directory -Path $root -Force | Out-Null
            $source = Join-Path $root "source.txt"
            $dest = Join-Path $root "dest.txt"
            $backup = "$dest.bak"
            "source content" | Set-Content $source
            "new dest content" | Set-Content $dest
            "old backup content" | Set-Content $backup

            $result = Invoke-FileSymlink -Source $source -Dest $dest
            $result | Should -Be "created"
            # Old .bak content should be replaced
            Get-Content $backup | Should -Be "new dest content"
        }

        It "skips when already correctly symlinked" -Skip:(-not $canSymlink) {
            $root = Join-Path $TestDrive "test-already"
            New-Item -ItemType Directory -Path $root -Force | Out-Null
            $source = Join-Path $root "source.txt"
            $dest = Join-Path $root "dest.txt"
            "source content" | Set-Content $source
            New-Item -ItemType SymbolicLink -Path $dest -Target $source | Out-Null

            $result = Invoke-FileSymlink -Source $source -Dest $dest
            $result | Should -Be "already-linked"
        }
    }

    Context "Abnormal cases" {
        It "skips when source does not exist" {
            $root = Join-Path $TestDrive "test-no-source"
            $source = Join-Path $root "nonexistent.txt"
            $dest = Join-Path $root "dest.txt"

            $result = Invoke-FileSymlink -Source $source -Dest $dest
            $result | Should -Be "skipped-no-source"
        }

        It "creates symlink when dest does not exist" -Skip:(-not $canSymlink) {
            $root = Join-Path $TestDrive "test-no-dest"
            New-Item -ItemType Directory -Path $root -Force | Out-Null
            $source = Join-Path $root "source.txt"
            $dest = Join-Path $root "dest.txt"
            "source content" | Set-Content $source

            $result = Invoke-FileSymlink -Source $source -Dest $dest
            $result | Should -Be "created"
            (Get-Item $dest -Force).Attributes -band [IO.FileAttributes]::ReparsePoint | Should -BeTrue
        }
    }
}

Describe "profile.ps1 broken symlink detection" {
    BeforeAll {
        # Extract the detection logic from profile.ps1
        function Get-BrokenSymlinks($Files) {
            $Files | Where-Object {
                (Test-Path $_) -and -not ((Get-Item $_ -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)
            }
        }
    }

    Context "Normal cases" {
        It "detects regular file as broken symlink" -Skip:(-not $canSymlink) {
            $root = Join-Path $TestDrive "test-detect-broken"
            New-Item -ItemType Directory -Path $root -Force | Out-Null
            $file = Join-Path $root "regular.txt"
            "content" | Set-Content $file

            $broken = @(Get-BrokenSymlinks @($file))
            $broken.Count | Should -Be 1
            $broken[0] | Should -Be $file
        }

        It "does not flag valid symlinks" -Skip:(-not $canSymlink) {
            $root = Join-Path $TestDrive "test-detect-valid"
            New-Item -ItemType Directory -Path $root -Force | Out-Null
            $source = Join-Path $root "source.txt"
            $link = Join-Path $root "link.txt"
            "content" | Set-Content $source
            New-Item -ItemType SymbolicLink -Path $link -Target $source | Out-Null

            $broken = @(Get-BrokenSymlinks @($link))
            $broken.Count | Should -Be 0
        }
    }

    Context "Abnormal cases" {
        It "does not flag non-existent files" {
            $file = Join-Path $TestDrive "nonexistent.txt"

            $broken = @(Get-BrokenSymlinks @($file))
            $broken.Count | Should -Be 0
        }
    }
}
