# Tests for AHK shortcut migration in install/win/autohotkey.ps1
# When startup shortcut points to old path (win\config\autohotkey\), it should be updated

Describe "autohotkey shortcut migration" {
    BeforeAll {
        # Helper: create a .lnk shortcut
        function New-TestShortcut($Path, $TargetPath, $WorkingDirectory) {
            $shell = New-Object -ComObject WScript.Shell
            $shortcut = $shell.CreateShortcut($Path)
            $shortcut.TargetPath = $TargetPath
            $shortcut.WorkingDirectory = $WorkingDirectory
            $shortcut.Save()
        }

        # Extract the migration logic as a testable function
        function Invoke-ShortcutMigration($ShortcutPath, $NewTarget) {
            if (-not (Test-Path $ShortcutPath)) {
                return "no-shortcut"
            }
            $shell = New-Object -ComObject WScript.Shell
            $existing = $shell.CreateShortcut($ShortcutPath)
            if ($existing.TargetPath -like "*win\config\autohotkey*") {
                $existing.TargetPath = $NewTarget
                $existing.WorkingDirectory = Split-Path $NewTarget -Parent
                $existing.Save()
                return "migrated"
            }
            return "already-current"
        }
    }

    Context "Normal cases" {
        It "migrates shortcut pointing to old path" {
            $root = Join-Path $TestDrive "test-migrate"
            $oldDir = Join-Path $root "win\config\autohotkey"
            $newDir = Join-Path $root "config\win\autohotkey"
            New-Item -ItemType Directory -Path $oldDir -Force | Out-Null
            New-Item -ItemType Directory -Path $newDir -Force | Out-Null
            "dummy" | Set-Content (Join-Path $oldDir "force-japanese-layout.ahk")
            "dummy" | Set-Content (Join-Path $newDir "force-japanese-layout.ahk")

            $lnk = Join-Path $TestDrive "test.lnk"
            $oldTarget = Join-Path $oldDir "force-japanese-layout.ahk"
            $newTarget = Join-Path $newDir "force-japanese-layout.ahk"
            New-TestShortcut -Path $lnk -TargetPath $oldTarget -WorkingDirectory $oldDir

            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -NewTarget $newTarget
            $result | Should -Be "migrated"

            # Verify shortcut now points to new path
            $shell = New-Object -ComObject WScript.Shell
            $updated = $shell.CreateShortcut($lnk)
            $updated.TargetPath | Should -BeLike "*config\win\autohotkey*"
            $updated.TargetPath | Should -Not -BeLike "*win\config\autohotkey*"
        }

        It "skips shortcut already pointing to new path" {
            $root = Join-Path $TestDrive "test-current"
            $newDir = Join-Path $root "config\win\autohotkey"
            New-Item -ItemType Directory -Path $newDir -Force | Out-Null
            "dummy" | Set-Content (Join-Path $newDir "force-japanese-layout.ahk")

            $lnk = Join-Path $TestDrive "test-current.lnk"
            $newTarget = Join-Path $newDir "force-japanese-layout.ahk"
            New-TestShortcut -Path $lnk -TargetPath $newTarget -WorkingDirectory $newDir

            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -NewTarget $newTarget
            $result | Should -Be "already-current"
        }
    }

    Context "Abnormal cases" {
        It "returns no-shortcut when file does not exist" {
            $lnk = Join-Path $TestDrive "nonexistent.lnk"
            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -NewTarget "C:\dummy"
            $result | Should -Be "no-shortcut"
        }
    }
}
