# Tests for AHK shortcut migration in install/win/autohotkey.ps1
# When startup shortcut points to old path or uses .ahk as TargetPath, it should be migrated

Describe "autohotkey shortcut migration" {
    BeforeAll {
        # Helper: create a .lnk shortcut
        function New-TestShortcut($Path, $TargetPath, $WorkingDirectory, $Arguments = "") {
            $shell = New-Object -ComObject WScript.Shell
            $shortcut = $shell.CreateShortcut($Path)
            $shortcut.TargetPath = $TargetPath
            $shortcut.WorkingDirectory = $WorkingDirectory
            $shortcut.Arguments = $Arguments
            $shortcut.Save()
        }

        # Extract the migration logic as a testable function
        function Invoke-ShortcutMigration($ShortcutPath, $AhkExe, $AhkScript) {
            if (-not (Test-Path $ShortcutPath)) {
                return "no-shortcut"
            }
            $shell = New-Object -ComObject WScript.Shell
            $existing = $shell.CreateShortcut($ShortcutPath)

            # Already using exe target with correct arguments
            if ($existing.TargetPath -eq $AhkExe -and $existing.Arguments -eq "`"$AhkScript`"") {
                return "already-current"
            }

            # Needs migration: .ahk as TargetPath (old path or format upgrade)
            if ($existing.TargetPath -like "*.ahk") {
                $existing.TargetPath = $AhkExe
                $existing.Arguments = "`"$AhkScript`""
                $existing.WorkingDirectory = Split-Path $AhkScript -Parent
                $existing.Save()
                return "migrated"
            }

            return "already-current"
        }
    }

    Context "Normal cases" {
        It "migrates shortcut from old .ahk path to exe format" {
            $root = Join-Path $TestDrive "test-migrate"
            $oldDir = Join-Path $root "win\config\autohotkey"
            $newDir = Join-Path $root "config\win\autohotkey"
            New-Item -ItemType Directory -Path $oldDir -Force | Out-Null
            New-Item -ItemType Directory -Path $newDir -Force | Out-Null
            "dummy" | Set-Content (Join-Path $oldDir "force-japanese-layout.ahk")
            "dummy" | Set-Content (Join-Path $newDir "force-japanese-layout.ahk")

            $lnk = Join-Path $TestDrive "test.lnk"
            $oldTarget = Join-Path $oldDir "force-japanese-layout.ahk"
            $newScript = Join-Path $newDir "force-japanese-layout.ahk"
            $fakeExe = Join-Path $TestDrive "AutoHotkey64.exe"
            "dummy" | Set-Content $fakeExe
            New-TestShortcut -Path $lnk -TargetPath $oldTarget -WorkingDirectory $oldDir

            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -AhkExe $fakeExe -AhkScript $newScript
            $result | Should -Be "migrated"

            # Verify shortcut now uses exe format
            $shell = New-Object -ComObject WScript.Shell
            $updated = $shell.CreateShortcut($lnk)
            $updated.TargetPath | Should -Be $fakeExe
            $updated.Arguments | Should -Be "`"$newScript`""
        }

        It "upgrades shortcut from current .ahk path to exe format" {
            $root = Join-Path $TestDrive "test-upgrade"
            $newDir = Join-Path $root "config\win\autohotkey"
            New-Item -ItemType Directory -Path $newDir -Force | Out-Null
            "dummy" | Set-Content (Join-Path $newDir "force-japanese-layout.ahk")

            $lnk = Join-Path $TestDrive "test-upgrade.lnk"
            $newScript = Join-Path $newDir "force-japanese-layout.ahk"
            $fakeExe = Join-Path $TestDrive "AutoHotkey64.exe"
            "dummy" | Set-Content $fakeExe
            # Shortcut currently targets the .ahk file directly (old format)
            New-TestShortcut -Path $lnk -TargetPath $newScript -WorkingDirectory $newDir

            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -AhkExe $fakeExe -AhkScript $newScript
            $result | Should -Be "migrated"

            $shell = New-Object -ComObject WScript.Shell
            $updated = $shell.CreateShortcut($lnk)
            $updated.TargetPath | Should -Be $fakeExe
            $updated.Arguments | Should -Be "`"$newScript`""
        }

        It "skips shortcut already using exe format" {
            $root = Join-Path $TestDrive "test-current"
            $newDir = Join-Path $root "config\win\autohotkey"
            New-Item -ItemType Directory -Path $newDir -Force | Out-Null
            "dummy" | Set-Content (Join-Path $newDir "force-japanese-layout.ahk")

            $lnk = Join-Path $TestDrive "test-current.lnk"
            $newScript = Join-Path $newDir "force-japanese-layout.ahk"
            $fakeExe = Join-Path $TestDrive "AutoHotkey64.exe"
            "dummy" | Set-Content $fakeExe
            New-TestShortcut -Path $lnk -TargetPath $fakeExe -WorkingDirectory $newDir -Arguments "`"$newScript`""

            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -AhkExe $fakeExe -AhkScript $newScript
            $result | Should -Be "already-current"
        }
    }

    Context "Abnormal cases" {
        It "returns no-shortcut when file does not exist" {
            $lnk = Join-Path $TestDrive "nonexistent.lnk"
            $result = Invoke-ShortcutMigration -ShortcutPath $lnk -AhkExe "C:\dummy.exe" -AhkScript "C:\dummy.ahk"
            $result | Should -Be "no-shortcut"
        }
    }
}
