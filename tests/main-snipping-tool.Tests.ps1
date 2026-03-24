# Tests for install/win/snipping-tool.ps1
# Validates Snipping Tool notification disabling via registry

Describe "Snipping Tool notification disable script" {
    BeforeAll {
        $ScriptPath = Join-Path $PSScriptRoot "..\install\win\snipping-tool.ps1"
        $RegPath = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Notifications\Settings\Microsoft.ScreenSketch_8wekyb3d8bbwe!App'

        # Save original state for restore
        $script:OriginalKeyExisted = Test-Path $RegPath
        if ($script:OriginalKeyExisted) {
            $props = Get-ItemProperty $RegPath -ErrorAction SilentlyContinue
            $script:OriginalEnabled = $props.PSObject.Properties['Enabled']
            if ($script:OriginalEnabled) {
                $script:OriginalEnabledValue = $props.Enabled
            }
        }
    }

    AfterAll {
        # Restore original state
        if (-not $script:OriginalKeyExisted) {
            Remove-Item $RegPath -ErrorAction SilentlyContinue
        } elseif ($script:OriginalEnabled) {
            Set-ItemProperty -Path $RegPath -Name 'Enabled' -Value $script:OriginalEnabledValue -Type DWord
        } else {
            Remove-ItemProperty -Path $RegPath -Name 'Enabled' -ErrorAction SilentlyContinue
        }
    }

    Context "Normal cases" {
        It "sets Enabled=0 when Enabled property does not exist" {
            # Setup: ensure key exists but Enabled property is absent
            if (-not (Test-Path $RegPath)) {
                New-Item -Path $RegPath -Force | Out-Null
            }
            Remove-ItemProperty -Path $RegPath -Name 'Enabled' -ErrorAction SilentlyContinue

            & $ScriptPath

            $val = (Get-ItemProperty $RegPath).Enabled
            $val | Should -Be 0
        }

        It "sets Enabled=0 when Enabled=1 (explicitly enabled)" {
            Set-ItemProperty -Path $RegPath -Name 'Enabled' -Value 1 -Type DWord

            & $ScriptPath

            $val = (Get-ItemProperty $RegPath).Enabled
            $val | Should -Be 0
        }
    }

    Context "Idempotency cases" {
        It "remains Enabled=0 when already disabled" {
            Set-ItemProperty -Path $RegPath -Name 'Enabled' -Value 0 -Type DWord

            & $ScriptPath

            $val = (Get-ItemProperty $RegPath).Enabled
            $val | Should -Be 0
        }
    }

    Context "Edge cases" {
        It "creates registry key and sets Enabled=0 when key does not exist" {
            # Setup: remove the key entirely
            Remove-Item $RegPath -ErrorAction SilentlyContinue

            & $ScriptPath

            Test-Path $RegPath | Should -Be $true
            $val = (Get-ItemProperty $RegPath).Enabled
            $val | Should -Be 0
        }
    }
}
