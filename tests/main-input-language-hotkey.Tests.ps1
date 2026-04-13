# Tests for input language hotkey disabling in install.ps1
# Validates Ctrl+Shift/Alt+Shift language hotkeys are disabled via registry

Describe "Input language hotkey disable" {
    BeforeAll {
        $ScriptPath = Join-Path $PSScriptRoot "..\install.ps1"
        $RegPath = 'HKCU:\Keyboard Layout\Toggle'

        # Save original state for restore
        $script:OriginalValues = @{}
        foreach ($name in 'Language Hotkey', 'Layout Hotkey', 'Hotkey') {
            $val = Get-ItemProperty $RegPath -Name $name -ErrorAction SilentlyContinue
            if ($val) {
                $script:OriginalValues[$name] = $val.$name
            }
        }
    }

    AfterAll {
        # Restore original state
        foreach ($name in 'Language Hotkey', 'Layout Hotkey', 'Hotkey') {
            if ($script:OriginalValues.ContainsKey($name)) {
                Set-ItemProperty -Path $RegPath -Name $name -Value $script:OriginalValues[$name]
            }
        }
    }

    Context "Normal cases" {
        It "sets all three hotkey values to 3 (disabled)" {
            # Pre-set to non-disabled values
            Set-ItemProperty $RegPath -Name 'Language Hotkey' -Value '1'
            Set-ItemProperty $RegPath -Name 'Layout Hotkey' -Value '2'
            Set-ItemProperty $RegPath -Name 'Hotkey' -Value '1'

            & $ScriptPath -Minimal

            $props = Get-ItemProperty $RegPath
            $props.'Language Hotkey' | Should -Be '3'
            $props.'Layout Hotkey' | Should -Be '3'
            $props.Hotkey | Should -Be '3'
        }
    }

    Context "Idempotency cases" {
        It "remains at 3 when already disabled" {
            Set-ItemProperty $RegPath -Name 'Language Hotkey' -Value '3'
            Set-ItemProperty $RegPath -Name 'Layout Hotkey' -Value '3'
            Set-ItemProperty $RegPath -Name 'Hotkey' -Value '3'

            & $ScriptPath -Minimal

            $props = Get-ItemProperty $RegPath
            $props.'Language Hotkey' | Should -Be '3'
            $props.'Layout Hotkey' | Should -Be '3'
            $props.Hotkey | Should -Be '3'
        }
    }
}
