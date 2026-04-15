# Test: install.ps1 — keyboard hotkey detection logic (lines 81–92)
# Verifies the Test-KeyboardHotkeysSet logic that replaced Get-ItemProperty
# with Get-Item + .GetValue() to safely handle missing registry values.
#
# Regression: the original code used Get-ItemProperty and accessed
# '.Language Hotkey' directly, which threw a terminating error when the
# registry key existed but had no properties. The fix returns $false safely.

BeforeAll {
    # Logic extracted from install.ps1 lines 82–85 + condition on line 86
    function Test-KeyboardHotkeysSet {
        param([string]$ToggleKey)
        $keyItem        = Get-Item -Path $ToggleKey -ErrorAction SilentlyContinue
        $languageHotkey = if ($keyItem) { $keyItem.GetValue('Language Hotkey') } else { $null }
        $layoutHotkey   = if ($keyItem) { $keyItem.GetValue('Layout Hotkey') } else { $null }
        $hotkeyValue    = if ($keyItem) { $keyItem.GetValue('Hotkey') } else { $null }
        return ($languageHotkey -eq '3' -and $layoutHotkey -eq '3' -and $hotkeyValue -eq '3')
    }
}

Describe "keyboard hotkey detection — Test-KeyboardHotkeysSet" {
    BeforeEach {
        $script:TestKey = "HKCU:\Software\dotfiles-test-$(Get-Random)"
    }

    AfterEach {
        if (Test-Path $script:TestKey) {
            Remove-Item -Path $script:TestKey -Recurse -Force -ErrorAction SilentlyContinue
        }
    }

    It "returns true when all three hotkey values are '3' (already disabled)" {
        New-Item -Path $script:TestKey -Force | Out-Null
        Set-ItemProperty -Path $script:TestKey -Name 'Language Hotkey' -Value '3'
        Set-ItemProperty -Path $script:TestKey -Name 'Layout Hotkey'   -Value '3'
        Set-ItemProperty -Path $script:TestKey -Name 'Hotkey'          -Value '3'

        Test-KeyboardHotkeysSet -ToggleKey $script:TestKey | Should -BeTrue
    }

    It "returns false when all three hotkey values are '1' (not disabled)" {
        New-Item -Path $script:TestKey -Force | Out-Null
        Set-ItemProperty -Path $script:TestKey -Name 'Language Hotkey' -Value '1'
        Set-ItemProperty -Path $script:TestKey -Name 'Layout Hotkey'   -Value '1'
        Set-ItemProperty -Path $script:TestKey -Name 'Hotkey'          -Value '1'

        Test-KeyboardHotkeysSet -ToggleKey $script:TestKey | Should -BeFalse
    }

    It "REGRESSION: key exists with no properties — returns false, no error thrown" {
        # Original bug: Get-ItemProperty threw terminating error in this case.
        # The fix using Get-Item + .GetValue() must return $null safely.
        New-Item -Path $script:TestKey -Force | Out-Null
        # Do NOT set any properties — key is empty

        { Test-KeyboardHotkeysSet -ToggleKey $script:TestKey } | Should -Not -Throw
        Test-KeyboardHotkeysSet -ToggleKey $script:TestKey | Should -BeFalse
    }

    It "returns false when registry key does not exist, no error thrown" {
        # Key is never created in this test

        { Test-KeyboardHotkeysSet -ToggleKey $script:TestKey } | Should -Not -Throw
        Test-KeyboardHotkeysSet -ToggleKey $script:TestKey | Should -BeFalse
    }

    It "returns false when only 'Language Hotkey' exists (partial state)" {
        New-Item -Path $script:TestKey -Force | Out-Null
        Set-ItemProperty -Path $script:TestKey -Name 'Language Hotkey' -Value '3'
        # Layout Hotkey and Hotkey are absent

        { Test-KeyboardHotkeysSet -ToggleKey $script:TestKey } | Should -Not -Throw
        Test-KeyboardHotkeysSet -ToggleKey $script:TestKey | Should -BeFalse
    }

    It "returns false when values are '4' instead of '3'" {
        New-Item -Path $script:TestKey -Force | Out-Null
        Set-ItemProperty -Path $script:TestKey -Name 'Language Hotkey' -Value '4'
        Set-ItemProperty -Path $script:TestKey -Name 'Layout Hotkey'   -Value '4'
        Set-ItemProperty -Path $script:TestKey -Name 'Hotkey'          -Value '4'

        Test-KeyboardHotkeysSet -ToggleKey $script:TestKey | Should -BeFalse
    }

    It "is idempotent — calling twice on the same key returns the same result" {
        New-Item -Path $script:TestKey -Force | Out-Null
        Set-ItemProperty -Path $script:TestKey -Name 'Language Hotkey' -Value '3'
        Set-ItemProperty -Path $script:TestKey -Name 'Layout Hotkey'   -Value '3'
        Set-ItemProperty -Path $script:TestKey -Name 'Hotkey'          -Value '3'

        $first  = Test-KeyboardHotkeysSet -ToggleKey $script:TestKey
        $second = Test-KeyboardHotkeysSet -ToggleKey $script:TestKey

        $first  | Should -BeTrue
        $second | Should -BeTrue
        $first  | Should -Be $second
    }
}
