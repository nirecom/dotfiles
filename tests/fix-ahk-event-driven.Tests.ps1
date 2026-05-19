# Tests for event-driven rewrite of force-japanese-layout.ahk
# Verifies static content of the script to ensure the new implementation
# uses shell hook events instead of a 500ms polling timer.

Describe "force-japanese-layout.ahk event-driven rewrite" {
    BeforeAll {
        $scriptPath = Join-Path (Join-Path (Join-Path (Join-Path (Join-Path $PSScriptRoot '..') 'config') 'win') 'autohotkey') 'force-japanese-layout.ahk'
        $script:scriptContent = Get-Content $scriptPath -Raw -ErrorAction Stop
    }

    Context "Polling removal" {
        It "does not use 500ms polling timer" {
            $script:scriptContent | Should -Not -Match 'SetTimer\s*\(\s*CheckLayout\s*,\s*500\s*\)'
        }
    }

    Context "Shell hook registration" {
        It "calls RegisterWindowMessage" {
            $script:scriptContent | Should -Match 'RegisterWindowMessage'
        }

        It "calls RegisterShellHookWindow" {
            $script:scriptContent | Should -Match 'RegisterShellHookWindow'
        }

        It "guards against RegisterWindowMessage failure (WM_SHELLHOOK = 0 check)" {
            $script:scriptContent | Should -Match 'WM_SHELLHOOK\s*=\s*0'
        }

        It "passes A_ScriptHwnd as Ptr to RegisterShellHookWindow" {
            $script:scriptContent | Should -Match '"Ptr"\s*,\s*A_ScriptHwnd'
        }
    }

    Context "Startup correction" {
        It "calls CheckLayout() at top level (outside any function body)" {
            # Extract lines that are NOT inside a function body.
            # A simple heuristic: find lines matching CheckLayout() that are
            # not indented (i.e., start at column 0 or have no leading whitespace
            # beyond what precedes top-level statements).
            # We look for a bare CheckLayout() call that is NOT part of the
            # SetTimer call and NOT inside a function definition block.
            $lines = $script:scriptContent -split "`n"
            $inFunction = $false
            $foundTopLevel = $false
            foreach ($line in $lines) {
                $trimmed = $line.TrimEnd()
                # Detect function definition start (name followed by opening brace)
                if ($trimmed -match '^\w.*\(\).*\{' -or $trimmed -match '^\w+\w*\(.*\)\s*\{') {
                    $inFunction = $true
                }
                if (-not $inFunction) {
                    # Top-level CheckLayout() call (not inside SetTimer argument)
                    if ($trimmed -match '^\s*CheckLayout\(\)\s*(;.*)?$') {
                        $foundTopLevel = $true
                    }
                }
                # Detect function body end (closing brace at column 0)
                if ($inFunction -and $trimmed -match '^\}') {
                    $inFunction = $false
                }
            }
            $foundTopLevel | Should -Be $true
        }
    }

    Context "Low-frequency safety net timer" {
        It "uses a low-frequency safety net timer (15000ms)" {
            $script:scriptContent | Should -Match 'SetTimer\s*\(\s*CheckLayout\s*,\s*15000\s*\)'
        }
    }

    Context "Shell hook event constants" {
        It "defines HSHELL_LANGUAGE constant" {
            $script:scriptContent | Should -Match 'HSHELL_LANGUAGE'
        }

        It "defines HSHELL_WINDOWACTIVATED constant" {
            $script:scriptContent | Should -Match 'HSHELL_WINDOWACTIVATED'
        }
    }

    Context "CheckLayoutForHwnd function" {
        It "contains CheckLayoutForHwnd function definition" {
            $script:scriptContent | Should -Match 'CheckLayoutForHwnd'
        }
    }

    Context "AHK version requirement" {
        It "declares AutoHotkey v2.0 requirement" {
            $script:scriptContent | Should -Match '#Requires AutoHotkey v2\.0'
        }
    }
}
