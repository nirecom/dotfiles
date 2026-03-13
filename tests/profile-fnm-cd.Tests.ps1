# Tests for fnm cd wrapper override in install/win/profile.ps1
# Verifies that Set-LocationWithFnm produces clean error messages
# Compatible with Pester 3.x

# Mock Set-FnmOnLoad (normally defined by fnm env)
function global:Set-FnmOnLoad { }

# Define the function under test (mirrors profile.ps1 override)
function global:Set-LocationWithFnm {
    param($path)
    if ($null -eq $path) {
        Set-Location $HOME
    } elseif (Test-Path -LiteralPath $path) {
        Set-Location -LiteralPath $path
    } else {
        Write-Error "cd: no such directory: $path" -ErrorAction Continue
        return
    }
    Set-FnmOnLoad
}

Describe "Set-LocationWithFnm (fnm cd wrapper)" {
    Context "Normal cases" {
        It "changes to an existing directory" {
            $target = Join-Path $TestDrive "existing"
            New-Item -ItemType Directory -Path $target -Force | Out-Null
            Set-LocationWithFnm $target
            (Get-Location).Path | Should Be $target
        }

        It "changes to home when no argument given" {
            Set-LocationWithFnm
            (Get-Location).Path | Should Be $HOME
        }

        It "calls Set-FnmOnLoad on successful cd" {
            $script:fnmCalled = $false
            function global:Set-FnmOnLoad { $script:fnmCalled = $true }
            $target = Join-Path $TestDrive "fnm-test"
            New-Item -ItemType Directory -Path $target -Force | Out-Null
            Set-LocationWithFnm $target
            $script:fnmCalled | Should Be $true
        }
    }

    Context "Abnormal cases" {
        It "produces a clean error for non-existent directory" {
            $err = $( Set-LocationWithFnm "nonexistent-dir-xyz" ) 2>&1
            $errMsg = ($err | Where-Object { $_ -is [System.Management.Automation.ErrorRecord] }).ToString()
            $errMsg | Should Match "cd: no such directory: nonexistent-dir-xyz"
        }

        It "error message does not contain line numbers" {
            $err = $( Set-LocationWithFnm "nonexistent-dir-xyz" ) 2>&1
            $errMsg = ($err | Where-Object { $_ -is [System.Management.Automation.ErrorRecord] }).ToString()
            $errMsg | Should Not Match "Line \|"
        }

        It "does not call Set-FnmOnLoad on failed cd" {
            $script:fnmCalled = $false
            function global:Set-FnmOnLoad { $script:fnmCalled = $true }
            Set-LocationWithFnm "nonexistent-dir-xyz" 2>&1 | Out-Null
            $script:fnmCalled | Should Be $false
        }
    }
}
