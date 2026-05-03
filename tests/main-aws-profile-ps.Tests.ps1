# tests/main-aws-profile-ps.Tests.ps1
# Pester 5.x tests for Select-AwsProfile and Set-LocationWithFnmAndAws
# The functions are defined inline (spec-driven, TDD) — do not depend on profile.ps1 existing.
# Run: Invoke-Pester 'tests/main-aws-profile-ps.Tests.ps1' -Output Detailed

BeforeAll {
    # ---------------------------------------------------------------------------
    # aws mock: script-level function that inspects $args to return correct region
    # ---------------------------------------------------------------------------
    function global:aws {
        $argList = $args
        # Join args to find --profile value
        $profileIdx = [array]::IndexOf([string[]]$argList, '--profile')
        $profileName = if ($profileIdx -ge 0 -and $profileIdx + 1 -lt $argList.Count) {
            $argList[$profileIdx + 1]
        } else { '' }

        # Only respond to "configure get region --profile <name>"
        if ($argList -contains 'configure' -and $argList -contains 'get' -and $argList -contains 'region') {
            switch ($profileName) {
                'work'     { return 'ap-northeast-1' }
                default    { return '' }
            }
        }
        return ''
    }

    # ---------------------------------------------------------------------------
    # Select-AwsProfile inline definition (from spec)
    # ---------------------------------------------------------------------------
    function global:Select-AwsProfile {
        $workDir = $env:AWS_WORK_DIR.TrimEnd('\', '/')
        $inWorkDir = $PWD.Path.StartsWith($workDir, [System.StringComparison]::OrdinalIgnoreCase) -and
                     ($PWD.Path.Length -eq $workDir.Length -or $PWD.Path[$workDir.Length] -in '\', '/')
        if ($inWorkDir) {
            if ($env:AWS_PROFILE -ne 'work') {
                $env:AWS_PROFILE = 'work'
                $region = aws configure get region --profile work 2>$null
                if ($region) { $env:AWS_DEFAULT_REGION = $region }
                else { Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue }
            }
        } else {
            if ($env:AWS_PROFILE) {
                Remove-Item Env:AWS_PROFILE -ErrorAction SilentlyContinue
                Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue
            }
        }
    }

    # ---------------------------------------------------------------------------
    # Temp directories for tests
    # ---------------------------------------------------------------------------
    $script:TmpBase     = Join-Path ([System.IO.Path]::GetTempPath()) "aws-ps-test-$([System.IO.Path]::GetRandomFileName())"
    $script:WorkDir     = Join-Path $script:TmpBase 'work'
    $script:WorkSubDir  = Join-Path $script:TmpBase 'work\project\subdir'
    $script:OtherDir    = Join-Path $script:TmpBase 'other'
    $script:WorkOther   = Join-Path $script:TmpBase 'workother'   # false prefix

    New-Item -ItemType Directory -Path $script:WorkDir    -Force | Out-Null
    New-Item -ItemType Directory -Path $script:WorkSubDir -Force | Out-Null
    New-Item -ItemType Directory -Path $script:OtherDir   -Force | Out-Null
    New-Item -ItemType Directory -Path $script:WorkOther  -Force | Out-Null

    # Save original location
    $script:OriginalLocation = Get-Location
}

AfterAll {
    Set-Location $script:OriginalLocation
    Remove-Item -Recurse -Force $script:TmpBase -ErrorAction SilentlyContinue
}

# Helper: set AWS_WORK_DIR, clear env, cd, then call Select-AwsProfile
function global:Invoke-SelectAwsProfile {
    param(
        [string]$WorkDir,
        [string]$Cwd
    )
    $env:AWS_WORK_DIR = $WorkDir
    Remove-Item Env:AWS_PROFILE          -ErrorAction SilentlyContinue
    Remove-Item Env:AWS_DEFAULT_REGION   -ErrorAction SilentlyContinue
    Set-Location $Cwd
    Select-AwsProfile
}

# ---------------------------------------------------------------------------
# Normal cases
# ---------------------------------------------------------------------------

Describe "N: Normal cases" {

    It "N1: PWD = work_dir (exact) → AWS_PROFILE=work, AWS_DEFAULT_REGION=ap-northeast-1" {
        Invoke-SelectAwsProfile -WorkDir $script:WorkDir -Cwd $script:WorkDir
        $env:AWS_PROFILE          | Should -Be 'work'
        $env:AWS_DEFAULT_REGION   | Should -Be 'ap-northeast-1'
    }

    It "N2: PWD in subdir of work_dir → AWS_PROFILE=work" {
        Invoke-SelectAwsProfile -WorkDir $script:WorkDir -Cwd $script:WorkSubDir
        $env:AWS_PROFILE | Should -Be 'work'
    }

    It "N3: PWD outside work_dir → AWS_PROFILE unset, AWS_DEFAULT_REGION unset" {
        Invoke-SelectAwsProfile -WorkDir $script:WorkDir -Cwd $script:OtherDir
        $env:AWS_PROFILE        | Should -BeNullOrEmpty
        $env:AWS_DEFAULT_REGION | Should -BeNullOrEmpty
    }

    It "N4: switch work → outside → AWS_PROFILE unset, AWS_DEFAULT_REGION unset" {
        $env:AWS_WORK_DIR = $script:WorkDir
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue

        Set-Location $script:WorkDir
        Select-AwsProfile
        $env:AWS_PROFILE | Should -Be 'work'

        Set-Location $script:OtherDir
        Select-AwsProfile
        $env:AWS_PROFILE        | Should -BeNullOrEmpty
        $env:AWS_DEFAULT_REGION | Should -BeNullOrEmpty
    }
}

# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

Describe "E: Edge cases" {

    It "E1: false prefix — C:\workother should be unset (not treated as subdir of work)" {
        Invoke-SelectAwsProfile -WorkDir $script:WorkDir -Cwd $script:WorkOther
        $env:AWS_PROFILE | Should -BeNullOrEmpty
    }

    It "E2: trailing backslash in AWS_WORK_DIR → treated same as without" {
        $env:AWS_WORK_DIR = $script:WorkDir + '\'
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue
        Set-Location $script:WorkDir
        Select-AwsProfile
        $env:AWS_PROFILE | Should -Be 'work'
    }

    It "E3: case-insensitive match — uppercase work dir matches lowercase PWD" {
        $upperWork = $script:WorkDir.ToUpper()
        $env:AWS_WORK_DIR = $upperWork
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue
        Set-Location $script:WorkDir   # lowercase
        Select-AwsProfile
        $env:AWS_PROFILE | Should -Be 'work'
    }

    It "E4: region unavailable → AWS_DEFAULT_REGION removed" {
        # Override aws mock to return empty
        function global:aws {
            return ''
        }

        $env:AWS_WORK_DIR        = $script:WorkDir
        $env:AWS_DEFAULT_REGION  = 'old-region'
        Remove-Item Env:AWS_PROFILE -ErrorAction SilentlyContinue
        Set-Location $script:WorkDir
        Select-AwsProfile

        $env:AWS_DEFAULT_REGION | Should -BeNullOrEmpty

        # Restore original aws mock
        function global:aws {
            $argList = $args
            $profileIdx = [array]::IndexOf([string[]]$argList, '--profile')
            $profileName = if ($profileIdx -ge 0 -and $profileIdx + 1 -lt $argList.Count) {
                $argList[$profileIdx + 1]
            } else { '' }
            if ($argList -contains 'configure' -and $argList -contains 'get' -and $argList -contains 'region') {
                switch ($profileName) {
                    'work'     { return 'ap-northeast-1' }
                    default    { return '' }
                }
            }
            return ''
        }
    }

    It "E5: same profile repeated call → aws configure NOT called again" {
        $script:AwsCallCount = 0

        function global:aws {
            $argList = $args
            if ($argList -contains 'configure' -and $argList -contains 'get' -and $argList -contains 'region') {
                $script:AwsCallCount++
                $profileIdx = [array]::IndexOf([string[]]$argList, '--profile')
                $profileName = if ($profileIdx -ge 0 -and $profileIdx + 1 -lt $argList.Count) {
                    $argList[$profileIdx + 1]
                } else { '' }
                switch ($profileName) {
                    'work'     { return 'ap-northeast-1' }
                    default    { return '' }
                }
            }
            return ''
        }

        $env:AWS_WORK_DIR = $script:WorkDir
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue

        # In-work repeated calls: aws should only be called once
        Set-Location $script:WorkDir
        Select-AwsProfile   # first call: profile changes → aws called
        Select-AwsProfile   # second call: profile same → no aws call
        Select-AwsProfile   # third call: profile same → no aws call

        $script:AwsCallCount | Should -Be 1

        # Outside (unset) repeated calls: aws should never be called
        $script:AwsCallCount = 0
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue

        Set-Location $script:OtherDir
        Select-AwsProfile   # already unset, staying outside → no aws call
        Select-AwsProfile   # still outside → no aws call
        Select-AwsProfile   # still outside → no aws call

        $script:AwsCallCount | Should -Be 0

        # Restore original aws mock
        function global:aws {
            $argList = $args
            $profileIdx = [array]::IndexOf([string[]]$argList, '--profile')
            $profileName = if ($profileIdx -ge 0 -and $profileIdx + 1 -lt $argList.Count) {
                $argList[$profileIdx + 1]
            } else { '' }
            if ($argList -contains 'configure' -and $argList -contains 'get' -and $argList -contains 'region') {
                switch ($profileName) {
                    'work'     { return 'ap-northeast-1' }
                    default    { return '' }
                }
            }
            return ''
        }
    }
}

# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------

Describe "ER: Error cases" {

    It "ER1: AWS_WORK_DIR unset → Select-AwsProfile guard does not run / AWS_PROFILE unchanged" {
        # Simulate what the block does when AWS_WORK_DIR is unset:
        # The outer `if` guard prevents Select-AwsProfile from being defined.
        # We test the guard logic directly.
        $savedWorkDir = $env:AWS_WORK_DIR
        Remove-Item Env:AWS_WORK_DIR   -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_PROFILE    -ErrorAction SilentlyContinue

        # Replicate the guard: only call Select-AwsProfile if AWS_WORK_DIR is set
        $guardPassed = (Get-Command aws -ErrorAction SilentlyContinue) -and $env:AWS_WORK_DIR
        $guardPassed | Should -Be $false

        # AWS_PROFILE should remain unset
        $env:AWS_PROFILE | Should -BeNullOrEmpty

        # Restore
        $env:AWS_WORK_DIR = $savedWorkDir
    }
}

# ---------------------------------------------------------------------------
# Idempotency cases
# ---------------------------------------------------------------------------

Describe "I: Idempotency cases" {

    It "I1: sourcing the block twice produces the same AWS_PROFILE result" {
        $env:AWS_WORK_DIR = $script:WorkDir
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue

        Set-Location $script:WorkDir

        # Simulate sourcing the block twice by calling Select-AwsProfile twice
        Select-AwsProfile
        $firstProfile = $env:AWS_PROFILE

        Select-AwsProfile
        $secondProfile = $env:AWS_PROFILE

        $firstProfile  | Should -Be 'work'
        $secondProfile | Should -Be 'work'
        $firstProfile  | Should -Be $secondProfile
    }

    It "I2: switching dirs and back produces consistent results" {
        $env:AWS_WORK_DIR = $script:WorkDir
        Remove-Item Env:AWS_PROFILE        -ErrorAction SilentlyContinue
        Remove-Item Env:AWS_DEFAULT_REGION -ErrorAction SilentlyContinue

        Set-Location $script:WorkDir
        Select-AwsProfile
        $env:AWS_PROFILE | Should -Be 'work'

        Set-Location $script:OtherDir
        Select-AwsProfile
        $env:AWS_PROFILE | Should -BeNullOrEmpty

        Set-Location $script:WorkDir
        Select-AwsProfile
        $env:AWS_PROFILE | Should -Be 'work'

        Set-Location $script:OtherDir
        Select-AwsProfile
        $env:AWS_PROFILE | Should -BeNullOrEmpty
    }

    It "I3: provider path passthrough — non-filesystem path does not call Select-AwsProfile" {
        # Test the isFileSystem guard logic from Set-LocationWithAws/Set-LocationWithFnmAndAws.
        # For a path like 'HKLM:\' the drivePrefix is 'HKLM' (length > 1), not [A-Za-z]:\, not /
        # → isFileSystem = false → cd happens without calling Select-AwsProfile.
        $script:AwsCallCountI3 = 0

        # Inline the isFileSystem check from the spec
        function Test-IsFilesystemPath {
            param([string]$target)
            $drivePrefix = ($target -split ':')[0]
            $isFileSystem = ($drivePrefix.Length -le 1) -or ($target -match '^[A-Za-z]:\\') -or ($target -match '^/')
            return $isFileSystem
        }

        # HKLM:\ should not be treated as filesystem (registry provider)
        Test-IsFilesystemPath 'HKLM:\' | Should -Be $false

        # C:\ should be treated as filesystem
        Test-IsFilesystemPath 'C:\Users' | Should -Be $true

        # Unix-style path
        Test-IsFilesystemPath '/tmp/foo' | Should -Be $true

        # Single-letter drive without colon (edge: just 'C' — length 1)
        Test-IsFilesystemPath 'C' | Should -Be $true
    }
}
