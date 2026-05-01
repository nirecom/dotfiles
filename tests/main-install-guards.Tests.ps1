# tests/main-install-guards.Tests.ps1
# TDD: Tests for platform guards in install.ps1 files across dotfiles, dotfiles-private, and agents.
# These tests are expected to FAIL until the platform guard implementation is in place.
# Syntax tests (S-group) should PASS immediately as the source files are valid PowerShell.

BeforeAll {
    $script:DotfilesDir  = 'c:/git/dotfiles'
    $script:PrivateDir   = 'c:/git/dotfiles-private'
    $script:AgentsDir    = 'c:/git/agents'

    $script:DotfilesPs1  = Join-Path $script:DotfilesDir  'install.ps1'
    $script:PrivatePs1   = Join-Path $script:PrivateDir   'install.ps1'
    $script:AgentsPs1    = Join-Path $script:AgentsDir    'install.ps1'

    $script:Contents = @{
        dotfiles        = if (Test-Path $script:DotfilesPs1) { Get-Content $script:DotfilesPs1 -Raw } else { '' }
        'dotfiles-private' = if (Test-Path $script:PrivatePs1)  { Get-Content $script:PrivatePs1  -Raw } else { '' }
        agents          = if (Test-Path $script:AgentsPs1)    { Get-Content $script:AgentsPs1    -Raw } else { '' }
    }
    $script:Lines = @{
        dotfiles        = if (Test-Path $script:DotfilesPs1) { Get-Content $script:DotfilesPs1 } else { @() }
        'dotfiles-private' = if (Test-Path $script:PrivatePs1)  { Get-Content $script:PrivatePs1  } else { @() }
        agents          = if (Test-Path $script:AgentsPs1)    { Get-Content $script:AgentsPs1    } else { @() }
    }
}

# ---------------------------------------------------------------------------
# S: Syntax checks — must pass now AND after implementation
# ---------------------------------------------------------------------------

Describe "S: Syntax checks" {
    It "S1: dotfiles/install.ps1 has valid PowerShell syntax" {
        $errors = $null
        [System.Management.Automation.Language.Parser]::ParseInput(
            $script:Contents['dotfiles'], [ref]$null, [ref]$errors) | Out-Null
        $errors.Count | Should -Be 0
    }

    It "S2: dotfiles-private/install.ps1 has valid PowerShell syntax" {
        $errors = $null
        [System.Management.Automation.Language.Parser]::ParseInput(
            $script:Contents['dotfiles-private'], [ref]$null, [ref]$errors) | Out-Null
        $errors.Count | Should -Be 0
    }

    It "S3: agents/install.ps1 has valid PowerShell syntax" {
        $errors = $null
        [System.Management.Automation.Language.Parser]::ParseInput(
            $script:Contents['agents'], [ref]$null, [ref]$errors) | Out-Null
        $errors.Count | Should -Be 0
    }
}

# ---------------------------------------------------------------------------
# G: Guard presence — static pattern checks (TDD: FAIL until implementation)
# ---------------------------------------------------------------------------

Describe "G: Guard presence — dotfiles/install.ps1" {
    It "G1: dotfiles/install.ps1 contains IsWindows guard" {
        # TDD: will FAIL until guard is added
        $script:Contents['dotfiles'] | Should -Match '\$IsWindows\s+-eq\s+\$false'
    }
}

Describe "G: Guard presence — dotfiles-private/install.ps1" {
    It "G2: dotfiles-private/install.ps1 contains IsWindows guard" {
        # TDD: will FAIL until guard is added
        $script:Contents['dotfiles-private'] | Should -Match '\$IsWindows\s+-eq\s+\$false'
    }
}

Describe "G: Guard presence — agents/install.ps1" {
    It "G3: agents/install.ps1 contains IsWindows guard" {
        # TDD: will FAIL until guard is added
        $script:Contents['agents'] | Should -Match '\$IsWindows\s+-eq\s+\$false'
    }
}

# ---------------------------------------------------------------------------
# L: Guard logic unit test — verify the condition semantics
# ---------------------------------------------------------------------------

Describe "L: Guard logic unit tests" {
    It "L1: null -eq false evaluates to false (PS5.1 behavior — no automatic IsWindows var)" {
        # In PS5.1 on Windows, $IsWindows is not defined → $null -eq $false → $false
        # The guard ($IsWindows -eq $false) should NOT trigger on PS5.1 Windows (install.ps1 is Windows-only)
        ($null -eq $false) | Should -Be $false
    }

    It "L2: true -eq false evaluates to false (PS7 Windows — IsWindows is true)" {
        # On Windows with PS7, $IsWindows = $true → $true -eq $false → $false (guard does not trigger)
        ($true -eq $false) | Should -Be $false
    }

    It "L3: false -eq false evaluates to true (PS7 Linux/macOS — IsWindows is false)" {
        # On Linux/macOS with PS7, $IsWindows = $false → $false -eq $false → $true (guard triggers, blocks run)
        ($false -eq $false) | Should -Be $true
    }

    It "L4: guard condition (simulated-Linux IsWindows=false) correctly blocks non-Windows execution" {
        # Simulate Linux/macOS: the guard variable holds $false (cannot reassign read-only $IsWindows)
        $simulatedIsWindows = $false
        ($simulatedIsWindows -eq $false) | Should -Be $true
    }

    It "L5: guard condition (simulated-Windows IsWindows=true) passes on Windows execution" {
        # Simulate Windows: the guard variable holds $true
        $simulatedIsWindows = $true
        ($simulatedIsWindows -eq $false) | Should -Be $false
    }
}

# ---------------------------------------------------------------------------
# P: Guard placement — $IsWindows guard BEFORE Set-StrictMode
# ---------------------------------------------------------------------------

Describe "P: Guard placement — dotfiles/install.ps1" {
    It "P1: dotfiles/install.ps1 has IsWindows guard before Set-StrictMode" {
        # TDD: will FAIL until guard is added before Set-StrictMode
        $lines = $script:Lines['dotfiles']
        $guardLine = ($lines | Select-String '\$IsWindows\s+-eq\s+\$false').LineNumber | Select-Object -First 1
        $strictLine = ($lines | Select-String 'Set-StrictMode').LineNumber | Select-Object -First 1
        # Guard must exist
        $guardLine | Should -Not -BeNullOrEmpty
        # Guard must come before Set-StrictMode
        $guardLine | Should -BeLessThan $strictLine
    }
}

Describe "P: Guard placement — dotfiles-private/install.ps1" {
    It "P2: dotfiles-private/install.ps1 has IsWindows guard before Set-StrictMode" {
        # TDD: will FAIL until guard is added before Set-StrictMode
        $lines = $script:Lines['dotfiles-private']
        $guardLine = ($lines | Select-String '\$IsWindows\s+-eq\s+\$false').LineNumber | Select-Object -First 1
        $strictLine = ($lines | Select-String 'Set-StrictMode').LineNumber | Select-Object -First 1
        $guardLine | Should -Not -BeNullOrEmpty
        $guardLine | Should -BeLessThan $strictLine
    }
}

Describe "P: Guard placement — agents/install.ps1" {
    It "P3: agents/install.ps1 has IsWindows guard before Set-StrictMode" {
        # TDD: will FAIL until guard is added before Set-StrictMode
        $lines = $script:Lines['agents']
        $guardLine = ($lines | Select-String '\$IsWindows\s+-eq\s+\$false').LineNumber | Select-Object -First 1
        $strictLine = ($lines | Select-String 'Set-StrictMode').LineNumber | Select-Object -First 1
        $guardLine | Should -Not -BeNullOrEmpty
        $guardLine | Should -BeLessThan $strictLine
    }
}
