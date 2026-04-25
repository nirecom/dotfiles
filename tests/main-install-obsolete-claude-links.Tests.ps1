# Test: install-obsolete cleanup for stale claude-global symlinks (Windows)
# Verifies the Remove-ObsoleteClaudeLinks / Remove-ObsoleteDocAppendCmd /
# Remove-ObsoleteHooksPath logic intended for install-obsolete.ps1.

BeforeAll {
    function Remove-ObsoleteClaudeLinks {
        param([string]$ClaudeDir)
        if (-not (Test-Path $ClaudeDir)) { return }
        $names = @("CLAUDE.md", "settings.json", "skills", "rules", "agents")
        foreach ($name in $names) {
            $link = Join-Path $ClaudeDir $name
            if (Test-Path $link -PathType Any) {
                $item = Get-Item $link -Force
                if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
                    $target = $item.Target
                    if ($target -like "*\dotfiles\claude-global\*" -or $target -like "*/dotfiles/claude-global/*") {
                        Remove-Item $link -Force
                    }
                }
            }
        }
    }

    function Remove-ObsoleteDocAppendCmd {
        param([string]$CmdPath)
        if (-not (Test-Path $CmdPath)) { return }
        $content = Get-Content $CmdPath -Raw -ErrorAction SilentlyContinue
        if ($content -like "*dotfiles\bin\doc-append.py*") {
            Remove-Item $CmdPath -Force
        }
    }

    function Remove-ObsoleteHooksPath {
        param([string]$ConfigLocalPath)
        if (-not (Test-Path $ConfigLocalPath)) { return }
        $hooksPath = git config --file $ConfigLocalPath core.hooksPath 2>$null
        if ($hooksPath -like "*\dotfiles\claude-global\hooks" -or $hooksPath -like "*/dotfiles/claude-global/hooks") {
            git config --file $ConfigLocalPath --unset core.hooksPath 2>$null | Out-Null
        }
    }

    # Removes leftover dotfiles\claude-global\ directory only when HEAD confirms
    # claude-global\ is no longer tracked (post-agents-split).
    function Remove-ObsoleteClaudeGlobalDir {
        param([string]$DotfilesDir)
        $cgDir = Join-Path $DotfilesDir 'claude-global'
        $dfGit = Join-Path $DotfilesDir '.git'
        if ((Test-Path $cgDir) -and (Test-Path $dfGit)) {
            $tracked = git -C $DotfilesDir ls-tree -r HEAD claude-global 2>$null
            if (-not $tracked) {
                Remove-Item -Recurse -Force $cgDir
            }
        }
    }
}

Describe "Remove-ObsoleteClaudeLinks" {
    BeforeEach {
        $script:TempDir = Join-Path $env:TEMP "claude-links-test-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TempDir -Force | Out-Null
        $script:ClaudeDir = Join-Path $script:TempDir ".claude"
        New-Item -ItemType Directory -Path $script:ClaudeDir -Force | Out-Null
        $script:DotfilesBase = Join-Path $script:TempDir "dotfiles\claude-global"
        New-Item -ItemType Directory -Path $script:DotfilesBase -Force | Out-Null
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TempDir -ErrorAction SilentlyContinue
    }

    It "removes symlink whose target is under *\dotfiles\claude-global\*" {
        $target = Join-Path $script:DotfilesBase "CLAUDE.md"
        Set-Content -Path $target -Value "real"
        $link = Join-Path $script:ClaudeDir "CLAUDE.md"

        try {
            New-Item -ItemType SymbolicLink -Path $link -Target $target -ErrorAction Stop | Out-Null
        } catch {
            Set-ItResult -Skipped -Because "symlink creation failed (no Dev Mode or admin): $_"
            return
        }

        Remove-ObsoleteClaudeLinks -ClaudeDir $script:ClaudeDir

        Test-Path $link | Should -BeFalse
        # Target file (outside .claude) should not be touched
        Test-Path $target | Should -BeTrue
    }

    It "removes all 5 names when all are symlinks under dotfiles\claude-global" {
        $names = @("CLAUDE.md", "settings.json", "skills", "rules", "agents")
        $created = @{}
        foreach ($name in $names) {
            $target = Join-Path $script:DotfilesBase $name
            if ($name -in @("skills", "rules", "agents")) {
                New-Item -ItemType Directory -Path $target -Force | Out-Null
            } else {
                Set-Content -Path $target -Value "real"
            }
            $link = Join-Path $script:ClaudeDir $name
            try {
                New-Item -ItemType SymbolicLink -Path $link -Target $target -ErrorAction Stop | Out-Null
                $created[$name] = $link
            } catch {
                Set-ItResult -Skipped -Because "symlink creation failed (no Dev Mode or admin): $_"
                return
            }
        }

        Remove-ObsoleteClaudeLinks -ClaudeDir $script:ClaudeDir

        foreach ($name in $names) {
            $link = Join-Path $script:ClaudeDir $name
            Test-Path $link | Should -BeFalse
        }
    }

    It "keeps symlink whose target is under *\agents\claude-global\* (new repo)" {
        $agentsBase = Join-Path $script:TempDir "agents\claude-global"
        New-Item -ItemType Directory -Path $agentsBase -Force | Out-Null
        $target = Join-Path $agentsBase "CLAUDE.md"
        Set-Content -Path $target -Value "real"
        $link = Join-Path $script:ClaudeDir "CLAUDE.md"

        try {
            New-Item -ItemType SymbolicLink -Path $link -Target $target -ErrorAction Stop | Out-Null
        } catch {
            Set-ItResult -Skipped -Because "symlink creation failed (no Dev Mode or admin): $_"
            return
        }

        Remove-ObsoleteClaudeLinks -ClaudeDir $script:ClaudeDir

        Test-Path $link | Should -BeTrue
    }

    It "keeps regular file (not a symlink)" {
        $regular = Join-Path $script:ClaudeDir "CLAUDE.md"
        Set-Content -Path $regular -Value "user-edited content"

        Remove-ObsoleteClaudeLinks -ClaudeDir $script:ClaudeDir

        Test-Path $regular | Should -BeTrue
        $item = Get-Item $regular -Force
        ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) | Should -Be 0
    }

    It "does not error when claude dir does not exist" {
        $missing = Join-Path $script:TempDir "does-not-exist"
        { Remove-ObsoleteClaudeLinks -ClaudeDir $missing } | Should -Not -Throw
    }

    It "is idempotent — second run on same state does not throw" {
        $target = Join-Path $script:DotfilesBase "CLAUDE.md"
        Set-Content -Path $target -Value "real"
        $link = Join-Path $script:ClaudeDir "CLAUDE.md"

        try {
            New-Item -ItemType SymbolicLink -Path $link -Target $target -ErrorAction Stop | Out-Null
        } catch {
            Set-ItResult -Skipped -Because "symlink creation failed (no Dev Mode or admin): $_"
            return
        }

        { Remove-ObsoleteClaudeLinks -ClaudeDir $script:ClaudeDir } | Should -Not -Throw
        { Remove-ObsoleteClaudeLinks -ClaudeDir $script:ClaudeDir } | Should -Not -Throw
        Test-Path $link | Should -BeFalse
    }
}

Describe "Remove-ObsoleteDocAppendCmd" {
    BeforeEach {
        $script:TempDir = Join-Path $env:TEMP "doc-append-cmd-test-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TempDir -Force | Out-Null
        $script:CmdPath = Join-Path $script:TempDir "doc-append.cmd"
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TempDir -ErrorAction SilentlyContinue
    }

    It "removes doc-append.cmd referencing dotfiles\bin\doc-append.py" {
        Set-Content -Path $script:CmdPath -Value "@echo off`r`nuv run C:\git\dotfiles\bin\doc-append.py %*"

        Remove-ObsoleteDocAppendCmd -CmdPath $script:CmdPath

        Test-Path $script:CmdPath | Should -BeFalse
    }

    It "keeps doc-append.cmd referencing agents\bin\doc-append.py (new repo)" {
        Set-Content -Path $script:CmdPath -Value "@echo off`r`nuv run C:\git\agents\bin\doc-append.py %*"

        Remove-ObsoleteDocAppendCmd -CmdPath $script:CmdPath

        Test-Path $script:CmdPath | Should -BeTrue
    }

    It "does not error when file does not exist" {
        $missing = Join-Path $script:TempDir "missing.cmd"
        { Remove-ObsoleteDocAppendCmd -CmdPath $missing } | Should -Not -Throw
    }

    It "is idempotent — second run on same state does not throw" {
        Set-Content -Path $script:CmdPath -Value "@echo off`r`nuv run C:\git\dotfiles\bin\doc-append.py %*"

        { Remove-ObsoleteDocAppendCmd -CmdPath $script:CmdPath } | Should -Not -Throw
        { Remove-ObsoleteDocAppendCmd -CmdPath $script:CmdPath } | Should -Not -Throw
        Test-Path $script:CmdPath | Should -BeFalse
    }
}

Describe "Remove-ObsoleteHooksPath" {
    BeforeAll {
        $script:GitAvailable = $null -ne (Get-Command git -ErrorAction SilentlyContinue)
    }

    BeforeEach {
        if (-not $script:GitAvailable) {
            return
        }
        $script:TempDir = Join-Path $env:TEMP "hooks-path-test-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TempDir -Force | Out-Null
        $script:ConfigLocal = Join-Path $script:TempDir "config.local"
    }

    AfterEach {
        if ($script:TempDir -and (Test-Path $script:TempDir)) {
            Remove-Item -Recurse -Force $script:TempDir -ErrorAction SilentlyContinue
        }
    }

    It "unsets hooksPath when value matches *\dotfiles\claude-global\hooks (backslash)" {
        if (-not $script:GitAvailable) {
            Set-ItResult -Skipped -Because "git not available"
            return
        }
        $hooksPath = "C:\git\dotfiles\claude-global\hooks"
        New-Item -ItemType File -Path $script:ConfigLocal -Force | Out-Null
        git config --file $script:ConfigLocal core.hooksPath $hooksPath | Out-Null

        Remove-ObsoleteHooksPath -ConfigLocalPath $script:ConfigLocal

        $after = git config --file $script:ConfigLocal core.hooksPath 2>$null
        $after | Should -BeNullOrEmpty
    }

    It "unsets hooksPath when value matches */dotfiles/claude-global/hooks (forward-slash)" {
        if (-not $script:GitAvailable) {
            Set-ItResult -Skipped -Because "git not available"
            return
        }
        $hooksPath = "/c/git/dotfiles/claude-global/hooks"
        New-Item -ItemType File -Path $script:ConfigLocal -Force | Out-Null
        git config --file $script:ConfigLocal core.hooksPath $hooksPath | Out-Null

        Remove-ObsoleteHooksPath -ConfigLocalPath $script:ConfigLocal

        $after = git config --file $script:ConfigLocal core.hooksPath 2>$null
        $after | Should -BeNullOrEmpty
    }

    It "keeps hooksPath when value is unrelated (e.g., agents repo path)" {
        if (-not $script:GitAvailable) {
            Set-ItResult -Skipped -Because "git not available"
            return
        }
        $hooksPath = "C:\git\agents\claude-global\hooks"
        New-Item -ItemType File -Path $script:ConfigLocal -Force | Out-Null
        git config --file $script:ConfigLocal core.hooksPath $hooksPath | Out-Null

        Remove-ObsoleteHooksPath -ConfigLocalPath $script:ConfigLocal

        $after = git config --file $script:ConfigLocal core.hooksPath 2>$null
        $after | Should -Be $hooksPath
    }

    It "does not error when config.local has no hooksPath set" {
        if (-not $script:GitAvailable) {
            Set-ItResult -Skipped -Because "git not available"
            return
        }
        New-Item -ItemType File -Path $script:ConfigLocal -Force | Out-Null
        git config --file $script:ConfigLocal user.name "test" | Out-Null

        { Remove-ObsoleteHooksPath -ConfigLocalPath $script:ConfigLocal } | Should -Not -Throw
    }

    It "does not error when config.local file does not exist" {
        if (-not $script:GitAvailable) {
            Set-ItResult -Skipped -Because "git not available"
            return
        }
        $missing = Join-Path $script:TempDir "missing-config.local"
        { Remove-ObsoleteHooksPath -ConfigLocalPath $missing } | Should -Not -Throw
    }

    It "is idempotent — running twice does not throw" {
        if (-not $script:GitAvailable) {
            Set-ItResult -Skipped -Because "git not available"
            return
        }
        $hooksPath = "C:\git\dotfiles\claude-global\hooks"
        New-Item -ItemType File -Path $script:ConfigLocal -Force | Out-Null
        git config --file $script:ConfigLocal core.hooksPath $hooksPath | Out-Null

        { Remove-ObsoleteHooksPath -ConfigLocalPath $script:ConfigLocal } | Should -Not -Throw
        { Remove-ObsoleteHooksPath -ConfigLocalPath $script:ConfigLocal } | Should -Not -Throw
        $after = git config --file $script:ConfigLocal core.hooksPath 2>$null
        $after | Should -BeNullOrEmpty
    }
}

Describe "Remove-ObsoleteClaudeGlobalDir" {
    BeforeAll {
        $script:GitAvailable = $null -ne (Get-Command git -ErrorAction SilentlyContinue)
    }

    BeforeEach {
        $script:TempDir = Join-Path $env:TEMP "claude-global-dir-test-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TempDir -Force | Out-Null
        $script:DotfilesDir = Join-Path $script:TempDir "dotfiles"
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TempDir -ErrorAction SilentlyContinue
    }

    function New-DotfilesRepo {
        param([string]$Dir, [bool]$WithTrackedClaudeGlobal)
        New-Item -ItemType Directory -Path $Dir -Force | Out-Null
        git -C $Dir init -q | Out-Null
        git -C $Dir config user.email "test@example.com" | Out-Null
        git -C $Dir config user.name "Test" | Out-Null
        Set-Content -Path (Join-Path $Dir "README.md") -Value "init"
        git -C $Dir add README.md | Out-Null
        git -C $Dir commit -q -m "initial" | Out-Null
        if ($WithTrackedClaudeGlobal) {
            $cg = Join-Path $Dir "claude-global"
            New-Item -ItemType Directory -Path (Join-Path $cg "rules") -Force | Out-Null
            Set-Content -Path (Join-Path $cg "CLAUDE.md") -Value "tracked"
            Set-Content -Path (Join-Path $cg "rules\language.md") -Value "tracked"
            git -C $Dir add claude-global | Out-Null
            git -C $Dir commit -q -m "add claude-global" | Out-Null
        }
    }

    It "C1. removes untracked claude-global/ when HEAD does not track it" {
        if (-not $script:GitAvailable) { Set-ItResult -Skipped -Because "git not available"; return }
        New-DotfilesRepo -Dir $script:DotfilesDir -WithTrackedClaudeGlobal $false
        $cg = Join-Path $script:DotfilesDir "claude-global"
        New-Item -ItemType Directory -Path $cg -Force | Out-Null
        Set-Content -Path (Join-Path $cg "settings.json") -Value "leftover"

        Remove-ObsoleteClaudeGlobalDir -DotfilesDir $script:DotfilesDir

        Test-Path $cg | Should -BeFalse
    }

    It "C2. keeps claude-global/ when tracked by HEAD (pre-split state)" {
        if (-not $script:GitAvailable) { Set-ItResult -Skipped -Because "git not available"; return }
        New-DotfilesRepo -Dir $script:DotfilesDir -WithTrackedClaudeGlobal $true
        $cg = Join-Path $script:DotfilesDir "claude-global"

        Remove-ObsoleteClaudeGlobalDir -DotfilesDir $script:DotfilesDir

        Test-Path $cg | Should -BeTrue
        Test-Path (Join-Path $cg "CLAUDE.md") | Should -BeTrue
    }

    It "C3. does not error when claude-global/ does not exist" {
        if (-not $script:GitAvailable) { Set-ItResult -Skipped -Because "git not available"; return }
        New-DotfilesRepo -Dir $script:DotfilesDir -WithTrackedClaudeGlobal $false

        { Remove-ObsoleteClaudeGlobalDir -DotfilesDir $script:DotfilesDir } | Should -Not -Throw
    }

    It "C4. keeps claude-global/ when dotfiles is not a git repo" {
        New-Item -ItemType Directory -Path $script:DotfilesDir -Force | Out-Null
        $cg = Join-Path $script:DotfilesDir "claude-global"
        New-Item -ItemType Directory -Path $cg -Force | Out-Null
        Set-Content -Path (Join-Path $cg "settings.json") -Value "leftover"

        Remove-ObsoleteClaudeGlobalDir -DotfilesDir $script:DotfilesDir

        Test-Path $cg | Should -BeTrue
    }

    It "C5. recursively removes nested untracked content" {
        if (-not $script:GitAvailable) { Set-ItResult -Skipped -Because "git not available"; return }
        New-DotfilesRepo -Dir $script:DotfilesDir -WithTrackedClaudeGlobal $false
        $cg = Join-Path $script:DotfilesDir "claude-global"
        New-Item -ItemType Directory -Path (Join-Path $cg "rules") -Force | Out-Null
        New-Item -ItemType Directory -Path (Join-Path $cg "hooks\lib") -Force | Out-Null
        Set-Content -Path (Join-Path $cg "settings.json") -Value "x"
        Set-Content -Path (Join-Path $cg "rules\language.md") -Value "x"
        Set-Content -Path (Join-Path $cg "hooks\lib\workflow-state.js") -Value "x"

        Remove-ObsoleteClaudeGlobalDir -DotfilesDir $script:DotfilesDir

        Test-Path $cg | Should -BeFalse
    }
}
