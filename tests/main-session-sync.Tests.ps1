# Test: session-sync-init.ps1 and session-sync.ps1
# Uses temp directories to avoid touching real ~/.claude
# Git root is at ~/.claude/projects/ (not ~/.claude/)

BeforeAll {
    $DotfilesDir = Split-Path -Parent $PSScriptRoot
    $InitScript = Join-Path (Join-Path $DotfilesDir "install") "win\session-sync-init.ps1"
    $SyncScript = Join-Path $DotfilesDir "bin\session-sync.ps1"
}

Describe "session-sync-init.ps1" {
    BeforeEach {
        $script:TestDir = Join-Path $env:TEMP "session-sync-test-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TestDir -Force | Out-Null
        # Create fake projects/ structure
        $projDir = Join-Path $script:TestDir "projects\C--LLM-ai-specs"
        New-Item -ItemType Directory -Path $projDir -Force | Out-Null
        Set-Content -Path (Join-Path $projDir "session.jsonl") -Value "test"
        # Create files outside projects/ that should NOT be in git
        Set-Content -Path (Join-Path $script:TestDir "settings.json") -Value "{}"
        $statsigDir = Join-Path $script:TestDir "statsig"
        New-Item -ItemType Directory -Path $statsigDir -Force | Out-Null
        Set-Content -Path (Join-Path $statsigDir "data.json") -Value "{}"
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TestDir -ErrorAction SilentlyContinue
    }

    It "initializes git repo inside projects/ not claude root" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $projGit = Join-Path $script:TestDir "projects\.git"
        Test-Path $projGit | Should -BeTrue -Because "git root should be in projects/"
        $rootGit = Join-Path $script:TestDir ".git"
        Test-Path $rootGit | Should -BeFalse -Because "git root should NOT be in claude dir"
    }

    It "creates .gitattributes with eol=lf in projects/" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $path = Join-Path $script:TestDir "projects\.gitattributes"
        Test-Path $path | Should -BeTrue
        $content = Get-Content $path -Raw
        $content | Should -Match '\* text eol=lf'
    }

    It "is idempotent - running twice produces same result" {
        $projDir = Join-Path $script:TestDir "projects"
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $commitCount1 = git -C $projDir rev-list --count HEAD
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $commitCount2 = git -C $projDir rev-list --count HEAD
        $commitCount2 | Should -Be $commitCount1 -Because "second init should not create extra commits"
    }

    It "files outside projects/ are not tracked" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $projDir = Join-Path $script:TestDir "projects"
        $tracked = git -C $projDir ls-files
        $tracked | Should -Not -Match 'settings\.json' -Because "settings.json is outside git root"
        $tracked | Should -Not -Match 'statsig' -Because "statsig/ is outside git root"
    }

    It "sets remote when -NoRemote is not specified" {
        $fakeRemote = Join-Path $env:TEMP "session-sync-remote-$(Get-Random)"
        git init --bare $fakeRemote 2>&1 | Out-Null
        try {
            & $InitScript -ClaudeDir $script:TestDir -RemoteUrl $fakeRemote
            $projDir = Join-Path $script:TestDir "projects"
            $remote = git -C $projDir remote get-url origin
            $remote | Should -Be $fakeRemote
        } finally {
            Remove-Item -Recurse -Force $fakeRemote -ErrorAction SilentlyContinue
        }
    }

    It "migrates old git root from claude dir to projects/" {
        # Simulate old layout: .git at claude root
        git init $script:TestDir 2>&1 | Out-Null
        Set-Content -Path (Join-Path $script:TestDir ".gitignore") -Value "*`n!projects/"
        git -C $script:TestDir add .gitignore
        git -C $script:TestDir commit -m "old layout" 2>&1 | Out-Null

        & $InitScript -ClaudeDir $script:TestDir -NoRemote

        # Old .git should be gone
        Test-Path (Join-Path $script:TestDir ".git") | Should -BeFalse
        # New .git should exist in projects/
        Test-Path (Join-Path $script:TestDir "projects\.git") | Should -BeTrue
    }
}

Describe "session-sync.ps1" {
    BeforeEach {
        $script:TestDir = Join-Path $env:TEMP "session-sync-test-$(Get-Random)"
        $script:RemoteDir = Join-Path $env:TEMP "session-sync-remote-$(Get-Random)"
        New-Item -ItemType Directory -Path $script:TestDir -Force | Out-Null
        git init --bare $script:RemoteDir 2>&1 | Out-Null
        # Initialize via init script
        & $InitScript -ClaudeDir $script:TestDir -RemoteUrl $script:RemoteDir
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TestDir -ErrorAction SilentlyContinue
        Remove-Item -Recurse -Force $script:RemoteDir -ErrorAction SilentlyContinue
    }

    It "push commits with machine name in message" {
        $projDir = Join-Path $script:TestDir "projects\test-proj"
        New-Item -ItemType Directory -Path $projDir -Force | Out-Null
        Set-Content -Path (Join-Path $projDir "session.jsonl") -Value "data"
        & $SyncScript -Action push -ClaudeDir $script:TestDir
        $gitDir = Join-Path $script:TestDir "projects"
        $log = git -C $gitDir log --oneline -1
        $log | Should -Match $env:COMPUTERNAME -Because "commit message should include machine name"
    }

    It "push with no changes doesn't create empty commit" {
        $gitDir = Join-Path $script:TestDir "projects"
        $commitsBefore = git -C $gitDir rev-list --count HEAD
        & $SyncScript -Action push -ClaudeDir $script:TestDir
        $commitsAfter = git -C $gitDir rev-list --count HEAD
        $commitsAfter | Should -Be $commitsBefore
    }

    It "pull uses --rebase" {
        $content = Get-Content $SyncScript -Raw
        $content | Should -Match 'pull\s.*--rebase' -Because "pull must use --rebase to avoid merge commits"
    }

    It "status runs without error" {
        $output = & $SyncScript -Action status -ClaudeDir $script:TestDir 2>&1
        $LASTEXITCODE | Should -BeIn @(0, $null)
    }

    It "push checks for Claude Code process" {
        $content = Get-Content $SyncScript -Raw
        $content | Should -Match 'claude' -Because "push should check for running Claude Code process"
    }

    It "commit message includes timestamp" {
        $projDir = Join-Path $script:TestDir "projects\test-proj2"
        New-Item -ItemType Directory -Path $projDir -Force | Out-Null
        Set-Content -Path (Join-Path $projDir "data.jsonl") -Value "test"
        & $SyncScript -Action push -ClaudeDir $script:TestDir
        $gitDir = Join-Path $script:TestDir "projects"
        $log = git -C $gitDir log --oneline -1
        $today = Get-Date -Format "yyyy-MM-dd"
        $log | Should -Match $today -Because "commit message should include date"
    }
}
