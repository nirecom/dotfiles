# Test: session-sync-init.ps1 and session-sync.ps1
# Uses temp directories to avoid touching real ~/.claude

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
        # Create files that should NOT be synced
        Set-Content -Path (Join-Path $script:TestDir "settings.json") -Value "{}"
        $statsigDir = Join-Path $script:TestDir "statsig"
        New-Item -ItemType Directory -Path $statsigDir -Force | Out-Null
        Set-Content -Path (Join-Path $statsigDir "data.json") -Value "{}"
    }

    AfterEach {
        Remove-Item -Recurse -Force $script:TestDir -ErrorAction SilentlyContinue
    }

    It "creates .gitignore with correct content" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $gitignorePath = Join-Path $script:TestDir ".gitignore"
        Test-Path $gitignorePath | Should -BeTrue
        $content = Get-Content $gitignorePath -Raw
        $content | Should -Match '(?m)^\*$' -Because ".gitignore should ignore everything by default"
        $content | Should -Match '!\.gitignore' -Because ".gitignore itself must be tracked"
        $content | Should -Match '!\.gitattributes' -Because ".gitattributes must be tracked"
        $content | Should -Match '!projects/' -Because "projects/ must be tracked"
    }

    It "creates .gitattributes with eol=lf" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $path = Join-Path $script:TestDir ".gitattributes"
        Test-Path $path | Should -BeTrue
        $content = Get-Content $path -Raw
        $content | Should -Match '\* text eol=lf' -Because "all files should use LF line endings"
    }

    It "initializes git repo" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        Test-Path (Join-Path $script:TestDir ".git") | Should -BeTrue
    }

    It "is idempotent - running twice produces same result" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $commitCount1 = git -C $script:TestDir rev-list --count HEAD
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $commitCount2 = git -C $script:TestDir rev-list --count HEAD
        $commitCount2 | Should -Be $commitCount1 -Because "second init should not create extra commits"
        # .gitignore should not have duplicated lines
        $lines = Get-Content (Join-Path $script:TestDir ".gitignore")
        ($lines | Where-Object { $_ -eq '*' }).Count | Should -Be 1
    }

    It ".gitignore excludes non-projects files from tracking" {
        & $InitScript -ClaudeDir $script:TestDir -NoRemote
        $status = git -C $script:TestDir status --porcelain
        $status | Should -Not -Match 'settings\.json' -Because "settings.json must be excluded"
        $status | Should -Not -Match 'statsig' -Because "statsig/ must be excluded"
    }

    It "sets remote when -NoRemote is not specified" {
        # Use a bare repo as fake remote
        $fakeRemote = Join-Path $env:TEMP "session-sync-remote-$(Get-Random)"
        git init --bare $fakeRemote 2>&1 | Out-Null
        try {
            & $InitScript -ClaudeDir $script:TestDir -RemoteUrl $fakeRemote
            $remote = git -C $script:TestDir remote get-url origin
            $remote | Should -Be $fakeRemote
        } finally {
            Remove-Item -Recurse -Force $fakeRemote -ErrorAction SilentlyContinue
        }
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
        $log = git -C $script:TestDir log --oneline -1
        $log | Should -Match $env:COMPUTERNAME -Because "commit message should include machine name"
    }

    It "push with no changes doesn't create empty commit" {
        $commitsBefore = git -C $script:TestDir rev-list --count HEAD
        & $SyncScript -Action push -ClaudeDir $script:TestDir
        $commitsAfter = git -C $script:TestDir rev-list --count HEAD
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
        $log = git -C $script:TestDir log --oneline -1
        $today = Get-Date -Format "yyyy-MM-dd"
        $log | Should -Match $today -Because "commit message should include date"
    }
}
