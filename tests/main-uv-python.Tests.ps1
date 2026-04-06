# Tests for uv python install step
Describe 'uv python install' {
    BeforeAll {
        $root = Split-Path -Parent $PSScriptRoot
    }

    It 'uv.ps1 contains uv python install' {
        $content = Get-Content (Join-Path $root 'install\win\uv.ps1') -Raw
        $content | Should -Match 'uv python install'
    }

    It 'uv.sh contains uv python install' {
        $content = Get-Content (Join-Path $root 'install\linux\uv.sh') -Raw
        $content | Should -Match 'uv python install'
    }

    It 'uv.ps1 checks if python is already installed before installing' {
        $content = Get-Content (Join-Path $root 'install\win\uv.ps1') -Raw
        $content | Should -Match 'uv python list'
    }

    It 'uv.sh checks if python is already installed before installing' {
        $content = Get-Content (Join-Path $root 'install\linux\uv.sh') -Raw
        $content | Should -Match 'uv python list'
    }
}
