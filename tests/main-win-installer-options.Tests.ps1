# tests/main-win-installer-options.Tests.ps1
# Pester tests for install.ps1 option reorganization (-Base, -Develop, -Toolchain, -Full)

BeforeAll {
    $script:InstallScript = "$PSScriptRoot\..\install.ps1"
    $script:Content = Get-Content $script:InstallScript -Raw
}

Describe "install.ps1 parameter definitions" {
    It "accepts -Base switch" {
        $params = (Get-Command $script:InstallScript).Parameters
        $params.ContainsKey('Base') | Should -BeTrue
        $params['Base'].ParameterType | Should -Be ([switch])
    }
    It "accepts -Develop switch" {
        $params = (Get-Command $script:InstallScript).Parameters
        $params.ContainsKey('Develop') | Should -BeTrue
        $params['Develop'].ParameterType | Should -Be ([switch])
    }
    It "accepts -Toolchain switch" {
        $params = (Get-Command $script:InstallScript).Parameters
        $params.ContainsKey('Toolchain') | Should -BeTrue
        $params['Toolchain'].ParameterType | Should -Be ([switch])
    }
    It "accepts -Full switch" {
        $params = (Get-Command $script:InstallScript).Parameters
        $params.ContainsKey('Full') | Should -BeTrue
        $params['Full'].ParameterType | Should -Be ([switch])
    }
}

Describe "install.ps1 always-run scripts" {
    It "runs dotfileslink.ps1 unconditionally" {
        $script:Content | Should -Match 'dotfileslink\.ps1'
    }
    It "runs claude-code.ps1 unconditionally (outside conditional blocks)" {
        # claude-code.ps1 should appear before any if ($Base / $Develop / $Full) block
        $lines = (Get-Content $script:InstallScript)
        $claudeCodeLine = ($lines | Select-String 'claude-code\.ps1').LineNumber | Select-Object -First 1
        $firstIfLine = ($lines | Select-String '^\s*if\s*\(\$(?:Base|Develop|Toolchain|Full)\b').LineNumber | Select-Object -First 1
        $claudeCodeLine | Should -BeLessThan $firstIfLine
    }
    It "runs install-obsolete.ps1 unconditionally" {
        $lines = (Get-Content $script:InstallScript)
        $obsoleteLine = ($lines | Select-String 'install-obsolete\.ps1').LineNumber | Select-Object -First 1
        $firstIfLine = ($lines | Select-String '^\s*if\s*\(\$(?:Base|Develop|Toolchain|Full)\b').LineNumber | Select-Object -First 1
        $obsoleteLine | Should -BeLessThan $firstIfLine
    }
    It "runs sounds.ps1 unconditionally" {
        $lines = (Get-Content $script:InstallScript)
        $soundsLine = ($lines | Select-String 'sounds\.ps1').LineNumber | Select-Object -First 1
        $firstIfLine = ($lines | Select-String '^\s*if\s*\(\$(?:Base|Develop|Toolchain|Full)\b').LineNumber | Select-Object -First 1
        $soundsLine | Should -BeLessThan $firstIfLine
    }
    It "runs fnm.ps1 unconditionally (Node.js required for Claude Code hooks)" {
        $lines = (Get-Content $script:InstallScript)
        $fnmLine = ($lines | Select-String 'fnm\.ps1').LineNumber | Select-Object -First 1
        $firstIfLine = ($lines | Select-String '^\s*if\s*\(\$(?:Base|Develop|Toolchain|Full)\b').LineNumber | Select-Object -First 1
        $fnmLine | Should -BeLessThan $firstIfLine
    }
}

Describe "install.ps1 -Base conditional block" {
    It "includes starship.ps1 in Base/Full block" {
        $script:Content | Should -Match '\$Base\s+-or\s+\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)[\s\S]*?starship\.ps1'
    }
    It "includes uv.ps1 in Base/Full block" {
        $script:Content | Should -Match '\$Base\s+-or\s+\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)[\s\S]*?uv\.ps1'
    }
    It "includes rize.ps1 in Base/Full block" {
        $script:Content | Should -Match '\$Base\s+-or\s+\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)[\s\S]*?rize\.ps1'
    }
    It "includes claude-usage-widget.ps1 in Base/Full block" {
        $script:Content | Should -Match '\$Base\s+-or\s+\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)[\s\S]*?claude-usage-widget\.ps1'
    }
    It "does NOT include fnm.ps1 in Base/Full block" {
        # Extract content between Base block start and its closing brace
        $baseBlock = [regex]::Match($script:Content, 'if\s*\(\$Base\s+-or\s+\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)\s*\{([\s\S]*?)\n\}').Groups[1].Value
        $baseBlock | Should -Not -Match 'fnm\.ps1'
    }
}

Describe "install.ps1 -Develop conditional block" {
    It "does NOT include fnm.ps1 in Develop/Full block (moved to always-run)" {
        $devBlock = [regex]::Match($script:Content, 'if\s*\(\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)\s*\{([\s\S]*?)\n\}').Groups[1].Value
        $devBlock | Should -Not -Match 'fnm\.ps1'
    }
    It "includes awscli.ps1 in Develop/Full block" {
        $script:Content | Should -Match '\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)[\s\S]*?awscli\.ps1'
    }
    It "does NOT include vs-cpp.ps1 in Develop/Full block" {
        $devBlock = [regex]::Match($script:Content, 'if\s*\(\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)\s*\{([\s\S]*?)\n\}').Groups[1].Value
        $devBlock | Should -Not -Match 'vs-cpp\.ps1'
    }
    It "does NOT include starship.ps1 in Develop/Full block" {
        $devBlock = [regex]::Match($script:Content, 'if\s*\(\$Develop\s+-or\s+\$Toolchain\s+-or\s+\$Full\)\s*\{([\s\S]*?)\n\}').Groups[1].Value
        $devBlock | Should -Not -Match 'starship\.ps1'
    }
}

Describe "install.ps1 -Toolchain conditional block" {
    It "includes vs-cpp.ps1 in Toolchain/Full block" {
        $script:Content | Should -Match '\$Toolchain\s+-or\s+\$Full\)[\s\S]*?vs-cpp\.ps1'
    }
    It "does NOT include awscli.ps1 in Toolchain/Full block" {
        $tcBlock = [regex]::Match($script:Content, 'if\s*\(\$Toolchain\s+-or\s+\$Full\)\s*\{([\s\S]*?)\n\}').Groups[1].Value
        $tcBlock | Should -Not -Match 'awscli\.ps1'
    }
}
