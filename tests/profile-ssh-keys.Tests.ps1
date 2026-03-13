# Tests for SSH key discovery logic in install/win/profile.ps1 (lines 12-17)
# Verifies that Get-ChildItem glob finds non-standard key names and excludes .pub files

Describe "SSH key discovery" {
    BeforeAll {
        $tempSSH = Join-Path $TestDrive ".ssh"
        New-Item -ItemType Directory -Path $tempSSH -Force | Out-Null

        # Extract the key-discovery logic as a testable function
        function Get-SSHPrivateKeys($sshDir) {
            Get-ChildItem "$sshDir\id_*" -File -ErrorAction SilentlyContinue |
                Where-Object { $_.Extension -ne '.pub' }
        }
    }

    Context "Normal cases" {
        It "finds standard id_ed25519 key" {
            Set-Content "$tempSSH\id_ed25519" "fake-key"
            Set-Content "$tempSSH\id_ed25519.pub" "fake-pub"
            $keys = Get-SSHPrivateKeys $tempSSH
            $keys.Name | Should -Contain "id_ed25519"
            $keys.Name | Should -Not -Contain "id_ed25519.pub"
        }

        It "finds non-standard key name like id_ed25519_20170106" {
            Set-Content "$tempSSH\id_ed25519_20170106" "fake-key"
            Set-Content "$tempSSH\id_ed25519_20170106.pub" "fake-pub"
            $keys = Get-SSHPrivateKeys $tempSSH
            $keys.Name | Should -Contain "id_ed25519_20170106"
            $keys.Name | Should -Not -Contain "id_ed25519_20170106.pub"
        }

        It "finds id_rsa key" {
            Set-Content "$tempSSH\id_rsa" "fake-key"
            Set-Content "$tempSSH\id_rsa.pub" "fake-pub"
            $keys = Get-SSHPrivateKeys $tempSSH
            $keys.Name | Should -Contain "id_rsa"
            $keys.Name | Should -Not -Contain "id_rsa.pub"
        }

        It "finds multiple keys at once" {
            $keys = Get-SSHPrivateKeys $tempSSH
            ($keys | Measure-Object).Count | Should -BeGreaterOrEqual 3
        }
    }

    Context "Abnormal cases" {
        It "returns nothing when .ssh directory is empty" {
            $emptySSH = Join-Path $TestDrive ".ssh-empty"
            New-Item -ItemType Directory -Path $emptySSH -Force | Out-Null
            $keys = Get-SSHPrivateKeys $emptySSH
            $keys | Should -BeNullOrEmpty
        }

        It "returns nothing when only .pub files exist" {
            $pubOnly = Join-Path $TestDrive ".ssh-pubonly"
            New-Item -ItemType Directory -Path $pubOnly -Force | Out-Null
            Set-Content "$pubOnly\id_ed25519.pub" "fake-pub"
            $keys = Get-SSHPrivateKeys $pubOnly
            $keys | Should -BeNullOrEmpty
        }

        It "ignores non-key files (known_hosts, config, authorized_keys)" {
            Set-Content "$tempSSH\known_hosts" "host-entry"
            Set-Content "$tempSSH\config" "Host *"
            Set-Content "$tempSSH\authorized_keys" "key-entry"
            $keys = Get-SSHPrivateKeys $tempSSH
            $keys.Name | Should -Not -Contain "known_hosts"
            $keys.Name | Should -Not -Contain "config"
            $keys.Name | Should -Not -Contain "authorized_keys"
        }

        It "does not error when directory does not exist" {
            $keys = Get-SSHPrivateKeys "$TestDrive\.ssh-nonexistent"
            $keys | Should -BeNullOrEmpty
        }
    }
}
