# vs-cpp.ps1 - Install Visual Studio 2022 Community with C++ Desktop workload
# Usage: Called by install.ps1 -Develop

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$vswherePath = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"

function Find-VsCpp {
    if (-not (Test-Path $vswherePath)) { return $null }
    $result = & $vswherePath -version "[17.0,18.0)" -requires Microsoft.VisualStudio.Workload.NativeDesktop -property installationPath 2>&1
    if ($LASTEXITCODE -eq 0 -and $result) { return $result.Trim() }
    return $null
}

$vsPath = Find-VsCpp

if ($vsPath) {
    Write-Host "VS 2022 with C++ workload already installed: $vsPath" -ForegroundColor DarkGray
    return
}

# Elevate to admin if needed
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    Write-Host "Requesting administrator privileges to install VS 2022 Community..." -ForegroundColor Yellow
    Write-Host "Installing in elevated window... this may take 10+ minutes." -ForegroundColor DarkGray
    try {
        $proc = Start-Process powershell -ArgumentList "-ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs -Wait -PassThru
    } catch {
        Write-Host "VS 2022 installation skipped (UAC declined)." -ForegroundColor DarkGray
        return
    }
    if ($proc.ExitCode -ne 0) {
        Write-Error "VS 2022 installation failed (exit code: $($proc.ExitCode))"
    }
    return
}

# Download bootstrapper
$bootstrapper = Join-Path $env:TEMP "vs_community.exe"
Write-Host "Downloading VS 2022 Community bootstrapper..."
try {
    Invoke-WebRequest -Uri "https://aka.ms/vs/17/release/vs_community.exe" -OutFile $bootstrapper -UseBasicParsing
} catch {
    Write-Error "Download failed: $_"
}

# Run installer
Write-Host "Installing VS 2022 with C++ Desktop workload (this may take a while)..."
$proc = Start-Process -FilePath $bootstrapper -ArgumentList "--add Microsoft.VisualStudio.Workload.NativeDesktop --includeRecommended --passive --norestart" -Wait -PassThru
$exitCode = $proc.ExitCode

# Clean up bootstrapper
Remove-Item $bootstrapper -Force -ErrorAction SilentlyContinue

if ($exitCode -ne 0 -and $exitCode -ne 3010) {
    Write-Error "VS 2022 installer exited with code $exitCode"
}

# Poll for installation completion (installer may spawn background processes)
$vsPath = $null
for ($i = 1; $i -le 5; $i++) {
    $vsPath = Find-VsCpp
    if ($vsPath) { break }
    Write-Host "Waiting for installation to finalize ($i/5)..."
    Start-Sleep -Seconds 10
}

if (-not $vsPath) {
    Write-Error "VS 2022 installer finished but vswhere cannot detect it"
}

Write-Host "VS 2022 installed: $vsPath" -ForegroundColor Green
if ($exitCode -eq 3010) {
    Write-Host "Reboot recommended to complete setup." -ForegroundColor Yellow
}
