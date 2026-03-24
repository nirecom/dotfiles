# snipping-tool.ps1 - Disable Snipping Tool notifications
# Prevents the "Snipping Tool" toast from appearing when copying images

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$regPath = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Notifications\Settings\Microsoft.ScreenSketch_8wekyb3d8bbwe!App'

# Create key if it doesn't exist (Snipping Tool never used or freshly installed)
if (-not (Test-Path $regPath)) {
    New-Item -Path $regPath -Force | Out-Null
}

$props = Get-ItemProperty $regPath -ErrorAction SilentlyContinue
$current = $props.PSObject.Properties['Enabled']

if ($current -and $current.Value -eq 0) {
    Write-Host "Already disabled: Snipping Tool notifications" -ForegroundColor DarkGray
} else {
    Set-ItemProperty -Path $regPath -Name 'Enabled' -Value 0 -Type DWord
    if ($current) {
        Write-Host "Disabled: Snipping Tool notifications (was: Enabled=$($current.Value))" -ForegroundColor Green
    } else {
        Write-Host "Disabled: Snipping Tool notifications" -ForegroundColor Green
    }
}
