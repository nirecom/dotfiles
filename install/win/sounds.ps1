# sounds.ps1 - Disable informational notification sounds
# Mutes: Notification.Default, SystemNotification, SystemAsterisk
# Keeps: SystemExclamation (warning), SystemHand (error)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$basePath = "HKCU:\AppEvents\Schemes\Apps\.Default"
$events = @(
    @{ Name = "Notification.Default"; Desc = "Toast notification default" }
    @{ Name = "SystemNotification";   Desc = "System notification" }
    @{ Name = "SystemAsterisk";       Desc = "Informational dialog" }
)

foreach ($event in $events) {
    $regPath = Join-Path $basePath "$($event.Name)\.Current"
    $current = (Get-ItemProperty -Path $regPath -ErrorAction SilentlyContinue).'(default)'
    if ($current) {
        Set-ItemProperty -Path $regPath -Name "(default)" -Value ""
        Write-Host "Muted: $($event.Desc) ($($event.Name))" -ForegroundColor Green
        Write-Host "  was: $current" -ForegroundColor DarkGray
    } else {
        Write-Host "Already muted: $($event.Desc) ($($event.Name))" -ForegroundColor DarkGray
    }
}
