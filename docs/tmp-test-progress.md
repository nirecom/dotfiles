# AutoHotkey Install Script - Test Progress

## Issue

`autohotkey.ps1` の AHK 検出ロジックが winget インストール済み環境で
"Installing AutoHotkey v2 via winget" → "既存のパッケージがすでにインストールされています"
と表示される。winget を呼ぶ前に検出できていない。

## Root Cause Investigation

現在のチェックロジック:
```powershell
$ahkExe = Join-Path $env:ProgramFiles "AutoHotkey\v2\AutoHotkey64.exe"
if (-not ((Get-Command AutoHotkey64 -ErrorAction SilentlyContinue) -or (Test-Path $ahkExe))) {
```

winget でインストールした場合、実際のインストールパスが
`$env:ProgramFiles\AutoHotkey\v2\AutoHotkey64.exe` と異なる可能性がある。

## Commands to Run on New Environment

### 1. Identify actual AHotkey install path

```powershell
# Check if AutoHotkey64 is on PATH
Get-Command AutoHotkey* -ErrorAction SilentlyContinue | Format-Table Name, Source

# Check standard install paths
Test-Path "$env:ProgramFiles\AutoHotkey\v2\AutoHotkey64.exe"
Test-Path "$env:ProgramFiles\AutoHotkey\v2\AutoHotkey32.exe"
Test-Path "$env:ProgramFiles\AutoHotkey\AutoHotkey64.exe"
Test-Path "${env:ProgramFiles(x86)}\AutoHotkey\v2\AutoHotkey64.exe"
Test-Path "$env:LOCALAPPDATA\Programs\AutoHotkey\v2\AutoHotkey64.exe"

# Find actual AutoHotkey exe location
Get-ChildItem -Path "$env:ProgramFiles\AutoHotkey" -Recurse -Filter "AutoHotkey*.exe" -ErrorAction SilentlyContinue | Select-Object FullName
Get-ChildItem -Path "${env:ProgramFiles(x86)}\AutoHotkey" -Recurse -Filter "AutoHotkey*.exe" -ErrorAction SilentlyContinue | Select-Object FullName
Get-ChildItem -Path "$env:LOCALAPPDATA\Programs\AutoHotkey" -Recurse -Filter "AutoHotkey*.exe" -ErrorAction SilentlyContinue | Select-Object FullName
```

### 2. Check winget detection alternative

```powershell
# winget list can check if a package is installed without re-installing
winget list --id AutoHotkey.AutoHotkey
```

### 3. Check .ahk file association (confirms AHK is usable)

```powershell
cmd /c assoc .ahk
cmd /c ftype AutoHotkeyScript
```

### 4. Idempotency re-test after fix

```powershell
.\install.ps1 -Full
# Should show "AutoHotkey v2 is already installed." without calling winget
```

## Proposed Fix Options

### Option A: Add more search paths
Check additional known install locations.

### Option B: Use winget list
```powershell
$wingetResult = winget list --id AutoHotkey.AutoHotkey 2>$null
if ($LASTEXITCODE -eq 0) {
    # already installed
}
```
Pros: Definitive check if installed via winget.
Cons: Slower (~1-2s), only works for winget-installed instances.

### Option C: Check .ahk file association
```powershell
$ahkAssoc = cmd /c assoc .ahk 2>$null
if ($ahkAssoc -match "AutoHotkey") {
    # already installed
}
```
Pros: Fast, works regardless of install method.
Cons: Association might not exist immediately after install.
