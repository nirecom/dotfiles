# Starship Install Guard - Test Progress

## Fix Applied

`install/win/starship.ps1` に AutoHotkey と同じ二重チェックパターンを適用済み。

```powershell
$starshipExe = Join-Path $env:ProgramFiles "starship\bin\starship.exe"
if (-not ((Get-Command starship -ErrorAction SilentlyContinue) -or (Test-Path $starshipExe))) {
```

## Commands to Run on Target Environment

### 1. Verify starship install path

```powershell
# Check actual install location
where.exe starship
Get-Command starship -ErrorAction SilentlyContinue | Format-Table Name, Source

# Check assumed path
Test-Path "$env:ProgramFiles\starship\bin\starship.exe"

# Search if path differs
Get-ChildItem -Path "$env:ProgramFiles\starship" -Recurse -Filter "starship.exe" -ErrorAction SilentlyContinue | Select-Object FullName
Get-ChildItem -Path "$env:LOCALAPPDATA\Programs" -Recurse -Filter "starship.exe" -ErrorAction SilentlyContinue | Select-Object FullName
```

### 2. Idempotency test

```powershell
.\install.ps1 -Full
# Should show "Starship is already installed." without calling winget
```

### 3. If path differs

Update `$starshipExe` in `install/win/starship.ps1` to match the actual path.
