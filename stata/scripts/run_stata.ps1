param(
    [Parameter(Mandatory = $true)][string]$StataExe,
    [Parameter(Mandatory = $true)][string]$DoFile,
    [Parameter(Mandatory = $true)][string]$WorkDir
)

if (-not $env:ComSpec) {
    $env:ComSpec = Join-Path $env:SystemRoot "System32\cmd.exe"
}
if (-not $env:TEMP) {
    $localAppData = [Environment]::GetFolderPath("LocalApplicationData")
    $env:TEMP = Join-Path $localAppData "Temp"
}
if (-not $env:TMP) {
    $env:TMP = $env:TEMP
}

Start-Process `
    -FilePath $StataExe `
    -ArgumentList "/e", "do", "`"$DoFile`"" `
    -Wait `
    -WorkingDirectory $WorkDir
