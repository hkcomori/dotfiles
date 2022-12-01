$scriptPath = $MyInvocation.MyCommand.Path
$nofilePath = Split-Path -Parent $scriptPath

. ($nofilePath + "\lib.ps1")

$shortcutPath = $UserProperty.Startup + "\main.ahk - ショートカット.lnk"
$targetPath = [Environment]::GetFolderPath("MyDocuments") + "\AutoHotKey\main.ahk"
$workingDir = [Environment]::GetFolderPath("MyDocuments") + "\AutoHotKey"

CreateShortcut "$shortcutPath" "$targetPath" "$workingDir"
