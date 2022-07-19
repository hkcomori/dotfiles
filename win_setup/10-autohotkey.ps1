#!/usr/bin/env -S powershell.exe -ExecutionPolicy RemoteSigned -file

$scriptPath = $MyInvocation.MyCommand.Path
$nofilePath = Split-Path -Parent $scriptPath

. ($nofilePath + "\lib.ps1")

$shortcutPath = $UserProperty.Startup + "\init.ahk - ショートカット.lnk"
$targetPath = "$env:USERPROFILE\Documents\AutoHotKey\init.ahk"
$workingDir = "$env:USERPROFILE\Documents\AutoHotKey"

CreateShortcut "$shortcutPath" "$targetPath" "$workingDir"
