#!/usr/bin/env -S powershell.exe -ExecutionPolicy Bypass -file

$scriptPath = $MyInvocation.MyCommand.Path
$nofilePath = Split-Path -Parent $scriptPath

. ($nofilePath + "\lib.ps1")

$shortcutPath = $UserProperty.Startup + "\main.ahk - ショートカット.lnk"
$targetPath = "$env:USERPROFILE\Documents\AutoHotKey\main.ahk"
$workingDir = "$env:USERPROFILE\Documents\AutoHotKey"

CreateShortcut "$shortcutPath" "$targetPath" "$workingDir"
