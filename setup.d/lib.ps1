$UserProperty = $(Get-ItemProperty 'HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders')

function CreateShortcut($shortcutPath, $targetPath, $workingDir) {
    $WsShell = New-Object -ComObject WScript.Shell
    $Shortcut = $WsShell.CreateShortcut("$shortcutPath")
    $Shortcut.TargetPath = "$targetPath"
    $Shortcut.WorkingDirectory = "$workingDir"
    $Shortcut.Save()
}
