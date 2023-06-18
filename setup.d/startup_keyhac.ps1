$scriptPath = $MyInvocation.MyCommand.Path
$nofilePath = Split-Path -Parent $scriptPath

. ($nofilePath + "\lib.ps1")

$shortcutPath = $UserProperty.Startup + "\keyhac.exe - �V���[�g�J�b�g.lnk"
$targetPath = $Env:OneDrive + "\Apps\Keyhac\keyhac.exe"
$workingDir = [Environment]::GetFolderPath("MyDocuments") + "\Keyhac"

CreateShortcut "$shortcutPath" "$targetPath" "$workingDir"
