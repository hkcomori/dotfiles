;--------------------------------------------------------------------------------
; Auto-execute section
;--------------------------------------------------------------------------------
#Persistent
#SingleInstance, Force
#NoEnv
#UseHook
#HotkeyInterval, 2000
#MaxHotkeysPerInterval, 200
Process, Priority,, Realtime
SendMode, Input
SetWorkingDir %A_ScriptDir%
SetTitleMatchMode, 2

; Office
GroupAdd, office, ahk_exe WINWORD.EXE   ; Microsoft Word
GroupAdd, office, ahk_exe EXCEL.EXE     ; Microsoft Excel
GroupAdd, office, ahk_exe POWERPNT.EXE  ; Microsoft PowerPoint

; Browser
GroupAdd, browser, ahk_exe msedge.exe   ; Microsoft Edge
GroupAdd, browser, ahk_exe chrome.exe   ; Google Chrome
GroupAdd, browser, ahk_exe firefox.exe  ; Mozilla Firefox
GroupAdd, browser, ahk_exe vivaldi.exe  ; Vivaldi

; Fast scroll
fastScrollSensitivity := 10

; Detect AppsKey long press
AppsKeyPressedTime := 0
SetTimer detectAppsKeyLongPress, 100
detectAppsKeyLongPress()
{
    ListLines, Off
    global AppsKeyPressedTime
    If (GetKeyState("AppsKey", "P"))
        AppsKeyPressedTime++
}

; Auto reload this script
FileGetTime scriptModTime, %A_ScriptFullPath%
SetTimer CheckScriptUpdate, 1000
CheckScriptUpdate() {
    ListLines, Off
    global scriptModTime
    FileGetTime currentModTime, %A_ScriptFullPath%
    If (currentModTime == scriptModTime)
        return
    SetTimer CheckScriptUpdate, Off
    Loop
    {
        reload
        Sleep 300 ; ms
        MsgBox 0x2, %A_ScriptName%, Reload failed. ; 0x2 = Abort/Retry/Ignore
        IfMsgBox Abort
            ExitApp
        IfMsgBox Ignore
            break
    } ; loops reload on "Retry"
}

;--------------------------------------------------------------------------------
; Global
;--------------------------------------------------------------------------------
; Disable Insert
Insert:: Return

; Emulate Fn-key of RealForce by AppsKey
AppsKey Up::
    If (AppsKeyPressedTime < 3)
        Send, {AppsKey}
    AppsKeyPressedTime := 0
    Return
AppsKey & Left:: Send, {Volume_Mute}
AppsKey & Down:: Send, {Volume_Down}
AppsKey & Right:: Send, {Volume_Up}
AppsKey & Up:: Send, {Media_Play_Pause}

; Fast scroll
AppsKey & WheelUp:: Send, {WheelUp %fastScrollSensitivity%}
AppsKey & WheelDown:: Send, {WheelDown %fastScrollSensitivity%}

; Turn off IME when opening start menu
~LWin Up::
~RWin Up::
    If (A_PriorKey = "LWin" || A_PriorKey = "RWin"){
        Sleep, 100
        ime_off(WinExist("A"))
    }
    Return

;--------------------------------------------------------------------------------
; Explorer
;--------------------------------------------------------------------------------
#IfWinActive ahk_class CabinetWClass ahk_exe Explorer.EXE
~^f::
~^l::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive

;--------------------------------------------------------------------------------
; Task view
;--------------------------------------------------------------------------------
#IfWinActive ahk_class Windows.UI.Core.CoreWindow ahk_exe Explorer.EXE
~^f::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive

;--------------------------------------------------------------------------------
; Outlook
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe OUTLOOK.EXE
~^e::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive

;--------------------------------------------------------------------------------
; Browser
;--------------------------------------------------------------------------------
#IfWinActive ahk_group browser
+WheelDown::WheelRight
+WheelUp::WheelLeft

~^e::
~^l::
~^t::
~F1::
~^+p::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive
