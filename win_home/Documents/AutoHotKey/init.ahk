;--------------------------------------------------------------------------------
; Auto-execute section
;--------------------------------------------------------------------------------
#Persistent
#SingleInstance, Force
#NoEnv
#UseHook
#InstallKeybdHook
#InstallMouseHook
#HotkeyInterval, 2000
#MaxHotkeysPerInterval, 200
Process, Priority,, Realtime
SendMode, Input
SetWorkingDir %A_ScriptDir%
SetTitleMatchMode, 2

; Get environment variables
EnvGet, A_UserProfile, USERPROFILE

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

; Detect long press
key_startDetectLongPress("vk1D")    ; Muhenkan
key_startDetectLongPress("vk1C")    ; Henkan
key_startDetectLongPress("vkF2")    ; Kana
key_startDetectLongPress("AppsKey")

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

; Use CapsLock as Ctrl
CapsLock:: LCtrl

; Global hot keys for Explorer
#e::Run, %A_UserProfile%\Downloads

#z:: Winset, AlwaysOnTop, Toggle, A

~*WheelDown:: mouse_activateUnderCursor()
~*WheelUp:: mouse_activateUnderCursor()
~*WheelRight:: mouse_activateUnderCursor()
~*WheelLeft:: mouse_activateUnderCursor()
+WheelDown:: WheelRight
+WheelUp:: WheelLeft

vk1D Up::   ; Muhenkan
    If !key_isLongPressed("vk1D", True)
        ime_off(WinExist("A"))
    Return

vk1C Up::   ; Henkan
    If !key_isLongPressed("vk1C", True)
        ime_on(WinExist("A"))
    Return

vkF2 Up::   ; Kana
    If !key_isLongPressed("vkF2", True)
        ime_on(WinExist("A"))
    Return

; Emulate Fn-key of RealForce by AppsKey
AppsKey Up::
    If !key_isLongPressed("AppsKey", True)
        Send, {AppsKey}
    Return
AppsKey & Left:: Send, {Volume_Mute}
AppsKey & Down:: Send, {Volume_Down}
AppsKey & Right:: Send, {Volume_Up}
AppsKey & Up:: Send, {Media_Play_Pause}

; Fast scroll
AppsKey & WheelUp::
AppsKey & WheelDown::
    mouse_activateUnderCursor()
    key := StrSplit(A_ThisHotkey, "&", " ")[2]
    Send, {%key% %fastScrollSensitivity%}
    Return

; Turn off IME when opening start menu
~LWin Up::
~RWin Up::
    If (A_PriorKey = "LWin" || A_PriorKey = "RWin"){
        Sleep, 100
        ime_off(WinExist("A"))
    }
    Return

; Macro board
AppsKey & Insert:: Send, {F13}
AppsKey & Home:: Send, {F14}
AppsKey & PgUp:: Send, {F15}
AppsKey & Delete:: Send, {F16}
AppsKey & End:: Send, {F17}
AppsKey & PgDn:: Send, {F18}

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
~^e::
~^f::
~^l::
~^t::
~F1::
~F3::
~+F3::
~^+p::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive
