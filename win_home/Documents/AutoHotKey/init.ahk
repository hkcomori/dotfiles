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

SetTimer, detectAutoExecFailure, -5000
detectAutoExecFailure() {
    MsgBox, Auto-execute section was not fully executed.
    Reload
}

; Get environment variables
EnvGet, A_UserProfile, USERPROFILE

objWMIService := ComObjGet("winmgmts:{impersonationLevel=impersonate}!\\" A_ComputerName "\root\cimv2")
For objComputer in objWMIService.ExecQuery("Select * from Win32_ComputerSystem") {
    Domain := objComputer.Domain
    Workgroup := objComputer.Workgroup
}

; Office
GroupAdd, office, ahk_exe WINWORD.EXE   ; Microsoft Word
GroupAdd, office, ahk_exe EXCEL.EXE     ; Microsoft Excel
GroupAdd, office, ahk_exe POWERPNT.EXE  ; Microsoft PowerPoint

; Outlook child window
GroupAdd, outlookChild, ahk_exe OUTLOOK.EXE,,, - Outlook

; Browser
GroupAdd, browser, ahk_exe msedge.exe   ; Microsoft Edge
GroupAdd, browser, ahk_exe chrome.exe   ; Google Chrome
GroupAdd, browser, ahk_exe firefox.exe  ; Mozilla Firefox
GroupAdd, browser, ahk_exe vivaldi.exe  ; Vivaldi

; Fast scroll
fastScrollSensitivity := 10

Menu, Tray, Add  ; separator
Menu, Tray, Add, Keep Awake, toggleKeepAwake
If (Domain <> "WORKGROUP") {
    ; Enable keep awake in the office
    toggleKeepAwake()
}

OnExit("confirmExit")

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
    Reload
    Sleep 1000
    ; If successful, the reload will close this instance during the Sleep,
    ; so the line below will never be reached.
    scriptModTime := currentModTime
    SetTimer CheckScriptUpdate, 1000
}

; End of Auto-execute section
SetTimer, detectAutoExecFailure, Delete
Return

;--------------------------------------------------------------------------------
; Global
;--------------------------------------------------------------------------------
; Disable Insert
Insert:: Return

; Use CapsLock as Ctrl
CapsLock:: LCtrl

; Keep Alt+Tab menu opened
!Tab:: ^!Tab

; Global hot keys for Explorer
#e::Run, %A_UserProfile%\Downloads

#z:: Winset, AlwaysOnTop, Toggle, A

~*XButton1:: mouse_activateUnderCursor()
~*XButton2:: mouse_activateUnderCursor()
~*F19:: mouse_activateUnderCursor()

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

; Henkan
vk1C & j:: Left
vk1C & i:: Up
vk1C & k:: Down
vk1C & l:: Right
vk1C & a:: Home
vk1C & e:: End
vk1C & Space:: Enter
vk1C & p:: BackSpace
vk1C & vkBB:: Delete    ; ";" as Delete
vk1C & [:: Esc

; Muhenkan
vk1D & j:: +Left
vk1D & i:: +Up
vk1D & k:: +Down
vk1D & l:: +Right

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

; Toggle keep awake
AppsKey & Esc:: toggleKeepAwake()

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
#IfWinActive - Outlook ahk_exe OUTLOOK.EXE
~^e::
    Sleep, 100
    ime_off(WinExist("A"))
    Return

F19:: !F4
#IfWinActive

#IfWinActive ahk_group outlookChild
; Ctrl+F to search instead of forwarding
^f:: Send, {F4}

; Close message window by pressing both back and forward
F19:: !F4

XButton1:: ^<
XButton2:: ^>
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

; Switch tabs by back/forward buttons
^XButton1:: ^+Tab
^XButton2:: ^Tab

; Close/open/reopen tabs by pressing both back and forward
F19:: ^w
^F19:: ^t
+F19:: ^+t

; Switch tabs by wheel
^WheelUp:: ^+Tab
^WheelDown:: ^Tab
#IfWinActive

;--------------------------------------------------------------------------------
; Visual Studio Code
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe Code.exe
~^f::
~^h::
~^k::
~^+f::
~^+h::
    Sleep, 100
    ime_off(WinExist("A"))
    Return

; Switch tabs by back/forward buttons
^XButton1:: ^PgUp
^XButton2:: ^PgDn

; Close/open/reopen tabs by pressing both back and forward
F19:: ^w
^F19:: ^t
+F19:: ^+t

; Switch tabs by wheel
^WheelUp:: ^PgUp
^WheelDown:: ^PgDn
#IfWinActive

;--------------------------------------------------------------------------------
; CaptureOnTouch
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe TouchDR.exe
XButton1:: Up
XButton2:: Down
#IfWinActive

toggleKeepAwake() {
    static enabled := False
    If (!enabled) {
        enabled := True
        Menu, Tray, Check, Keep Awake
        SetTimer, keepAwake, 300000
    } Else {
        enabled := False
        Menu, Tray, Uncheck, Keep Awake
        SetTimer, keepAwake, Delete
    }
    Return
}

keepAwake() {
    If (A_TimeIdlePhysical > 300000) {
        MouseMove, 1, 0, 1, R  ;Move the mouse one pixel to the right
        MouseMove, -1, 0, 1, R ;Move the mouse back one pixel
    }
    Return
}

confirmExit(ExitReason, ExitCode) {
    If (ExitReason == "Menu") {
        MsgBox, 0x04, %A_ScriptName%, Are you sure you want to exit?
        IfMsgBox, No
            Return 1
    }
}
