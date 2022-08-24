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
SetKeyDelay, -1
SetWorkingDir %A_ScriptDir%
SetTitleMatchMode, 2

SetTimer, detectAutoExecFailure, -5000
detectAutoExecFailure() {
    MsgBox, Auto-execute section was not fully executed.
    Reload
}

#include <key>
#Include <stroke>
#Include <traymenu>
#Include ime_manager.ahk

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
imeStatus := new ImeManager(60, 180000)
disableImeMenu := new ToggleTrayMenu("Auto IME OFF", ObjBindMethod(imeStatus, "tick"), 1000)
disableImeMenu.toggle()
keepAwakeMenu := new ToggleTrayMenu("Keep Awake", "KeepAwake", 300000)
keepAwake() {
    If (A_TimeIdlePhysical > 300000) {
        MouseMove, 1, 0, 1, R  ;Move the mouse one pixel to the right
        MouseMove, -1, 0, 1, R ;Move the mouse back one pixel
    }
}
If (Domain <> "WORKGROUP") {
    ; Enable keep awake in the office
    keepAwakeMenu.toggle()
}

OnExit("confirmExit")

; Detect long press
key_startDetectLongPress("vk1D")    ; Muhenkan
key_startDetectLongPress("vk1C")    ; Henkan
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

stroke := new StrokeInfo()

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

; Input underscore without shift
vkE2:: _

~*XButton1:: mouse_activateUnderCursor()
~*XButton2:: mouse_activateUnderCursor()
~*F19:: mouse_activateUnderCursor()

~*WheelDown:: mouse_activateUnderCursor()
~*WheelUp:: mouse_activateUnderCursor()
~*WheelRight:: mouse_activateUnderCursor()
~*WheelLeft:: mouse_activateUnderCursor()
+WheelDown:: WheelRight
+WheelUp:: WheelLeft

sc29:: imeStatus.toggle()      ; Hankaku/Zenkaku

vk1D Up::   ; Muhenkan
    If !key_isLongPressed("vk1D", True)
        imeStatus.off()
    Return

vk1C Up::   ; Henkan
    If !key_isLongPressed("vk1C", True)
        imeStatus.on()
    Return

sc70:: imeStatus.on()          ; Kana

; Henkan
vk1C & Left:: !Left
vk1C & Right:: !Right
vk1C & Up:: !Up
vk1C & Down:: !Down
vk1C & b:: ^Left
vk1C & f:: ^Right
vk1C & n:: Send, {Down 5}
vk1C & p:: Send, {Up 5}
vk1C & ,:: ^Home
vk1C & .:: ^End
vk1C & w:: ^c
vk1C & v:: PgUp

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
!WheelUp::
vk1C & WheelUp::
AppsKey & WheelUp::
    mouse_activateUnderCursor()
    Send, {WheelUp %fastScrollSensitivity%}
    Return
!WheelDown::
vk1C & WheelDown::
AppsKey & WheelDown::
    mouse_activateUnderCursor()
    Send, {WheelDown %fastScrollSensitivity%}
    Return

; Toggle keep awake
AppsKey & Esc:: keepAwakeMenu.toggle()

; Turn off IME when opening start menu
~LWin Up::
~RWin Up::
    If (A_PriorKey = "LWin" || A_PriorKey = "RWin"){
        Sleep, 100
        imeStatus.off()
    }
    Return

; Macro board
AppsKey & Insert:: Send, {F13}
AppsKey & Home:: Send, {F14}
AppsKey & PgUp:: Send, {F15}
AppsKey & Delete:: Send, {F16}
AppsKey & End:: Send, {F17}
AppsKey & PgDn:: Send, {F18}

#If !stroke.is_active()
    ; Shows command launcher
    #Space::
    vk1D & Space::
        Send, ^!{Insert}
        Sleep, 100
        imeStatus.off()
        Return

    vk1D & b:: Left
    vk1D & p:: Up
    vk1D & n:: Down
    vk1D & f:: Right
    vk1D & a:: Home
    vk1D & e:: End
    vk1D & v:: PgDn
    vk1D & m:: Enter
    vk1D & h:: BackSpace
    vk1D & d:: Delete
    vk1D & s:: ^f
    vk1D & y:: ^v
    vk1D & [:: Esc
    vk1D & /:: ^z
    vk1D & vkE2:: ^y        ; Backslash located next to slash key
    vk1D & Left:: ^Left
    vk1D & Right:: ^Right
    vk1D & Up:: Send, {Up 5}
    vk1D & Down:: Send, {Down 5}
    vk1D & PgUp:: ^PgUp
    vk1D & PgDn:: ^PgDn
    vk1D & @:: ^@
    vk1D & BackSpace:: ^BackSpace
    vk1D & Delete:: ^Delete

    ; Start multi-stroke
    vk1D & q::
    vk1D & x::
        stroke.activate(A_ThisHotkey)
        Return
    vk1D & u::
        stroke.activate(A_ThisHotkey, True)
        Return
    vk1D & g:: Esc
#If

; Stop multi-stroke
#If stroke.is_active()
    Esc::
    vk1D & g::
        stroke.deactivate(A_ThisHotKey)
        Return
#If

; C-q like behavior
#If stroke.is_active() && (stroke.keys[1] == "vk1D & q")
    vk1D & a::
        stroke.deactivate(A_ThisHotKey)
        Send, ^a
        Return
#If

; C-x like behavior
#If stroke.is_active() && (stroke.keys[1] == "vk1D & x")
    vk1D & s::
        stroke.deactivate(A_ThisHotKey)
        Send, ^s
        Return
#If

; C-u like behavior (Repeat)
#If stroke.is_active() && (stroke.keys[1] == "vk1D & u")
    ; Set repeat counts
    0::
    1::
    2::
    3::
    4::
    5::
    6::
    7::
    8::
    9::
        stroke.push(A_ThisHotkey)
        Return
    ; Repeated send keys
    a::
    b::
    c::
    d::
    e::
    f::
    g::
    h::
    i::
    j::
    k::
    l::
    m::
    n::
    o::
    p::
    q::
    r::
    s::
    t::
    u::
    v::
    w::
    x::
    y::
    z::
    !::
    #::
    $::
    %::
    &::
    '::
    (::
    )::
    -::
    ^::
    \::
    =::
    ~::
    @::
    [::
    ]::
    `::
    {::
    }::
    `;::
    :::
    +::
    *::
    <::
    >::
    ?::
    _::
    Left::
    Right::
    Down::
    Up::
    Enter::
    BackSpace::
    Delete::
        repeat := stroke.deactivate(A_ThisHotKey)
        Send, {%A_ThisHotkey% %repeat%}
        Return
    +A::
    +B::
    +C::
    +D::
    +E::
    +F::
    +G::
    +H::
    +I::
    +J::
    +K::
    +L::
    +M::
    +N::
    +O::
    +P::
    +Q::
    +R::
    +S::
    +T::
    +U::
    +V::
    +W::
    +X::
    +Y::
    +Z::
        key := KeyUtil.trim_modifier(A_ThisHotkey)
        repeat := stroke.deactivate(A_ThisHotKey)
        Send, {%key% %repeat%}
        Return
    +2::    ; Double quotation
        repeat := stroke.deactivate(A_ThisHotKey)
        Loop % repeat {
            Send, {ASC 034}
            Sleep, 1
        }
        Return
    +7::    ; Single quotation
        repeat := stroke.deactivate(A_ThisHotKey)
        Loop % repeat {
            Send, {ASC 039}
            Sleep, 1
        }
        Return
    vkE2::  ; Backslash located next to slash key
        repeat := stroke.deactivate(A_ThisHotKey)
        Send, {_ %repeat%}
        Return
#If

;--------------------------------------------------------------------------------
; Explorer
;--------------------------------------------------------------------------------
#IfWinActive ahk_class CabinetWClass ahk_exe Explorer.EXE
    ~^f::
    ~^l::
        Sleep, 100
        imeStatus.off()
        Return
#IfWinActive

;--------------------------------------------------------------------------------
; Task view
;--------------------------------------------------------------------------------
#IfWinActive ahk_class Windows.UI.Core.CoreWindow ahk_exe Explorer.EXE
    ~^f::
        Sleep, 100
        imeStatus.off()
        Return
#IfWinActive

;--------------------------------------------------------------------------------
; Outlook
;--------------------------------------------------------------------------------
#IfWinActive - Outlook ahk_exe OUTLOOK.EXE
    ~^e::
        Sleep, 100
        imeStatus.off()
        Return
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
        imeStatus.off()
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
#If WinActive("ahk_exe Code.exe") && GetKeyState("vk1D", "P")
    ; Swap lines
    Up:: !Up
    Down:: !Down

    ; Add multiple cursors
    ^Up:: ^!Up
    ^Down:: ^!Down

    ; Change focus editors
    1:: ^1
    2:: ^2
    3:: ^3
    4:: ^4
    5:: ^5
    6:: ^6
    7:: ^7
    8:: ^8
    9:: ^9
    0:: ^0
#If

#IfWinActive ahk_exe Code.exe
    ~^f::
    ~^h::
    ~^k::
    ~^+f::
    ~^+h::
        Sleep, 100
        imeStatus.off()
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

    !WheelUp:: !WheelUp
    vk1C & WheelUp:: !WheelUp           ; Henkan
    AppsKey & WheelUp:: !WheelUp
    !WheelDown:: !WheelDown
    vk1C & WheelDown:: !WheelDown       ; Henkan
    AppsKey & WheelDown:: !WheelDown
#IfWinActive

;--------------------------------------------------------------------------------
; CaptureOnTouch
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe TouchDR.exe
    XButton1:: Up
    XButton2:: Down
#IfWinActive

confirmExit(ExitReason, ExitCode) {
    If (ExitReason == "Menu") {
        MsgBox, 0x04, %A_ScriptName%, Are you sure you want to exit?
        IfMsgBox, No
            Return 1
    }
}
