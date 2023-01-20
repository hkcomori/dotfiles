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

OnExit("confirmExit")

#include <key>
#Include <stroke>
#Include <sys>
#Include <traymenu>
#Include config.ahk
#Include ime_manager.ahk

convMillisecond := 1000
stableWait() {
    Sleep % config.global.stableWait
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
fastScrollSensitivity := config.fastscroll.speed

; separator
Menu, Tray, Add

imeStatus := new ImeManager(config.imeoff.nonactive, config.imeoff.idle * convMillisecond)
disableImeMenu := new ToggleTrayMenu("Auto IME OFF", ObjBindMethod(imeStatus, "tick"), config.global.interval)
if (config.imeoff.enable) {
    disableImeMenu.toggle()
}

keepAwakeMenu := new ToggleTrayMenu("Keep Awake", "KeepAwake", config.global.interval)
keepAwake() {
    If (A_TimeIdlePhysical > (config.keepawake.interval * convMillisecond)) {
        MouseMove, 1, 0, 1, R  ;Move the mouse one pixel to the right
        MouseMove, -1, 0, 1, R ;Move the mouse back one pixel
    }
}
if (config.keepawake.enable) {
    keepAwakeMenu.toggle()
}

; Detect long press
key_startDetectLongPress("vk1D")    ; Muhenkan
key_startDetectLongPress("vk1C")    ; Henkan
key_startDetectLongPress("AppsKey")

; Auto reload this script
FileGetTime scriptModTime, %A_ScriptFullPath%
SetTimer CheckScriptUpdate, % config.global.interval
CheckScriptUpdate() {
    ListLines, Off
    global scriptModTime
    FileGetTime currentModTime, %A_ScriptFullPath%
    If (currentModTime == scriptModTime)
        return
    SetTimer CheckScriptUpdate, Off
    Reload
    Sleep % config.global.interval
    ; If successful, the reload will close this instance during the Sleep,
    ; so the line below will never be reached.
    scriptModTime := currentModTime
    SetTimer CheckScriptUpdate, % config.global.interval
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

#z:: Winset, AlwaysOnTop, Toggle, A

; Input underscore without shift
vkE2:: _

XButton1:: mouse_sendUnderCursor("{XButton1}")
XButton2:: mouse_sendUnderCursor("{XButton2}")
F19:: mouse_sendUnderCursor("{F19}")

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
    Send, {WheelUp %fastScrollSensitivity%}
    Return
!WheelDown::
vk1C & WheelDown::
AppsKey & WheelDown::
    Send, {WheelDown %fastScrollSensitivity%}
    Return

; Toggle keep awake
AppsKey & Esc:: keepAwakeMenu.toggle()

; Turn off IME when opening start menu
~LWin Up::
~RWin Up::
    If (A_PriorKey = "LWin" || A_PriorKey = "RWin"){
        stableWait()
        imeStatus.off()
    }
    Return

#If !stroke.is_active()
    ; Shows command launcher
    #Space::
    vk1D & Space::
        Send, ^!{Insert}
        stableWait()
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
        stableWait()
        imeStatus.off()
        Return
#IfWinActive

;--------------------------------------------------------------------------------
; Task view
;--------------------------------------------------------------------------------
#IfWinActive ahk_class Windows.UI.Core.CoreWindow ahk_exe Explorer.EXE
    ~^f::
        stableWait()
        imeStatus.off()
        Return
#IfWinActive

;--------------------------------------------------------------------------------
; Outlook
;--------------------------------------------------------------------------------
#IfWinActive - Outlook ahk_exe OUTLOOK.EXE
    ~^e::
        stableWait()
        imeStatus.off()
        Return
#IfWinActive

#IfWinActive ahk_group outlookChild
    ; Ctrl+F to search instead of forwarding
    ^f:: Send, {F4}

    ; Close message window by pressing both back and forward
    F19:: mouse_sendUnderCursor("!{F4}")

    XButton1:: mouse_sendUnderCursor("^<")
    XButton2:: mouse_sendUnderCursor("^>")
#IfWinActive

;--------------------------------------------------------------------------------
; Office
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe EXCEL.EXE
    AppsKey & WheelDown::
    !WheelDown::
        Send, {PgDn}
        Return
    AppsKey & WheelUp::
    !WheelUp::
        Send, {PgUp}
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
        stableWait()
        imeStatus.off()
        Return

    ; Switch tabs by back/forward buttons
    ^XButton1:: mouse_sendUnderCursor("^+Tab")
    ^XButton2:: mouse_sendUnderCursor("^Tab")

    ; Close/open/reopen tabs by pressing both back and forward
    F19:: mouse_sendUnderCursor("^w")
    ^F19:: mouse_sendUnderCursor("^t")
    +F19:: mouse_sendUnderCursor("^+t")

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
    ~^e::
    ~^f::
    ~^h::
    ~^k::
    ~^p::
    ~^t::
    ~^+f::
    ~^+h::
    ~^+p::
    ~^@::
    ~F1::
        stableWait()
        imeStatus.off()
        Return

    ; Switch tabs by back/forward buttons
    ^XButton1:: mouse_sendUnderCursor("^{PgUp}")
    ^XButton2:: mouse_sendUnderCursor("^{PgDn}")

    ; Close/open/reopen tabs by pressing both back and forward
    F19:: mouse_sendUnderCursor("^w")
    ^F19:: mouse_sendUnderCursor("^t")
    +F19:: mouse_sendUnderCursor("^+t")

    ; Switch tabs by wheel
    ^WheelUp:: ^PgUp
    ^WheelDown:: ^PgDn

    !WheelUp::
    vk1C & WheelUp::        ; Henkan
    AppsKey & WheelUp::
        Send, !{WheelUp}
        Return
    !WheelDown::
    vk1C & WheelDown::      ; Henkan
    AppsKey & WheelDown::
        Send, !{WheelDown}
        Return
#IfWinActive

;--------------------------------------------------------------------------------
; CaptureOnTouch
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe TouchDR.exe
    XButton1:: mouse_sendUnderCursor("{Up}")
    XButton2:: mouse_sendUnderCursor("{Down}")
#IfWinActive

;--------------------------------------------------------------------------------
; Obsidian
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe Obsidian.exe
    F19:: mouse_sendUnderCursor("^w")
#IfWinActive

confirmExit(ExitReason, ExitCode) {
    If (ExitReason == "Menu") {
        MsgBox, 0x04, %A_ScriptName%, Are you sure you want to exit?
        IfMsgBox, No
            Return 1
    }
}
