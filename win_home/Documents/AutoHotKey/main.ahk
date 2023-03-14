;--------------------------------------------------------------------------------
; Auto-execute section
;--------------------------------------------------------------------------------
Persistent
#SingleInstance Force
#UseHook
InstallKeybdHook
InstallMouseHook
HotkeyInterval := 2000
MaxHotkeysPerInterval := 200
ProcessSetPriority("Realtime")
SendMode("Input")
SetKeyDelay(-1)
SetWorkingDir(A_ScriptDir)
SetTitleMatchMode(2)

SetTimer(detectAutoExecFailure, -5000)
detectAutoExecFailure() {
    MsgBox("Auto-execute section was not fully executed.")
    Reload
}

OnExit(confirmExit)

#include <key>
#Include <mouse>
#Include <stroke>
#Include <traymenu>
#Include config.ahk
#Include ime_manager.ahk

convMillisecond := 1000
stableWait() {
    Sleep(config.global.stableWait)
}

; Office
GroupAdd("office", "ahk_exe WINWORD.EXE")   ; Microsoft Word
GroupAdd("office", "ahk_exe EXCEL.EXE")     ; Microsoft Excel
GroupAdd("office", "ahk_exe POWERPNT.EXE")  ; Microsoft PowerPoint

; Outlook child window
GroupAdd("outlookChild", "ahk_exe OUTLOOK.EXE",,, "- Outlook")

; Browser
GroupAdd("browser", "ahk_exe msedge.exe")   ; Microsoft Edge
GroupAdd("browser", "ahk_exe chrome.exe")   ; Google Chrome
GroupAdd("browser", "ahk_exe firefox.exe")  ; Mozilla Firefox
GroupAdd("browser", "ahk_exe vivaldi.exe")  ; Vivaldi

; Fast scroll
fastScrollSensitivity := config.fastscroll.speed

; separator
tray := A_TrayMenu
tray.Add()

imeStatus := ImeManager(config.imeoff.nonactive, config.imeoff.idle * convMillisecond)
disableImeMenu := ToggleTrayMenu("Auto IME OFF", ObjBindMethod(imeStatus, "tick"), config.global.interval)
if (config.imeoff.enable) {
    disableImeMenu.toggle()
}

keepAwakeMenu := ToggleTrayMenu("Keep Awake", KeepAwake, config.global.interval)
keepAwake() {
    If (A_TimeIdlePhysical > (config.keepawake.interval * convMillisecond)) {
        MouseMove(1, 0, 1, "R")  ;Move the mouse one pixel to the right
        MouseMove(-1, 0, 1, "R") ;Move the mouse back one pixel
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
scriptModTime := FileGetTime(A_ScriptFullPath)
SetTimer(CheckScriptUpdate, config.global.interval)
CheckScriptUpdate() {
    ListLines(False)
    global scriptModTime
    currentModTime := FileGetTime(A_ScriptFullPath)
    If (currentModTime == scriptModTime)
        return
    SetTimer(CheckScriptUpdate, 0)
    Reload
    Sleep(config.global.interval)
    ; If successful, the reload will close this instance during the Sleep,
    ; so the line below will never be reached.
    scriptModTime := currentModTime
    SetTimer(CheckScriptUpdate, config.global.interval)
}

stroke := StrokeInfo()

; End of Auto-execute section
SetTimer(detectAutoExecFailure, 0)
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

; FancyZones expand window
#+Left:: Send("{Blind}#^!{Left}")
#+Down:: Send("{Blind}#^!{Down}")
#+Right:: Send("{Blind}#^!{Right}")
#+Up:: Send("{Blind}#^!{Up}")
; FancyZones rotate windows in current zone
AppsKey & PgUp:: Send("{Blind}#{PgUp}")
AppsKey & PgDn:: Send("{Blind}#{PgDn}")

#z:: WinsetAlwaysOnTop(-1, "A")
#F11::
AppsKey & F11::
{
    If WinExist("ahk_exe Obsidian.exe") {
        WinActivate("ahk_exe Obsidian.exe")
    } else {
        Run EnvGet("USERPROFILE") "\AppData\Local\Obsidian\Obsidian.exe"
    }
    Return
}
#F12::
AppsKey & F12::
{
    If WinExist("電卓") {
        WinActivate("電卓")
    } else {
        Run "calc.exe"
    }
    Return
}

; Input underscore without shift
vkE2:: _

XButton1:: mouse_sendUnderCursor("{XButton1}")
XButton2:: mouse_sendUnderCursor("{XButton2}")
F19:: mouse_sendUnderCursor("{F19}")

+WheelDown:: WheelRight
+WheelUp:: WheelLeft

sc29:: imeStatus.toggle()      ; Hankaku/Zenkaku

vk1D Up::   ; Muhenkan
{
    If !key_isLongPressed("vk1D", True)
        imeStatus.off()
    Return
}

vk1C Up::   ; Henkan
{
    If !key_isLongPressed("vk1C", True)
        imeStatus.on()
    Return
}

sc70:: imeStatus.on()          ; Kana

; Henkan
vk1C & Left:: !Left
vk1C & Right:: !Right
vk1C & Up:: !Up
vk1C & Down:: !Down
vk1C & b:: ^Left
vk1C & f:: ^Right
vk1C & n:: Send("{Down 5}")
vk1C & p:: Send("{Up 5}")
vk1C & ,:: ^Home
vk1C & .:: ^End
vk1C & w:: ^c
vk1C & v:: PgUp

; Emulate Fn-key of RealForce by AppsKey
AppsKey Up::
{
    If !key_isLongPressed("AppsKey", True)
        Send("{AppsKey}")
    Return
}
AppsKey & F1:: Send("{Media_Play_Pause}")
AppsKey & F2:: Send("{Volume_Mute}")
AppsKey & F3:: Send("{Volume_Down}")
AppsKey & F4:: Send("{Volume_Up}")

; Virtual Desktop switch desktops
AppsKey & Home:: Send("{Blind}#^{Left}")
AppsKey & End:: Send("{Blind}#^{Right}")
AppsKey & XButton1:: Send("{Blind}#^{Left}")
AppsKey & XButton2:: Send("{Blind}#^{Right}")
AppsKey & WheelUp:: Send("{Blind}#{PgUp}")
AppsKey & WheelDown:: Send("{Blind}#{PgDn}")

; Fast scroll
!WheelUp::
vk1C & WheelUp::
{
    Send("{WheelUp " fastScrollSensitivity "}")
    Return
}
!WheelDown::
vk1C & WheelDown::
{
    Send("{WheelDown " fastScrollSensitivity "}")
    Return
}

; Toggle keep awake
AppsKey & Esc:: keepAwakeMenu.toggle()

; Turn off IME when opening start menu
~LWin Up::
~RWin Up::
{
    If (A_PriorKey = "LWin" || A_PriorKey = "RWin"){
        stableWait()
        imeStatus.off()
    }
    Return
}

#HotIf !stroke.is_active()
    ; Shows command launcher
    #Space::
    vk1D & Space::
    {
        Send("^!{Insert}")
        stableWait()
        imeStatus.off()
        Return
    }

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
    vk1D & Up:: Send("{Up 5}")
    vk1D & Down:: Send("{Down 5}")
    vk1D & PgUp:: ^PgUp
    vk1D & PgDn:: ^PgDn
    vk1D & @:: ^@
    vk1D & BackSpace:: ^BackSpace
    vk1D & Delete:: ^Delete

    ; Start multi-stroke
    vk1D & q::
    vk1D & x::
    {
        stroke.activate(A_ThisHotkey)
        Return
    }
    vk1D & u::
    {
        stroke.activate(A_ThisHotkey, True)
        Return
    }
    vk1D & g:: Esc
#HotIf

; Stop multi-stroke
#HotIf stroke.is_active()
    Esc::
    vk1D & g::
    {
        stroke.deactivate(A_ThisHotKey)
        Return
    }
#HotIf

; C-q like behavior
#HotIf stroke.is_active() && (stroke.keys[1] == "vk1D & q")
    vk1D & a::
    {
        stroke.deactivate(A_ThisHotKey)
        Send("^a")
        Return
    }
#HotIf

; C-x like behavior
#HotIf stroke.is_active() && (stroke.keys[1] == "vk1D & x")
    vk1D & s::
    {
        stroke.deactivate(A_ThisHotKey)
        Send("^s")
        Return
    }
#HotIf

; C-u like behavior (Repeat)
#HotIf stroke.is_active() && (stroke.keys[1] == "vk1D & u")
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
    {
        stroke.push(A_ThisHotkey)
        Return
    }
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
    {
        repeat := stroke.deactivate(A_ThisHotKey)
        Send("{" A_ThisHotkey " " repeat "}")
        Return
    }
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
    {
        key := KeyUtil.trim_modifier(A_ThisHotkey)
        repeat := stroke.deactivate(A_ThisHotKey)
        Send("{" key " " repeat "}")
        Return
    }
    +2::    ; Double quotation
    {
        repeat := stroke.deactivate(A_ThisHotKey)
        Loop repeat {
            Send("{ASC 034}")
            Sleep(1)
        }
        Return
    }
    +7::    ; Single quotation
    {
        repeat := stroke.deactivate(A_ThisHotKey)
        Loop repeat {
            Send("{ASC 039}")
            Sleep(1)
        }
        Return
    }
    vkE2::  ; Backslash located next to slash key
    {
        repeat := stroke.deactivate(A_ThisHotKey)
        Send("{_ " repeat "}")
        Return
    }
#HotIf

;--------------------------------------------------------------------------------
; Explorer
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_class CabinetWClass ahk_exe Explorer.EXE")
    ~^f::
    ~^l::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf

;--------------------------------------------------------------------------------
; Task view
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_class Windows.UI.Core.CoreWindow ahk_exe Explorer.EXE")
    ~^f::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf

;--------------------------------------------------------------------------------
; Outlook
;--------------------------------------------------------------------------------
#HotIf WinActive("- Outlook ahk_exe OUTLOOK.EXE")
    ~^e::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf

#HotIf WinActive("ahk_group outlookChild")
    ; Ctrl+F to search instead of forwarding
    ^f:: Send("{F4}")

    ; Close message window by pressing both back and forward
    F19:: mouse_sendUnderCursor("!{F4}")

    XButton1:: mouse_sendUnderCursor("^<")
    XButton2:: mouse_sendUnderCursor("^>")
#HotIf

;--------------------------------------------------------------------------------
; Office
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_exe EXCEL.EXE")
    AppsKey & WheelDown::
    !WheelDown::
    {
        Send("{PgDn}")
        Return
    }
    AppsKey & WheelUp::
    !WheelUp::
    {
        Send("{PgUp}")
        Return
    }
#HotIf

;--------------------------------------------------------------------------------
; Browser
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_group browser")
    ~^e::
    ~^f::
    ~^l::
    ~^t::
    ~F1::
    ~F3::
    ~+F3::
    ~^+p::
    {
        stableWait()
        imeStatus.off()
        Return
    }

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
#HotIf

;--------------------------------------------------------------------------------
; Visual Studio Code
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_exe Code.exe") && GetKeyState("vk1D", "P")
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
#HotIf

#HotIf WinActive("ahk_exe Code.exe")
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
    {
        stableWait()
        imeStatus.off()
        Return
    }

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
    {
        Send("!{WheelUp}")
        Return
    }
    !WheelDown::
    vk1C & WheelDown::      ; Henkan
    AppsKey & WheelDown::
    {
        Send("!{WheelDown}")
        Return
    }
#HotIf

;--------------------------------------------------------------------------------
; CaptureOnTouch
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_exe TouchDR.exe")
    XButton1:: mouse_sendUnderCursor("{Up}")
    XButton2:: mouse_sendUnderCursor("{Down}")
#HotIf

;--------------------------------------------------------------------------------
; Obsidian
;--------------------------------------------------------------------------------
#HotIf WinActive("ahk_exe Obsidian.exe")
    F19:: mouse_sendUnderCursor("^w")
#HotIf

confirmExit(ExitReason, ExitCode) {
    If (ExitReason == "Menu") {
        pressed := MsgBox("Are you sure you want to exit?", A_ScriptName, 0x04)
        If (pressed == "No")
            Return 1
    }
}
