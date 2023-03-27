#include <key>
#Include <mouse>
#Include <stroke>
#Include config.ahk

fastScrollSensitivity := config.fastscroll.speed

stroke := StrokeInfo()

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
#+Up:: Send("{Blind}#+{Up}")
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

; Muhenkan
vk1D Up:: imeStatus.off()

; Henkan
vk1C Up:: imeStatus.on()

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

AppsKey Up:: Send("{AppsKey}")
AppsKey & F1:: Send("{Media_Play_Pause}")
AppsKey & F2:: Send("{Volume_Mute}")
AppsKey & F3:: Send("{Volume_Down}")
AppsKey & F4:: Send("{Volume_Up}")

; Virtual Desktop switch desktops
#Home:: Send("{Blind}#^{Left}")
#End:: Send("{Blind}#^{Right}")
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
