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
