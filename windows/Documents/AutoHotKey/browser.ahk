; Browser
GroupAdd("browser", "ahk_exe msedge.exe")   ; Microsoft Edge
GroupAdd("browser", "ahk_exe chrome.exe")   ; Google Chrome
GroupAdd("browser", "ahk_exe firefox.exe")  ; Mozilla Firefox
GroupAdd("browser", "ahk_exe vivaldi.exe")  ; Vivaldi

GroupAdd("browserDevtool", "DevTools ahk_exe chrome.exe")                        ; Google Chrome
GroupAdd("browserDevtool", "Developer Tools ahk_exe vivaldi.exe")      ; Vivaldi

#HotIf WinActive("ahk_group browserDevtool")
    F19:: WinClose("A")
#HotIf

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
