; Office
GroupAdd("office", "ahk_exe WINWORD.EXE")   ; Microsoft Word
GroupAdd("office", "ahk_exe EXCEL.EXE")     ; Microsoft Excel
GroupAdd("office", "ahk_exe POWERPNT.EXE")  ; Microsoft PowerPoint

; Outlook child window
GroupAdd("outlookChild", "ahk_exe OUTLOOK.EXE",,, "- Outlook")

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
