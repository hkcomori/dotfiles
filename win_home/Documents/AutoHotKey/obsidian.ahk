#HotIf WinActive("ahk_exe Obsidian.exe")
    F19:: mouse_sendUnderCursor("^w")

    ~^f::
    ~^h::
    ~^+f::
    ~^+p::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf
