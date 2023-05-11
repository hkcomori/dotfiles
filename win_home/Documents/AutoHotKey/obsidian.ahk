#HotIf WinActive("ahk_exe Obsidian.exe")
    F19:: mouse_sendUnderCursor("^w")

    ~^f::
    ~^h::
    ~^o::
    ~^+f::
    ~^+p::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf
