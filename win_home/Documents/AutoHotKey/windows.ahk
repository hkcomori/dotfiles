#HotIf WinActive("ahk_class CabinetWClass ahk_exe Explorer.EXE")
    F19:: WinClose("A")
    ~^f::
    ~^l::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf

#HotIf WinActive("ahk_class Windows.UI.Core.CoreWindow ahk_exe Explorer.EXE")
    ~^f::
    {
        stableWait()
        imeStatus.off()
        Return
    }
#HotIf
