;--------------------------------------------------------------------------------
; Mouse
;--------------------------------------------------------------------------------
mouse_activateUnderCursor()
{
    prevWindow := WinExist("A")
    MouseGetPos , , &id, &control
    title := WinGetTitle("ahk_id " id)
    Winactivate("ahk_id " id)
    newWindow := WinExist("A")
    return (prevWindow == newWindow)
}

mouse_sendUnderCursor(sendkey) {
    if (mouse_activateUnderCursor()) {
        Send(sendkey)
    }
}
