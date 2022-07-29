;--------------------------------------------------------------------------------
; Mouse
;--------------------------------------------------------------------------------
mouse_activateUnderCursor()
{
    MouseGetPos, , , id, control
    WinGetTitle, title, ahk_id %id%
    Winactivate, ahk_id %id%
}
