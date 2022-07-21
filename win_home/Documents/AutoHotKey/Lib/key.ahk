;--------------------------------------------------------------------------------
; Key
;--------------------------------------------------------------------------------
key_startDetectLongPress(key)
{
    global pressedTime
    If !IsObject(pressedTime)
        pressedTime := Object()
    pressedTime[key] := 0
    SetTimer key_detectLongPress, 100
}

key_stopDetectLongPress(key)
{
    global pressedTime
    pressedTime.Delete(key)
    If pressedTime.Length() == 0
        SetTimer key_detectLongPress, Delete
}

key_detectLongPress()
{
    ListLines, Off
    global pressedTime
    for key, _ in pressedTime
        If (GetKeyState(key, "P"))
            pressedTime[key]++
}

key_isLongPressed(key, clear=False)
{
    global pressedTime
    _pressedTime := pressedTime[key]
    If clear
        pressedTime[key] := 0
    Return _pressedTime > 3
}
