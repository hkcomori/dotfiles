;--------------------------------------------------------------------------------
; IME
; https://morakana.hatenadiary.org/entry/20090702/1246526840
;--------------------------------------------------------------------------------
ime_getConv(hWindow)
{
    Return api_SendMessage(api_ImmGetDefaultIMEWnd(hWindow), 0x283, 0x1, 0x0)
}

ime_isOn(hWindow)
{
    Return api_SendMessage(api_ImmGetDefaultIMEWnd(hWindow), 0x283, 0x5, 0x0)
}

ime_isKana(hWindow)
{
    If ( (ime_getConv(hWindow) & 0x10) == 0x10 )
        Return 0
    Return 1
}

ime_isRoman(hWindow)
{
    If ( (ime_getConv(hWindow) & 0x10) == 0x10 )
        Return 1
    Return 0
}

ime_off(hWindow)
{
    If ( !api_IsWindow(hWindow))
        Return 0
    If ( !ime_isOn(hWindow) )
        Return 1
    Return ( !api_SendMessage(api_ImmGetDefaultIMEWnd(hWindow), 0x283, 0x6, 0x0) )
}

ime_on(hWindow)
{
    If ( !api_IsWindow(hWindow))
        Return 0
    If ( ime_isOn(hWindow) )
        Return 1
    Return ( !api_SendMessage(api_ImmGetDefaultIMEWnd(hWindow), 0x283, 0x6, 0x1) )
}

ime_setConv(hWindow,ConversionMode)
{
    Return api_SendMessage(api_ImmGetDefaultIMEWnd(hWindow), 0x283, 0x2, ConversionMode)
}
