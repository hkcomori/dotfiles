;--------------------------------------------------------------------------------
; Auto-execute section
;--------------------------------------------------------------------------------
; Browser
GroupAdd, browser, ahk_exe msedge.exe   ; Microsoft Edge
GroupAdd, browser, ahk_exe chrome.exe   ; Google Chrome
GroupAdd, browser, ahk_exe firefox.exe  ; Mozilla Firefox
GroupAdd, browser, ahk_exe vivaldi.exe  ; Vivaldi

; Fast scroll
fastScrollSensitivity := 10

;--------------------------------------------------------------------------------
; Global
;--------------------------------------------------------------------------------
; Disable Insert
Insert:: Return

; Emulate Fn-key of RealForce by AppsKey
AppsKey Up:: Send, {AppsKey}
AppsKey & Left:: Send, {Volume_Mute}
AppsKey & Down:: Send, {Volume_Down}
AppsKey & Right:: Send, {Volume_Up}

; Fast scroll
AppsKey & WheelUp:: Send, {WheelUp %fastScrollSensitivity%}
AppsKey & WheelDown:: Send, {WheelDown %fastScrollSensitivity%}

; Turn off IME when opening start menu
~LWin Up::
~RWin Up::
    If (A_PriorKey = "LWin" || A_PriorKey = "RWin"){
        Sleep, 100
        ime_off(WinExist("A"))
    }
    Return

;--------------------------------------------------------------------------------
; Explorer
;--------------------------------------------------------------------------------
#IfWinActive ahk_class CabinetWClass ahk_exe Explorer.EXE
~^f::
~^l::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive

;--------------------------------------------------------------------------------
; Task view
;--------------------------------------------------------------------------------
#IfWinActive ahk_class Windows.UI.Core.CoreWindow ahk_exe Explorer.EXE
~^f::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive

;--------------------------------------------------------------------------------
; Outlook
;--------------------------------------------------------------------------------
#IfWinActive ahk_exe OUTLOOK.EXE
~^e::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive

;--------------------------------------------------------------------------------
; Browser
;--------------------------------------------------------------------------------
#IfWinActive ahk_group browser
~^e::
~^l::
~^t::
~F1::
~^+p::
    Sleep, 100
    ime_off(WinExist("A"))
    Return
#IfWinActive
