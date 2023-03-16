;--------------------------------------------------------------------------------
; Auto-execute section
;--------------------------------------------------------------------------------
Persistent
#SingleInstance Force
#UseHook
InstallKeybdHook
InstallMouseHook
HotkeyInterval := 2000
MaxHotkeysPerInterval := 200
ProcessSetPriority("Realtime")
SendMode("Input")
SetKeyDelay(-1)
SetWorkingDir(A_ScriptDir)
SetTitleMatchMode(2)

SetTimer(detectAutoExecFailure, -5000)
detectAutoExecFailure() {
    MsgBox("Auto-execute section was not fully executed.")
    Reload
}

OnExit(confirmExit)

#Include <traymenu>
#Include config.ahk
#Include ime_manager.ahk

convMillisecond := 1000
stableWait() {
    Sleep(config.global.stableWait)
}

; separator
tray := A_TrayMenu
tray.Add()

imeStatus := ImeManager(config.imeoff.nonactive, config.imeoff.idle * convMillisecond)
disableImeMenu := ToggleTrayMenu("Auto IME OFF", ObjBindMethod(imeStatus, "tick"), config.global.interval)
if (config.imeoff.enable) {
    disableImeMenu.toggle()
}

keepAwakeMenu := ToggleTrayMenu("Keep Awake", KeepAwake, config.global.interval)
keepAwake() {
    If (A_TimeIdlePhysical > (config.keepawake.interval * convMillisecond)) {
        MouseMove(1, 0, 1, "R")  ;Move the mouse one pixel to the right
        MouseMove(-1, 0, 1, "R") ;Move the mouse back one pixel
    }
}
if (config.keepawake.enable) {
    keepAwakeMenu.toggle()
}

; Auto reload this script
scriptModTime := FileGetTime(A_ScriptFullPath)
SetTimer(CheckScriptUpdate, config.global.interval)
CheckScriptUpdate() {
    ListLines(False)
    global scriptModTime
    currentModTime := FileGetTime(A_ScriptFullPath)
    If (currentModTime == scriptModTime)
        return
    SetTimer(CheckScriptUpdate, 0)
    Reload
    Sleep(config.global.interval)
    ; If successful, the reload will close this instance during the Sleep,
    ; so the line below will never be reached.
    scriptModTime := currentModTime
    SetTimer(CheckScriptUpdate, config.global.interval)
}

; End of Auto-execute section
SetTimer(detectAutoExecFailure, 0)

;--------------------------------------------------------------------------------
; Hotkeys
;--------------------------------------------------------------------------------
#Include global.ahk
#Include windows.ahk
#Include office.ahk
#Include browser.ahk
#Include vscode.ahk
#Include captureontouch.ahk
#Include obsidian.ahk

confirmExit(ExitReason, ExitCode) {
    If (ExitReason == "Menu") {
        pressed := MsgBox("Are you sure you want to exit?", A_ScriptName, 0x04)
        If (pressed == "No")
            Return 1
    }
}
