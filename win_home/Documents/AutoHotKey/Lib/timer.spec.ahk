#SingleInstance, Force
SetWorkingDir, %A_ScriptDir%

#Include <Yunit\Yunit>
#Include <Yunit\Stdout>

#Include %A_ScriptDir%\timer.ahk

Tester := Yunit.Use(YunitStdout).Test(TestTimer)

cnt := 0

checkCalled() {
    global cnt
    cnt += 1
}

class TestTimer {
    Begin() {
        global cnt
        cnt := 0
    }
    interval() {
        global cnt
        timer := new Timer("checkCalled")
        timer.start(100)
        Yunit.assert(cnt == 0)
        Sleep, 150
        Yunit.assert(cnt == 1)
        Sleep, 100
        Yunit.assert(cnt == 2)
    }
    oneshot() {
        global cnt
        timer := new Timer("checkCalled")
        timer.start(-100)
        Yunit.assert(cnt == 0)
        Sleep, 150
        Yunit.assert(cnt == 1)
        Sleep, 100
        Yunit.assert(cnt == 1)
    }
    stop() {
        global cnt
        timer := new Timer("checkCalled")
        timer.start(100)
        Yunit.assert(cnt == 0)
        timer.stop()
        Sleep, 150
        Yunit.assert(cnt == 0)
        Sleep, 100
        Yunit.assert(cnt == 0)
    }
}
