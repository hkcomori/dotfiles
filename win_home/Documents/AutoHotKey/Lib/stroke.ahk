#include <array>
#include <time>

class StrokeInfo {
    static IS_NUMBER := new ArrayMatcher(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    __New(timeout_ms := 5000, repeat_limit := 100) {
        this.init_variables()
        this.timer := new Timer(ObjBindMethod(this, "deactivate"))
        this.timeout_ms := timeout_ms
        this.repeat_limit := repeat_limit
    }
    init_variables() {
        this.repeat_mode := False
        this.keys := []
        this.display := new StrokeDisplay()
        this.repeat := ""
    }
    is_active() {
        Return (this.repeat > 1) || (this.keys.Length() > 0)
    }
    activate(key, repeat_mode := False) {
        this.init_variables()
        this.push(key)
        this.repeat_mode := repeat_mode
    }
    push(key) {
        this.timer.start(-this.timeout_ms)
        If (this.repeat_mode == True) {
            If (this.IS_NUMBER.in(key)) {
                this.repeat .= key
            } Else {
                this.repeat_mode := False
                this.keys := []
            }
        }
        this.keys.push(key)
        this.display.push(key)
    }
    deactivate(key := "") {
        this.timer.stop()
        If (key <> "")
            this.push(key)
        repeat := (this.repeat == "") ? "1" : this.repeat
        If (repeat > this.repeat_limit)
            repeat := this.repeat_limit
        this.init_variables()
        Return repeat
    }
}

class StrokeDisplay {
    __New() {
        this.text := ""
    }
    push(key) {
        this.text .= " " . key
        ; Shows tooltip left-top of active window
        ToolTip % this.text, 10, 10
    }
    __Delete() {
        ToolTip
    }
}
