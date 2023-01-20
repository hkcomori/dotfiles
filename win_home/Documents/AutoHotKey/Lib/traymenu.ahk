#include <time>

class ToggleTrayMenu{
    __New(menu_name, tick_func, interval) {
        this.enabled := False
        this.menu_name := menu_name
        this.timer := Timer(tick_func)
        this.interval := interval
        toggle := ObjBindMethod(this, "toggle")
        Menu, Tray, Add, % this.menu_name, % toggle
    }
    toggle() {
        If (!this.enabled) {
            this.enabled := True
            Menu, Tray, Check, % this.menu_name
            this.timer.start(this.interval)
        } Else {
            this.enabled := False
            Menu, Tray, Uncheck, % this.menu_name
            this.timer.stop()
        }
        Return
    }
}
