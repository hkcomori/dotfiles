#Include <ime>

class ImeManager {
    __New(set_count, msIdle) {
        this.counts := {}
        this.set_count := set_count
        this.msIdle := msIdle
    }
    on(win_title := "A") {
        window := WinExist(win_title)
        ime_on(window)
        this.counts[window] := this.set_count
        if (WinActive(win_title)) {
            Send, {vk1C}    ; Henkan
        }
    }
    off(win_title := "A") {
        window := WinExist(win_title)
        this.counts.Delete(window)
        ime_off(window)
        if (WinActive(win_title)) {
            Send, {vk1D}    ; Muhenkan
        }
    }
    toggle(win_title := "A") {
        window := WinExist(win_title)
        if (this.counts.haskey(window)) {
            this.off(win_title)
        } Else {
            this.on(win_title)
        }
    }
    tick() {
        activeWindow := WinExist("A")
        isIdle := (this.msIdle > 0) && (A_TimeIdlePhysical > this.msIdle)
        for window, _ in this.counts {
            if (window == activeWindow) {
                this.counts[window] := this.set_count
            } Else {
                this.counts[window] -= 1
            }
            if (isIdle || (this.counts[window] <= 0)) {
                this.counts.Delete(window)
                ime_off(window)
            }
        }
    }
}
