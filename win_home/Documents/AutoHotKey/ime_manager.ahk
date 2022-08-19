#Include <ime>

class ImeManager {
    __New(set_count) {
        this.counts := {}
        this.set_count := set_count
    }
    on(win_title := "A") {
        window := WinExist(win_title)
        ime_on(window)
        this.counts[window] := this.set_count
    }
    off(win_title := "A") {
        window := WinExist(win_title)
        this.counts.Delete(window)
        ime_off(window)
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
        for window, _ in this.counts {
            if (window == activeWindow) {
                this.counts[window] := this.set_count
                continue
            }
            this.counts[window] -= 1
            if (this.counts[window] <= 0) {
                this.counts.Delete(window)
                ime_off(window)
            }
        }
    }
}
