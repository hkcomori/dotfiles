class Timer {
    __New(callback) {
        this.callback := callback
    }
    start(interval) {
        callback := this.callback
        SetTimer % callback, % interval
    }
    stop() {
        callback := this.callback
        SetTimer % callback, Delete
    }
}
