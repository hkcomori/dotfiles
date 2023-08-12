from extension.domain.desktop import (
    Desktop,
    DesktopService,
)
from .share import (
    SendMessage,
)


class DesktopWin32(Desktop):
    def lock_on(self) -> bool:
        SC_MONITORPOWER = 0xF170
        WM_SYSCOMMAND = 0x0112
        MONITOR_OFF = 2
        SendMessage(-1, WM_SYSCOMMAND, SC_MONITORPOWER, MONITOR_OFF)
        return True


class DesktopServiceWin32(DesktopService):
    def from_active(self) -> Desktop:
        return DesktopWin32()
