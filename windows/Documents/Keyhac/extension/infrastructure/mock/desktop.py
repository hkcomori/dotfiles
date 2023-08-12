from extension.domain.desktop import (
    Desktop,
    DesktopService,
)


class DesktopMock(Desktop):
    def lock_on(self) -> bool:
        return True


class DesktopServiceMock(DesktopService):
    def from_active(self) -> DesktopMock:
        return DesktopMock()
