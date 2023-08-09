from extension.domain.desktop import (
    Desktop,
    DesktopFactory,
)


class DesktopMock(Desktop):
    def lock_on(self) -> None:
        pass


class DesktopFactoryMock(DesktopFactory):
    def from_active(self) -> DesktopMock:
        return DesktopMock()
