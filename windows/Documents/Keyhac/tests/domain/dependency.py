from extension.domain.require_injection import (
    AbstractDependency,
    Window,
    WindowService,
    Desktop,
    DesktopService,
    InputService,
    ShellService,
)
from extension.infrastructure.mock.window import (
    WindowMock,
    WindowServiceMock,
)
from extension.infrastructure.mock.desktop import (
    DesktopMock,
    DesktopServiceMock,
)
from extension.infrastructure.mock.input import (
    InputServiceMock,
)
from extension.infrastructure.mock.shell import (
    ShellServiceMock,
)


class Dependency(AbstractDependency):
    def config(self, binder) -> None:
        binder.bind(Window, WindowMock)
        binder.bind(WindowService, WindowServiceMock)
        binder.bind(Desktop, DesktopMock)
        binder.bind(DesktopService, DesktopServiceMock)
        binder.bind(InputService, InputServiceMock)
        binder.bind(ShellService, ShellServiceMock)
