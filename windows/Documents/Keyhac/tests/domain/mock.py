from extension.vendor.injector import (
    Injector,
)

from extension.domain.window import (
    Window,
    WindowService,
)
from extension.domain.desktop import (
    Desktop,
    DesktopService,
)
from extension.domain.input import (
    InputService,
)
from extension.domain.shell import (
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


class Dependency:
    def __init__(self):
        self.injector = Injector(self.config)

    def config(self, binder):
        binder.bind(Window, WindowMock)
        binder.bind(WindowService, WindowServiceMock)
        binder.bind(Desktop, DesktopMock)
        binder.bind(DesktopService, DesktopServiceMock)
        binder.bind(InputService, InputServiceMock)
        binder.bind(ShellService, ShellServiceMock)

    def resolve(self, cls):
        return self.injector.get(cls)
