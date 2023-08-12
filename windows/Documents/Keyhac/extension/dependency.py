from abc import ABCMeta, abstractmethod
from typing import (
    Type,
    TypeVar,
)

from .vendor.injector import (
    Injector,
    Binder,
    InstanceProvider,
)

from .domain.require_injection import (
    KeymapRegistry,
    KeymapService,
    Window,
    WindowService,
    Desktop,
    DesktopService,
    ShellService,
    InputService,
)
from .infrastructure.keyhac import (
    KeymapKeyhac,
    KeymapRegistryKeyhac,
    KeymapServiceKeyhac,
    ShellServiceKeyhac,
    InputServiceKeyhac,
)
from .infrastructure.win32 import (
    DesktopWin32,
    DesktopServiceWin32,
    WindowWin32,
    WindowServiceWin32,
)


T1 = TypeVar('T1')


class AbstractDependency(metaclass=ABCMeta):
    @abstractmethod
    def config(self, binder: Binder) -> None:
        raise NotImplementedError

    def resolve(self, class_: Type[T1]) -> T1:
        injector = Injector(self.config)
        return injector.get(class_)


class Dependency(AbstractDependency):
    def __init__(self, keymap: KeymapKeyhac):
        self._keymap = keymap

    def config(self, binder: Binder) -> None:
        binder.bind(KeymapKeyhac, to=InstanceProvider(self._keymap))
        binder.bind(KeymapRegistry, to=KeymapRegistryKeyhac)
        binder.bind(KeymapService, to=KeymapServiceKeyhac)
        binder.bind(Window, to=WindowWin32)
        binder.bind(WindowService, to=WindowServiceWin32)
        binder.bind(Desktop, to=DesktopWin32)
        binder.bind(DesktopService, to=DesktopServiceWin32)
        binder.bind(ShellService, to=ShellServiceKeyhac)
        binder.bind(InputService, to=InputServiceKeyhac)
