from typing import (
    List,
)

from extension.domain.exception import (
    DomainRuntimeError,
)
from extension.domain.window import (
    WindowId,
    WindowQuery,
    Window,
    WindowService,
)


class WindowMock(Window):
    window_list: List['WindowMock'] = []

    def __init__(
        self,
        window_id: WindowId,
        exe_name: str = '',
        class_name: str = '',
        window_text: str = '',
        is_active: bool = False,
        ime_enabled: bool = False,
    ):
        super().__init__(window_id)
        self.exe_name = exe_name
        self.class_name = class_name
        self.window_text = window_text
        self.is_active = is_active
        self.ime_enabled = ime_enabled

    def activate(self) -> bool:
        for w in self.__class__.window_list:
            if w.is_active is True:
                w.is_active = False
        self.is_active = True
        return True

    def ime_on(self) -> bool:
        self.ime_enabled = True
        return True

    def ime_off(self) -> bool:
        self.ime_enabled = False
        return True

    @classmethod
    def init_dummy(cls):
        cls.window_list = [
            WindowMock(WindowId(1), 'hoge1.exe', 'hoge_class', 'hoge1', True),
            WindowMock(WindowId(2), 'hoge2.exe', 'hoge_class', 'hoge2'),
            WindowMock(WindowId(3), 'hoge3.exe', 'hoge_class', 'hoge3'),
            WindowMock(WindowId(4), 'fuga1.exe', 'fuga_class', 'fuga1'),
            WindowMock(WindowId(5), 'fuga2.exe', 'fuga_class', 'fuga2'),
        ]


class WindowServiceMock(WindowService):
    def __init__(self):
        WindowMock.init_dummy()

    def from_id(self, window_id: WindowId) -> 'Window':
        return next((i for i in WindowMock.window_list if i._window_id == window_id))

    def from_active(self) -> 'Window':
        for w in WindowMock.window_list:
            if w.is_active is True:
                return w
        raise DomainRuntimeError('Active window not found')

    def from_pointer(self) -> 'Window':
        return WindowMock.window_list[3]

    def from_query(self, query: WindowQuery) -> 'Window':
        return WindowMock.window_list[2]
