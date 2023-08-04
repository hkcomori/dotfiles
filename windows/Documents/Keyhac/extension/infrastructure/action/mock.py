from typing import (
    List,
)

from extension.domain.window import (
    WindowQuery,
    Window,
    WindowFactory,
)


class WindowMock(Window):
    window_list: List['WindowMock'] = []

    def __init__(
        self,
        window_id: int,
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

    def __hash__(self) -> int:
        return hash(self._window_id)

    def activate(self) -> None:
        for w in self.__class__.window_list:
            if w.is_active is True:
                w.is_active = False
        self.is_active = True

    def ime_on(self) -> None:
        self.ime_enabled = True

    def ime_off(self) -> None:
        self.ime_enabled = False

    @classmethod
    def init_dummy(cls):
        cls.window_list = [
            WindowMock(1, 'hoge1.exe', 'hoge_class', 'hoge1', True),
            WindowMock(2, 'hoge2.exe', 'hoge_class', 'hoge2'),
            WindowMock(3, 'hoge3.exe', 'hoge_class', 'hoge3'),
            WindowMock(4, 'fuga1.exe', 'fuga_class', 'fuga1'),
            WindowMock(5, 'fuga2.exe', 'fuga_class', 'fuga2'),
        ]


class WindowFactoryMock(WindowFactory):
    def __init__(self):
        WindowMock.init_dummy()

    @classmethod
    def from_id(cls, window_id: int) -> 'Window':
        return WindowMock.window_list[window_id - 1]

    @classmethod
    def from_active(cls) -> 'Window':
        for w in WindowMock.window_list:
            if w.is_active is True:
                return w
        raise RuntimeError('Active window not found')

    @classmethod
    def from_pointer(cls) -> 'Window':
        return WindowMock.window_list[3]

    @classmethod
    def from_find(cls, query: WindowQuery) -> 'Window':
        raise NotImplementedError
