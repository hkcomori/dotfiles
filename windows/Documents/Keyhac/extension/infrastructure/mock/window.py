from extension.domain.window import (
    WindowId,
    WindowQuery,
    Window,
    WindowService,
)


class WindowMock(Window):
    def __init__(
        self,
        window_id: WindowId,
        exe_name: str = '',
        class_name: str = '',
        window_text: str = '',
    ):
        self._window_id = window_id
        self._exe_name = exe_name
        self._class_name = class_name
        self._window_text = window_text
        self.is_active = False
        self.ime_enabled = False

    @property
    def window_id(self) -> WindowId:
        return self._window_id

    @property
    def exe_name(self) -> str:
        return self._exe_name

    @property
    def class_name(self) -> str:
        return self._class_name

    @property
    def window_text(self) -> str:
        return self._window_text

    def activate(self) -> bool:
        self.is_active = True
        return True

    def ime_on(self) -> bool:
        self.ime_enabled = True
        return True

    def ime_off(self) -> bool:
        self.ime_enabled = False
        return True

    def close(self) -> bool:
        return True


class WindowServiceMock(WindowService):
    def from_id(self, window_id: WindowId) -> 'Window':
        return WindowMock(window_id)

    def from_active(self) -> 'Window':
        wnd = self.from_id(WindowId(1))
        wnd.activate()
        return wnd

    def from_pointer(self) -> 'Window':
        return self.from_id(WindowId(1))

    def from_query(self, query: WindowQuery) -> 'Window':
        return self.from_id(WindowId(1))
