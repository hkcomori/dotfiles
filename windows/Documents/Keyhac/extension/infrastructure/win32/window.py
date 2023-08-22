import enum
from ctypes import (
    c_ulong,
    pointer,
    sizeof,
    create_unicode_buffer,
)
from threading import currentThread
from typing import (
    Optional,
    Tuple,
)

import pyauto   # type: ignore

from extension.domain.exception import (
    DomainTypeError,
    DomainValueError,
    DomainRuntimeError,
    WindowNotFoundError,
)
from extension.domain.window import (
    WindowId,
    WindowQuery,
    Window,
    WindowService,
)
from .share import (
    SystemError,
    POINT,
    BOOL,
    LPARAM,
    WNDENUMPROC,
    GetWindowCmd,
    ShowWindowCmd,
    HRESULT,
    GetLastError,
    SendMessage,
    DwmGetWindowAttribute,
    GetWindowThreadProcessId,
    GetWindowTextLengthW,
    GetWindowTextW,
    GetClassNameW,
    SetForegroundWindow,
    GetForegroundWindow,
    BringWindowToTop,
    GetWindow,
    IsIconic,
    IsWindow,
    ShowWindow,
    GetWindowLongW,
    GetParent,
    WindowFromPoint,
    GetFocus,
    EnumChildWindows,
    IsWindowVisible,
    GetCursorPos,
    AttachThreadInput,
    ImmGetDefaultIMEWnd,
)


MAX_CLASS_NAME_LENGTH = 256
"""クラス名の最大長"""


class AttacheThreadInputError(DomainRuntimeError):
    """スレッド入力の接続に失敗したときのエラー"""

    def __init__(self, err_code: int, thread_id1: int, thread_id2: int):
        err = SystemError(err_code)
        msg = f'{err.name}: Thread({thread_id1}), Thread({thread_id2})'
        super().__init__(msg)


class SetForegroundError(DomainRuntimeError):
    """フォアグラウンド化に失敗したときのエラー"""

    def __init__(self, err_code: int, target: Window):
        err = SystemError(err_code)
        msg = f'{err.name}: {repr(target)}'
        super().__init__(msg)


class WindowInfo:
    def __init__(self, window_id: WindowId):
        self._window_id = window_id

    @property
    def window_id(self) -> WindowId:
        return self._window_id

    def _get_thread_info(self) -> Tuple['Thread', str]:
        pid = c_ulong()
        thread_id: int = GetWindowThreadProcessId(self.window_id.value, pointer(pid))
        thread = Thread(thread_id)
        pyauto_window = pyauto.Window.fromHWND(self.window_id.value)
        exe_name = pyauto_window.getProcessName()
        return thread, exe_name

    @property
    def thread(self) -> 'Thread':
        if not hasattr(self, '_thread'):
            self._thread, self._exe_name = self._get_thread_info()
        return self._thread

    @property
    def exe_name(self) -> str:
        if not hasattr(self, '_exe_name'):
            self._thread, self._exe_name = self._get_thread_info()
        return self._exe_name

    def _get_window_text(self):
        length = GetWindowTextLengthW(self.window_id.value)
        buff = create_unicode_buffer(length + 1)
        GetWindowTextW(self.window_id.value, buff, length + 1)
        return buff.value

    @property
    def window_text(self) -> str:
        if not hasattr(self, '_title'):
            self._window_text: str = self._get_window_text()
        return self._window_text

    def _get_class_name(self) -> str:
        buff = create_unicode_buffer(MAX_CLASS_NAME_LENGTH + 1)
        GetClassNameW(self.window_id.value, buff, MAX_CLASS_NAME_LENGTH + 1)
        return buff.value

    @property
    def class_name(self) -> str:
        if not hasattr(self, '_class_name'):
            self._class_name: str = self._get_class_name()
        return self._class_name

    @property
    def is_minimized(self) -> bool:
        return IsIconic(self.window_id.value)

    @property
    def is_child(self) -> bool:
        GWL_STYLE = -16
        WS_CHILD = 0x40000000
        return GetWindowLongW(self.window_id.value, GWL_STYLE) & WS_CHILD != 0

    @property
    def is_visible(self) -> bool:
        return IsWindowVisible(self.window_id.value)

    @property
    def is_cloaked(self) -> bool:
        cloaked = BOOL()
        DWMWA_CLOAKED = 14
        result: HRESULT = DwmGetWindowAttribute(
            self.window_id.value, DWMWA_CLOAKED, pointer(cloaked), sizeof(cloaked))
        return bool(result) and bool(cloaked)


class WindowWin32(Window):
    def __init__(self, window_id: WindowId):
        if not IsWindow(window_id.value):
            raise WindowNotFoundError(f'hwnd={window_id.value} is not window')
        self._window_id = window_id
        self._window_info = WindowInfo(window_id)

    def activate(self) -> bool:
        current_wnd = WindowServiceWin32().from_active()

        if self._window_info.is_minimized:
            self._disable_minimize()

        if current_wnd == self:
            return True

        try:
            with Thread.from_current().attach(current_wnd.thread):
                with current_wnd.thread.attach(self.thread):
                    wnd = self._set_foreground(current_wnd)
                    BringWindowToTop(wnd.window_id.value)
                    return True
        except AttacheThreadInputError:
            pass

        wnd = self._set_foreground(current_wnd)
        BringWindowToTop(wnd.window_id.value)
        return True

    def ime_on(self) -> bool:
        ime = IME(self.window_id)
        ime.status = IME.Status.ON
        return True

    def ime_off(self) -> bool:
        ime = IME(self.window_id)
        ime.status = IME.Status.OFF
        return True

    @property
    def window_id(self) -> WindowId:
        return self._window_id

    @property
    def thread(self) -> 'Thread':
        return self._window_info.thread

    @property
    def exe_name(self) -> str:
        return self._window_info.exe_name

    @property
    def window_text(self) -> str:
        return self._window_info.window_text

    @property
    def class_name(self) -> str:
        return self._window_info.class_name

    def _disable_minimize(self) -> None:
        ShowWindow(self.window_id.value, ShowWindowCmd.SW_RESTORE)

    def _set_foreground(self, current: 'WindowWin32') -> 'WindowWin32':
        """
        See: AutoHotkey/source/window.cpp: AttemptSetForeground
        """
        if not SetForegroundWindow(self.window_id.value):
            raise SetForegroundError(GetLastError(), self)
        while True:
            hwnd = GetForegroundWindow()
            try:
                new_id = WindowId(hwnd)
            except DomainValueError:
                continue
            if new_id == self.window_id:
                return self
            if new_id != current.window_id:
                break
        new_wnd = self.__class__(new_id)
        owner_hwnd = GetWindow(new_id.value, GetWindowCmd.GW_OWNER)
        try:
            owner_window_id = WindowId(owner_hwnd)
        except DomainValueError:
            return new_wnd
        if self.window_id == owner_window_id:
            return new_wnd
        raise DomainRuntimeError(
            f'activate failure: target={self}, new={new_wnd}')


class WindowServiceWin32(WindowService):
    def from_id(self, window_id: WindowId) -> WindowWin32:
        window_info = WindowInfo(window_id)
        if not window_info.is_visible or window_info.is_cloaked:
            raise WindowNotFoundError(f'hwnd={window_id.value} is not visible')
        return WindowWin32(window_id)

    def from_pointer(self) -> WindowWin32:
        point = Cursor().point
        hwnd = WindowFromPoint(point)
        window_id = WindowId(hwnd)
        wnd = WindowWin32(window_id)
        ancestor_wnd = self._get_first_ancestor(window_id)
        if ancestor_wnd.class_name == 'ApplicationFrameWindow':
            return wnd
        return ancestor_wnd

    @staticmethod
    def _get_first_ancestor(window_id: WindowId) -> 'WindowWin32':
        """
        Returns the first ancestor of self that isn't itself a child.
        See: AutoHotkey/source/window.cpp: GetNonChildParent
        """
        parent_prev = window_id
        while True:
            if not WindowInfo(parent_prev).is_child:
                return WindowWin32(parent_prev)
            hwnd = GetParent(parent_prev.value)
            try:
                parent = WindowId(hwnd)
            except DomainValueError:
                return WindowWin32(parent_prev)
            parent_prev = parent

    def from_active(self) -> WindowWin32:
        hwnd = GetForegroundWindow()
        try:
            fore_window_id = WindowId(hwnd)
        except DomainValueError:
            raise WindowNotFoundError('No foreground window')
        wnd = WindowWin32(fore_window_id)
        try:
            with wnd.thread.attach(Thread.from_current()):
                focus_window_id = WindowId(GetFocus())
        except (DomainValueError, DomainRuntimeError):
            return wnd
        return WindowWin32(focus_window_id)

    def from_query(self, query: WindowQuery) -> WindowWin32:
        return self._from_query(query, None)

    def _from_query(
        self,
        query: WindowQuery,
        parent: Optional[WindowWin32],
    ) -> WindowWin32:
        founds = []

        def _callback(hwnd: int, lParam: LPARAM) -> bool:
            try:
                window_id = WindowId(hwnd)
                window = self.from_id(window_id)
            except (DomainValueError, WindowNotFoundError):
                return True
            # UWPアプリはトップレベルウィンドウではないので、子ウィンドウを検索する
            if window.class_name == 'ApplicationFrameWindow':
                return EnumChildWindows(hwnd, WNDENUMPROC(_callback), 0)
            elif query.match(window):
                founds.append(window)
                return False
            return True

        hwnd = parent.window_id.value if parent is not None else 0
        EnumChildWindows(hwnd, WNDENUMPROC(_callback), 0)

        if len(founds) > 0:
            return founds[0]
        raise WindowNotFoundError(f'No window matched: {repr(query)}')


class Cursor():
    @property
    def point(self) -> POINT:
        point = POINT()
        GetCursorPos(pointer(point))
        return point


class IME():
    @enum.unique
    class Status(enum.Enum):
        """
        IMEの有効/無効状態を表すパラメーター
        """
        ON = 0x01
        OFF = 0x00

    def __init__(self, window_id: WindowId):
        hwnd = ImmGetDefaultIMEWnd(window_id.value)
        self._window_id = WindowId(hwnd)

    def __repr__(self) -> str:
        return f'IME({self.window_id})'

    def __str__(self) -> str:
        return str(self.window_id)

    @property
    def window_id(self) -> WindowId:
        return self._window_id

    @property
    def status(self) -> 'IME.Status':
        result = SendMessage(self.window_id.value, 0x283, 0x5, 0x00)
        return IME.Status(result)

    @status.setter
    def status(self, status: 'IME.Status'):
        SendMessage(self.window_id.value, 0x283, 0x6, status.value)


class Thread():
    def __init__(self, thread_id: int):
        self._thread_id = thread_id

    def __eq__(self, other) -> bool:
        if not isinstance(other, self.__class__):
            raise DomainTypeError(other, self.__class__)
        return self._thread_id == other._thread_id

    def __repr__(self) -> str:
        return f'Thread({self._thread_id})'

    def __str__(self) -> str:
        return str(self._thread_id)

    @classmethod
    def from_current(cls) -> 'Thread':
        thread_id = currentThread().ident
        if thread_id is None:
            raise DomainRuntimeError('currentThread failure')
        return cls(thread_id)

    def attach(self, other: 'Thread') -> 'Thread.AttacheInput':
        return Thread.AttacheInput(self._thread_id, other._thread_id)

    class AttacheInput:
        def __init__(self, self_thread_id: int, other_thread_id: int):
            self._self_thread_id = self_thread_id
            self._other_thread_id = other_thread_id
            self._attached = False

        def __enter__(self):
            if self._self_thread_id != self._other_thread_id:
                self._attached = AttachThreadInput(self._self_thread_id, self._other_thread_id, True)
                if not self._attached:
                    raise AttacheThreadInputError(
                        GetLastError(), self._self_thread_id, self._other_thread_id)

        def __exit__(self, exc_type, exc_value, traceback):
            if self._attached:
                AttachThreadInput(self._self_thread_id, self._other_thread_id, False)
