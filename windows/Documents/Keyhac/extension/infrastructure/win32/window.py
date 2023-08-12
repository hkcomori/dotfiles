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
import re

import pyauto   # type: ignore

from extension.domain.exception import (
    DomainTypeError,
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
    GetWindow,
    IsIconic,
    IsWindow,
    ShowWindow,
    GetWindowLongW,
    GetParent,
    WindowFromPoint,
    GetFocus,
    EnumWindows,
    IsWindowVisible,
    GetCursorPos,
    AttachThreadInput,
    ImmGetDefaultIMEWnd,
)


MAX_CLASS_NAME_LENGTH = 256
"""クラス名の最大長"""


HWND = int


class IMEWindowNotFoundError(WindowNotFoundError):
    pass


class WindowWin32(Window):
    def __init__(self, window_id: WindowId):
        super().__init__(window_id)
        self._thread: Optional[Thread] = None
        self._exe_name: Optional[str] = None

    def activate(self) -> bool:
        old = WindowServiceWin32().from_active()

        if IsIconic(self._hwnd):
            ShowWindow(self._hwnd, ShowWindowCmd.SW_RESTORE)

        if old == self:
            return True

        with self.thread.attach(old.thread):
            self._set_foreground(old)

        return True

    def ime_on(self) -> bool:
        ime = IME(self._hwnd)
        ime.status = IME.Status.ON
        return True

    def ime_off(self) -> bool:
        ime = IME(self._hwnd)
        ime.status = IME.Status.OFF
        return True

    @property
    def _hwnd(self) -> HWND:
        return HWND(self._window_id.value)

    def _get_thread_info(self) -> Tuple['Thread', str]:
        pid = c_ulong()
        thread_id: int = GetWindowThreadProcessId(self._hwnd, pointer(pid))
        thread = Thread(thread_id)
        pyauto_window = pyauto.Window.fromHWND(self._hwnd)
        exe_name = pyauto_window.getProcessName()
        return thread, exe_name

    @property
    def thread(self) -> 'Thread':
        if self._thread is None:
            self._thread, self._exe_name = self._get_thread_info()
        return self._thread

    @property
    def exe_name(self) -> str:
        if self._exe_name is None:
            self._thread, self._exe_name = self._get_thread_info()
        return self._exe_name

    def _get_title(self):
        length = GetWindowTextLengthW(self._hwnd)
        buff = create_unicode_buffer(length + 1)
        GetWindowTextW(self._hwnd, buff, length + 1)
        return buff.value

    @property
    def title(self) -> str:
        if not hasattr(self, '_title'):
            self._title: str = self._get_title()
        return self._title

    def _get_class_name(self) -> str:
        buff = create_unicode_buffer(MAX_CLASS_NAME_LENGTH + 1)
        GetClassNameW(self._hwnd, buff, MAX_CLASS_NAME_LENGTH + 1)
        return buff.value

    @property
    def class_name(self) -> str:
        if not hasattr(self, '_class_name'):
            self._class_name: str = self._get_class_name()
        return self._class_name

    def _set_foreground(self, current: 'WindowWin32') -> 'WindowWin32':
        SetForegroundWindow(self._hwnd)
        while True:
            new_hwnd: HWND = GetForegroundWindow()
            if new_hwnd == 0:
                continue
            if new_hwnd == self._hwnd:
                return self
            if new_hwnd != current._hwnd:
                break
        if self._hwnd == GetWindow(new_hwnd, GetWindowCmd.GW_OWNER):
            return self.__class__(WindowId(new_hwnd))
        raise DomainRuntimeError(f'SetForegroundWindow failure: target={self}, current={current}, new={new_hwnd}')


class WindowServiceWin32(WindowService):
    def from_id(self, window_id: WindowId) -> WindowWin32:
        return WindowWin32(window_id)

    def from_pointer(self) -> WindowWin32:
        point = Cursor().point
        hwnd = WindowFromPoint(point)
        if hwnd == 0:
            raise WindowNotFoundError(f'point=({point.x}, {point.y})')
        return self._get_first_ancestor(hwnd)

    @staticmethod
    def _get_first_ancestor(hwnd: HWND) -> 'WindowWin32':
        """
        Returns the first ancestor of self that isn't itself a child.
        See: AutoHotkey/source/window.cpp: GetNonChildParent
        """
        GWL_STYLE = -16
        WS_CHILD = 0x40000000
        parent_prev = hwnd
        while True:
            if (GetWindowLongW(parent_prev, GWL_STYLE) & WS_CHILD) == 0:
                if parent_prev != 0:
                    return WindowWin32(WindowId(parent_prev))
            parent = GetParent(parent_prev)
            if parent == 0:
                return WindowWin32(WindowId(parent_prev))
            parent_prev = parent

    def from_active(self) -> WindowWin32:
        hwnd = GetForegroundWindow()
        if hwnd == 0:
            raise WindowNotFoundError('No foreground window')
        wnd = WindowWin32(WindowId(hwnd))
        with wnd.thread.attach(Thread.from_current()):
            hwnd_focus = GetFocus()
        if hwnd_focus != 0:
            return WindowWin32(WindowId(hwnd_focus))
        return wnd

    def from_query(self, query: WindowQuery) -> WindowWin32:
        exe_name = query.exe_name
        window_text = query.window_text
        class_name = query.class_name
        founds = []

        def _callback(hwnd: HWND, lParam: LPARAM) -> bool:
            if hwnd == 0:
                return True
            window = WindowWin32(WindowId(hwnd))
            if all((
                any((exe_name == '', re.search(exe_name, window.exe_name, re.IGNORECASE))),
                any((window_text == '', re.search(window_text, window.title, re.IGNORECASE))),
                any((class_name == '', re.search(class_name, window.class_name, re.IGNORECASE))),
            )):
                founds.append(window)
                return False
            return True

        EnumWindows(WNDENUMPROC(_callback), 0)

        if len(founds) > 0:
            return founds[0]
        raise WindowNotFoundError(f'No matched: proc={exe_name}, title={window_text}, class={class_name}')

    def is_visible(self, hwnd: HWND) -> bool:
        if not IsWindowVisible(hwnd):
            return False
        if self.is_cloaked(hwnd):
            return False
        return True

    def is_cloaked(self, hwnd: HWND) -> bool:
        cloaked = BOOL()
        DWMWA_CLOAKED = 14
        result: HRESULT = DwmGetWindowAttribute(
            hwnd, DWMWA_CLOAKED, pointer(cloaked), sizeof(cloaked))
        return bool(result) and bool(cloaked)


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

    def __init__(self, hwnd: HWND):
        self._hwnd = ImmGetDefaultIMEWnd(hwnd)
        if not IsWindow(self._hwnd):
            raise IMEWindowNotFoundError(f'hwnd={self._hwnd}')

    @property
    def status(self) -> 'IME.Status':
        result = SendMessage(self._hwnd, 0x283, 0x5, 0x00)
        return IME.Status(result)

    @status.setter
    def status(self, status: 'IME.Status'):
        SendMessage(self._hwnd, 0x283, 0x6, status.value)


class Thread():
    def __init__(self, thread_id: int):
        self._thread_id = thread_id

    def __eq__(self, other) -> bool:
        if not isinstance(other, self.__class__):
            raise DomainTypeError(other, self.__class__)
        return self._thread_id == other._thread_id

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
        def __init__(self, self_thread_id, other_thread_id):
            self._self_thread_id = self_thread_id
            self._other_thread_id = other_thread_id
            self._attached = False

        def __enter__(self):
            if self._self_thread_id != self._other_thread_id:
                self._attached = AttachThreadInput(self._self_thread_id, self._other_thread_id, True)
                if not self._attached:
                    err = GetLastError()
                    raise DomainRuntimeError(f'AttachThreadInput failed {err}: self={self._self_thread_id}, other={self._other_thread_id}')

        def __exit__(self, exc_type, exc_value, traceback):
            if self._attached:
                AttachThreadInput(self._self_thread_id, self._other_thread_id, False)
