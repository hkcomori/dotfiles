from ctypes import (
    windll,
    WINFUNCTYPE,
    pointer,
    sizeof,
    create_unicode_buffer,
)
from ctypes.wintypes import (
    POINT,
    HWND,
    BOOL,
    LPARAM,
)
import enum
from logging import getLogger
import re
from threading import currentThread

import pyauto


RETRY_COUNT = 3
RETRY_WAIT_MS = 100


logger = getLogger(__name__)


class WindowNotFoundError(ValueError):
    pass


class IMEWindowNotFoundError(WindowNotFoundError):
    pass


class HRESULT:
    def __init__(self, hresult: int):
        self._hresult = hresult

    def __bool__(self) -> bool:
        return self._hresult >= 0


kernel32 = windll.kernel32
user32 = windll.user32
imm32 = windll.imm32
dwmapi = windll.dwmapi

dwmapi.DwmGetWindowAttribute.restype = HRESULT

WNDENUMPROC = WINFUNCTYPE(BOOL, HWND, LPARAM)

MAX_CLASS_NAME_LENGTH = 256
"""クラス名の最大長"""


class Window:
    @enum.unique
    class Relation(enum.IntEnum):
        """
        ウィンドウ間の関係を表すパラメーター
        https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindow
        """
        CHILD = 5   # GW_CHILD
        POPUP = 6   # GW_ENABLEDPOPUP
        FIRST = 0   # GW_HWNDFIRST
        LAST = 1    # GW_HWNDLAST
        NEXT = 2    # GW_HWNDNEXT
        PREV = 3    # GW_HWNDPREV
        OWNER = 4   # GW_OWNER

    def __init__(self, hwnd: HWND):
        self._hwnd = hwnd
        if not user32.IsWindow(self._hwnd):
            raise WindowNotFoundError(f'hwnd={self._hwnd}')
        thread_id: int = user32.GetWindowThreadProcessId(self._hwnd, 0)
        self._thread = Thread(thread_id)
        pyauto_window = pyauto.Window.fromHWND(self._hwnd)
        self._process_name: str = pyauto_window.getProcessName()
        self._title = self._get_title()
        self._class_name = self._get_class_name()

    def __eq__(self, other) -> bool:
        if not isinstance(other, self.__class__):
            raise NotImplementedError
        return self._hwnd == other._hwnd

    def __str__(self) -> str:
        return str(self._hwnd)

    @property
    def process_name(self) -> str:
        return self._process_name

    def _get_title(self):
        length = user32.GetWindowTextLengthW(self._hwnd)
        buff = create_unicode_buffer(length + 1)
        user32.GetWindowTextW(self._hwnd, buff, length + 1)
        return buff.value

    @property
    def title(self) -> str:
        return self._title

    def _get_class_name(self) -> str:
        buff = create_unicode_buffer(MAX_CLASS_NAME_LENGTH + 1)
        user32.GetClassNameW(self._hwnd, buff, MAX_CLASS_NAME_LENGTH + 1)
        return buff.value

    @property
    def class_name(self) -> str:
        return self._class_name

    def _set_foreground(self, current: 'Window') -> 'Window':
        user32.SetForegroundWindow(self._hwnd)
        new_hwnd = current._hwnd
        while new_hwnd == current._hwnd:
            new_hwnd: HWND = user32.GetForegroundWindow()
        if new_hwnd == self._hwnd:
            return self
        if (new_hwnd != current._hwnd) and (self._hwnd == user32.GetWindow(new_hwnd, Window.Relation.OWNER)):
            return self.__class__.from_hwnd(new_hwnd)
        raise RuntimeError(f'SetForegroundWindow failure: target={self}, current={current}, new={new_hwnd}')

    def set_foreground(self) -> 'Window':
        self_thread = self._thread
        old_fore = self.__class__.from_foreground()
        old_thread = old_fore._thread

        if user32.IsIconic(self._hwnd):
            SW_RESTORE = 9
            user32.ShowWindow(self._hwnd, SW_RESTORE)

        if old_fore == self:
            logger.debug(f'Foreground: (hwnd={old_fore}, thread={old_thread}) => (hwnd={self}, thread={self_thread}), Same hwnd')
            return old_fore
        if old_thread == self_thread:
            logger.debug(f'Foreground: (hwnd={old_fore}, thread={old_thread}) => (hwnd={self}, thread={self_thread}), Same thread')
            return old_fore

        with self_thread.attach(old_thread):
            new_fore = self._set_foreground(old_fore)
            new_thread = new_fore._thread

        if not user32.BringWindowToTop(self._hwnd):
            logger.error(f'BringWindowTop failure: {self._hwnd}')

        with self_thread.attach(Thread.from_current()):
            if user32.SetFocus(self._hwnd) == 0:
                logger.error(f'SetFocus failure: {self._hwnd}')

        if (new_fore == self) or (new_thread == self_thread):
            logger.debug(f'Foreground: (hwnd={old_fore}, thread={old_thread}) => (hwnd={self}, thread={self_thread})')
        else:
            logger.error(f'Foreground: (hwnd={old_fore}, thread={old_thread}) => (hwnd={new_fore}, thread={new_thread}), expected=(hwnd={self}, thread={self_thread})')
        return new_fore

    def ime_on(self):
        IME(self._hwnd).status = IME.Status.ON

    def ime_off(self):
        IME(self._hwnd).status = IME.Status.OFF

    @classmethod
    def from_hwnd(cls, hwnd: HWND, allow_hidden: bool = False) -> 'Window':
        if allow_hidden or cls.is_visible(hwnd):
            return cls(hwnd)
        raise WindowNotFoundError(f'hwnd={hwnd}')

    @classmethod
    def from_point(cls, point: POINT) -> 'Window':
        hwnd: HWND = user32.WindowFromPoint(point)
        if hwnd == 0:
            raise WindowNotFoundError(f'point=({point.x}, {point.y})')
        return cls(hwnd)

    @classmethod
    def from_foreground(cls) -> 'Window':
        hwnd: HWND = user32.GetForegroundWindow()
        if hwnd == 0:
            raise WindowNotFoundError('No foreground window')
        return cls(hwnd)

    @classmethod
    def from_find(
        cls,
        process_name: str = '',
        title: str = '',
        class_name: str = '',
    ) -> 'Window':
        founds = []

        def _callback(hwnd: HWND, lparam: LPARAM) -> bool:
            try:
                window = Window.from_hwnd(hwnd)
            except WindowNotFoundError:
                return True
            if all((
                any((len(process_name) == 0, re.search(process_name, window.process_name, re.IGNORECASE))),
                any((len(title) == 0, re.search(title, window.title, re.IGNORECASE))),
                any((len(class_name) == 0, re.search(class_name, window.class_name, re.IGNORECASE))),
            )):
                founds.append(window)
                w_proc = window.process_name
                w_title = window.title
                w_class = window.class_name
                logger.debug(f'Matched windows: proc={w_proc}, title={w_title}, class={w_class}')
                return False
            return True

        user32.EnumWindows(WNDENUMPROC(_callback), 0)

        if len(founds) > 0:
            return founds[0]
        raise WindowNotFoundError(f'No matched: proc={process_name}, title={title}, class={class_name}')

    @staticmethod
    def is_visible(hwnd: HWND) -> bool:
        if not user32.IsWindowVisible(hwnd):
            return False
        if Window.is_cloaked(hwnd):
            return False
        return True

    @staticmethod
    def is_cloaked(hwnd: HWND) -> bool:
        cloaked = BOOL()
        DWMWA_CLOAKED = 14
        result: HRESULT = dwmapi.DwmGetWindowAttribute(
            hwnd, DWMWA_CLOAKED, pointer(cloaked), sizeof(cloaked))
        return bool(result) and bool(cloaked)


class IME:
    @enum.unique
    class Status(enum.Enum):
        """
        IMEの有効/無効状態を表すパラメーター
        """
        ON = 0x01
        OFF = 0x00

    def __init__(self, hwnd: HWND):
        self._hwnd = imm32.ImmGetDefaultIMEWnd(hwnd)
        if not user32.IsWindow(self._hwnd):
            raise IMEWindowNotFoundError(f'hwnd={self._hwnd}')

    @property
    def status(self) -> 'IME.Status':
        result = user32.SendMessage(self._hwnd, 0x283, 0x5, 0x00)
        return IME.Status(result)

    @status.setter
    def status(self, status: 'IME.Status'):
        user32.SendMessage(self._hwnd, 0x283, 0x6, status.value)


class Cursor:
    @property
    def point(self) -> POINT:
        point = POINT()
        user32.GetCursorPos(pointer(point))
        return point


class Thread:
    def __init__(self, thread_id: int):
        self._thread_id = thread_id

    def __eq__(self, other) -> bool:
        if not isinstance(other, self.__class__):
            raise NotImplementedError
        return self._thread_id == other._thread_id

    def __str__(self) -> str:
        return str(self._thread_id)

    @classmethod
    def from_current(cls) -> 'Thread':
        thread_id = currentThread().ident
        if thread_id is None:
            raise RuntimeError('currentThread failure')
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
                self._attached = user32.AttachThreadInput(self._self_thread_id, self._other_thread_id, True)
                if not self._attached:
                    err = kernel32.GetLastError()
                    raise RuntimeError(f'AttachThreadInput failed {err}: self={self._self_thread_id}, other={self._other_thread_id}')

        def __exit__(self, exc_type, exc_value, traceback):
            if self._attached:
                user32.AttachThreadInput(self._self_thread_id, self._other_thread_id, False)
