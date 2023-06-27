from ctypes import (
    windll,
    pointer,
)
from ctypes.wintypes import (
    POINT,
    HWND,
    DWORD,
)
import enum
from logging import getLogger


RETRY_COUNT = 3
RETRY_WAIT_MS = 100


logger = getLogger(__name__)


class WindowNotFoundError(ValueError):
    pass


class IMEWindowNotFoundError(WindowNotFoundError):
    pass


user32 = windll.user32
imm32 = windll.imm32


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

    def __init__(self, hwnd):
        self._hwnd = hwnd
        if not user32.IsWindow(self._hwnd):
            raise WindowNotFoundError(f'hwnd={self._hwnd}')
        thread_id: DWORD = user32.GetWindowThreadProcessId(self._hwnd, 0)
        self._thread = Thread(thread_id)

    def __eq__(self, other: 'Window') -> bool:
        return self._hwnd == other._hwnd

    def __str__(self) -> str:
        return str(self._hwnd)

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

        user32.BringWindowToTop(self._hwnd)
        user32.SetFocus(self._hwnd)

        if (new_fore == self) or (new_thread == self_thread):
            logger.debug(f'Foreground: (hwnd={old_fore}, thread={old_thread}) => (hwnd={self}, thread={self_thread})')
        else:
            logger.error(f'Foreground: (hwnd={old_fore}, thread={old_thread}) => (hwnd={new_fore}, thread={new_thread}), expected=(hwnd={self}, thread={self_thread})')
        return new_fore

    def ime_on(self):
        IME(self._hwnd).status = IME.Status.ON

    def ime_off(self):
        IME(self._hwnd).status = IME.Status.OFF

    def get_owner(self) -> 'Window':
        relation = self.__class__.Relation.OWNER
        hwnd: HWND = user32.GetWindow(self._hwnd, relation)
        return self.__class__(hwnd)

    @classmethod
    def from_hwnd(cls, hwnd: HWND) -> 'Window':
        return cls(hwnd)

    @classmethod
    def from_point(cls, point: POINT) -> 'Window':
        hwnd: HWND = user32.WindowFromPoint(point)
        if not hwnd:
            raise WindowNotFoundError(f'point=({point.x}, {point.y})')
        return cls(hwnd)

    @classmethod
    def from_foreground(cls) -> 'Window':
        hwnd: HWND = user32.GetForegroundWindow()
        if hwnd == 0:
            raise WindowNotFoundError('No foreground window')
        return cls(hwnd)


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
    def __init__(self, thread_id: DWORD):
        self._thread_id = thread_id

    def __eq__(self, other: 'Thread') -> bool:
        return self._thread_id == other._thread_id

    def __str__(self) -> str:
        return str(self._thread_id)

    def attach(self, other: 'Thread') -> 'Thread.AttacheInput':
        return Thread.AttacheInput(self._thread_id, other._thread_id)

    class AttacheInput:
        def __init__(self, self_thread_id, other_thread_id):
            self._self_thread_id = self_thread_id
            self._other_thread_id = other_thread_id
            self._attached = False

        def __enter__(self):
            self._attached = user32.AttachThreadInput(self._self_thread_id, self._other_thread_id, True)

        def __exit__(self, exc_type, exc_value, traceback):
            if self._attached:
                user32.AttachThreadInput(self._self_thread_id, self._other_thread_id, False)
