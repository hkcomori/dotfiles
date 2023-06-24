from ctypes import (
    windll,
    pointer,
    c_bool,
)
from ctypes.wintypes import (
    POINT,
    HWND,
)
import enum
from logging import getLogger
from time import sleep


import pyauto


RETRY_COUNT = 3
RETRY_WAIT_MS = 100


logger = getLogger(__name__)


class WindowNotFoundError(ValueError):
    pass


class IMEWindowNotFoundError(WindowNotFoundError):
    pass


def restype_bool(value) -> bool:
    if value == 0:
        return True
    return False


user32 = windll.user32
user32.IsWindow.restype = c_bool
user32.SetForegroundWindow.restype = c_bool
user32.GetForegroundWindow.restype = HWND
user32.GetWindow.restype = HWND
user32.WindowFromPoint.restype = HWND

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
        self._window = pyauto.Window.fromHWND(self._hwnd)

    def __equal__(self, other: 'Window') -> bool:
        return self._hwnd == other._hwnd

    @property
    def title(self) -> str:
        return self._window.getText()

    @property
    def process_name(self) -> str:
        return self._window.getProcessName()

    def _set_foreground(self, force: bool = False):
        self._window.setForeground(force)

    def set_foreground(self) -> 'Window':
        # _target = pyauto.Window.fromHWND(self._hwnd)
        # current_window = self.__class__.from_foreground()
        # if current_window == self:
        #     return self

        for i in range(0, RETRY_COUNT):
            self._set_foreground(True)
            current = self.__class__.from_foreground()
            result = self == current
            # result: bool = user32.SetForegroundWindow(self._hwnd)
            # sleep(RETRY_WAIT_MS)
            if not result:
                t_text = self.process_name
                c_text = current.process_name
                logger.debug(f'SetForegroundWindow failed: target={t_text}, fore={c_text}')
                continue
            # try:
            # new_window = self.__class__.from_foreground()
            # except WindowNotFoundError:
            #     continue
            # if new_window != current_window:
            #     break

        return self
        # if new_window == self:
        #     return self
        # if new_window.get_owner() == self:
        #     return new_window
        # raise WindowNotFoundError(f'hwnd={self._hwnd}')

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
        if not hwnd:
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
