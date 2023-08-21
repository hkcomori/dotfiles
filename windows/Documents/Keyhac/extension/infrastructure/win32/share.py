import enum
from ctypes import (    # type: ignore  # noqa: F401
    windll,             # type: ignore
    WINFUNCTYPE,        # type: ignore
    c_wchar,
    c_ulong,
    _Pointer,
    Array,
)
from ctypes.wintypes import (
    DWORD,
    INT,
    UINT,
    LONG,
    BOOL,
    POINT,
    LPARAM,
    WPARAM,
)
from typing import (
    Optional,
)


HWND = LONG


class HRESULT:
    """https://learn.microsoft.com/ja-jp/windows/win32/seccrypto/common-hresult-values"""
    def __init__(self, hresult: int):
        self._hresult = hresult

    def __bool__(self) -> bool:
        """https://learn.microsoft.com/ja-jp/windows/win32/api/winerror/nf-winerror-succeeded"""
        return self._hresult >= 0


WNDENUMPROC = WINFUNCTYPE(BOOL, HWND, LPARAM)


class GetWindowCmd(enum.Enum):
    """
    指定したウィンドウと、ハンドルを取得するウィンドウの間のリレーションシップ。

    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindow
    """

    GW_CHILD = UINT(5)
    GW_ENABLEDPOPUP = UINT(6)
    GW_HWNDFIRST = UINT(0)
    GW_HWNDLAST = UINT(1)
    GW_HWNDNEXT = UINT(2)
    GW_HWNDPREV = UINT(3)
    GW_OWNER = UINT(4)


class ShowWindowCmd(enum.Enum):
    """
    ウィンドウの表示方法を制御します。

    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-showwindow
    """

    SW_HIDE = UINT(0)
    SW_SHOWNORMAL = UINT(1)
    SW_NORMAL = UINT(1)
    SW_SHOWMINIMIZED = UINT(2)
    SW_SHOWMAXIMIZED = UINT(3)
    SW_MAXIMIZE = UINT(3)
    SW_SHOWNOACTIVATE = UINT(4)
    SW_SHOW = UINT(5)
    SW_MINIMIZE = UINT(6)
    SW_SHOWMINNOACTIVE = UINT(7)
    SW_SHOWNA = UINT(8)
    SW_RESTORE = UINT(9)
    SW_SHOWDEFAULT = UINT(10)
    SW_FORCEMINIMIZE = UINT(11)


def GetLastError() -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/errhandlingapi/nf-errhandlingapi-getlasterror
    """
    res: int = windll.kernel32.GetLastError()
    return res


def SendMessage(hwnd: int, msg: int, wParam: int, lParam: int) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-sendmessagew
    """
    res: int = windll.user32.SendMessageW(HWND(hwnd), UINT(msg), WPARAM(wParam), LPARAM(lParam))
    return res


def PostMessage(hwnd: int, msg: int, wParam: int, lParam: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-postmessagew
    """
    res: bool = windll.user32.PostMessageW(HWND(hwnd), UINT(msg), WPARAM(wParam), LPARAM(lParam))
    return res


def DwmGetWindowAttribute(hwnd: int, dwAttribute: int, pvAttribute: _Pointer, cbAttribute: int) -> HRESULT:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/dwmapi/nf-dwmapi-dwmgetwindowattribute
    """
    res: int = windll.dwmapi.DwmGetWindowAttribute(HWND(hwnd), DWORD(dwAttribute), pvAttribute, DWORD(cbAttribute))
    return HRESULT(res)


def GetWindowThreadProcessId(hwnd: int, lpdwProcessId: _Pointer) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowthreadprocessid
    """
    res: int = windll.user32.GetWindowThreadProcessId(HWND(hwnd), lpdwProcessId)
    return res


def GetWindowTextLengthW(hwnd: int) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowtextlengthw
    """
    res: int = windll.user32.GetWindowTextLengthW(HWND(hwnd))
    return res


def GetWindowTextW(hwnd: int, lpString: Array, nMaxCount: int) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowtextw
    """
    res: int = windll.user32.GetWindowTextW(HWND(hwnd), lpString, INT(nMaxCount))
    return res


def GetClassNameW(hwnd: int, lpString: Array, nMaxCount: int) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getclassnamew
    """
    res: int = windll.user32.GetClassNameW(HWND(hwnd), lpString, INT(nMaxCount))
    return res


def SetForegroundWindow(hwnd: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-setforegroundwindow
    """
    res: bool = windll.user32.SetForegroundWindow(HWND(hwnd))
    return res


def GetForegroundWindow() -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getforegroundwindow
    """
    res: int = windll.user32.GetForegroundWindow()
    return res


def GetWindow(hwnd: int, uCmd: GetWindowCmd) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindow
    """
    res: int = windll.user32.GetWindow(HWND(hwnd), uCmd.value)
    return res


def IsIconic(hwnd: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-isiconic
    """
    res: bool = windll.user32.IsIconic(HWND(hwnd))
    return res


def IsWindow(hwnd: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-iswindow
    """
    res: bool = windll.user32.IsWindow(HWND(hwnd))
    return res


def ShowWindow(hwnd: int, nCmdShow: ShowWindowCmd) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-showwindow
    """
    res: bool = windll.user32.ShowWindow(HWND(hwnd), nCmdShow.value)
    return res


def GetWindowLongW(hwnd: int, nIndex: int) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowlongw
    """
    res: int = windll.user32.GetWindowLongW(HWND(hwnd), INT(nIndex))
    return res


def GetParent(hwnd) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getparent
    """
    res: int = windll.user32.GetParent(HWND(hwnd))
    return res


def WindowFromPoint(point: POINT) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-windowfrompoint
    """
    res: int = windll.user32.WindowFromPoint(point)
    return res


def GetFocus() -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getfocus
    """
    res: int = windll.user32.GetFocus()
    return res


def EnumWindows(lpEnumFunc: WINFUNCTYPE, lParam: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-enumwindows
    """
    res: bool = windll.user32.EnumWindows(lpEnumFunc, LPARAM(lParam))
    return res


def EnumChildWindows(hwndParent: int, lpEnumFunc: WINFUNCTYPE, lParam: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-enumchildwindows
    """
    res: bool = windll.user32.EnumChildWindows(hwndParent, lpEnumFunc, LPARAM(lParam))
    return res


def IsWindowVisible(hwnd: int) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-iswindowvisible
    """
    res: bool = windll.user32.IsWindowVisible(HWND(hwnd))
    return res


def GetCursorPos(lpPoint: _Pointer) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getcursorpos
    """
    res: bool = windll.user32.GetCursorPos(lpPoint)
    return res


def AttachThreadInput(idAttach: int, idAttachTo: int, fAttach: bool) -> bool:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-attachthreadinput
    """
    res: bool = windll.user32.AttachThreadInput(DWORD(idAttach), DWORD(idAttachTo), BOOL(fAttach))
    return res


def ImmGetDefaultIMEWnd(hwnd: int) -> int:
    """
    https://learn.microsoft.com/ja-jp/windows/win32/api/imm/nf-imm-immgetdefaultimewnd
    """
    res: int = windll.imm32.ImmGetDefaultIMEWnd(HWND(hwnd))
    return res
