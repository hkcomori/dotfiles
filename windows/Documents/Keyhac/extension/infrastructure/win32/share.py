from ctypes import (    # type: ignore  # noqa: F401
    windll,             # type: ignore
    WINFUNCTYPE,        # type: ignore
    pointer,
    sizeof,
    create_unicode_buffer,
)
from ctypes.wintypes import (
    HWND,
    DWORD,
    INT,
    UINT,
    LONG,
    BOOL,
    LPVOID,
    POINT,
    LPWSTR,
    LPDWORD,
    LPPOINT,
    LPARAM,
    WPARAM,
)


class HRESULT:
    def __init__(self, hresult: int):
        self._hresult = hresult

    def __bool__(self) -> bool:
        return self._hresult >= 0


WNDENUMPROC = WINFUNCTYPE(BOOL, HWND, LPARAM)


SendMessage = windll.user32.SendMessageW
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-sendmessagew
"""
SendMessage.argtypes = (HWND, UINT, WPARAM, LPARAM,)
SendMessage.restype = INT


PostMessage = windll.user32.PostMessageW
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-postmessagew
"""
PostMessage.argtypes = (HWND, INT, INT, INT,)
PostMessage.restype = INT


DwmGetWindowAttribute = windll.dwmapi.DwmGetWindowAttribute
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/dwmapi/nf-dwmapi-dwmgetwindowattribute
"""
DwmGetWindowAttribute.argtypes = (HWND, DWORD, LPVOID, DWORD,)
DwmGetWindowAttribute.restype = HRESULT


GetWindowThreadProcessId = windll.user32.GetWindowThreadProcessId
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowthreadprocessid
"""
GetWindowThreadProcessId.argtypes = (HWND, LPDWORD,)
GetWindowThreadProcessId.restype = DWORD


GetWindowTextLengthW = windll.user32.GetWindowTextLengthW
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowtextlengthw
"""
GetWindowTextLengthW.argtypes = (HWND,)
GetWindowTextLengthW.restype = INT


GetWindowTextW = windll.user32.GetWindowTextW
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowtextw
"""
GetWindowTextW.argtypes = (HWND, LPWSTR, INT,)
GetWindowTextW.restype = INT


GetClassNameW = windll.user32.GetClassNameW
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getclassnamew
"""
GetClassNameW.argtypes = (HWND, LPWSTR, INT,)
GetClassNameW.restype = INT


SetForegroundWindow = windll.user32.SetForegroundWindow
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-setforegroundwindow
"""
SetForegroundWindow.argtypes = (HWND,)
SetForegroundWindow.restype = BOOL


GetForegroundWindow = windll.user32.GetForegroundWindow
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getforegroundwindow
"""
GetForegroundWindow.argtypes = ()
GetForegroundWindow.restype = HWND


GetWindow = windll.user32.GetWindow
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindow
"""
GetWindow.argtypes = (HWND, UINT,)
GetWindow.restype = HWND


IsIconic = windll.user32.IsIconic
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-isiconic
"""
IsIconic.argtypes = (HWND,)
IsIconic.restype = BOOL


ShowWindow = windll.user32.ShowWindow
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-showwindow
"""
ShowWindow.argtypes = (HWND, INT,)
ShowWindow.restype = BOOL


GetWindowLongW = windll.user32.GetWindowLongW
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getwindowlongw
"""
GetWindowLongW.argtypes = (HWND, INT,)
GetWindowLongW.restype = LONG


GetParent = windll.user32.GetParent
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getparent
"""
GetParent.argtypes = (HWND,)
GetParent.restype = HWND


WindowFromPoint = windll.user32.WindowFromPoint
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-windowfrompoint
"""
WindowFromPoint.argtypes = (POINT,)
WindowFromPoint.restype = HWND


GetFocus = windll.user32.GetFocus
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getfocus
"""
GetFocus.argtypes = ()
GetFocus.restype = HWND


EnumWindows = windll.user32.EnumWindows
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-enumwindows
"""
EnumWindows.argtypes = (WNDENUMPROC, LPARAM,)
EnumWindows.restype = BOOL


IsWindowVisible = windll.user32.IsWindowVisible
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-iswindowvisible
"""
IsWindowVisible.argtypes = (HWND,)
IsWindowVisible.restype = BOOL


GetCursorPos = windll.user32.GetCursorPos
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-getcursorpos
"""
GetCursorPos.argtypes = (LPPOINT,)
GetCursorPos.restype = BOOL


AttachThreadInput = windll.user32.AttachThreadInput
"""
https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/nf-winuser-attachthreadinput
"""
AttachThreadInput.argtypes = (DWORD, DWORD, BOOL,)
AttachThreadInput.restype = BOOL
