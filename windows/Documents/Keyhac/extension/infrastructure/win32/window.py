from abc import abstractmethod
import enum
from collections import deque
from ctypes import (
    c_ulong,
    pointer,
    sizeof,
    create_unicode_buffer,
)
import itertools
from threading import currentThread
from typing import (
    Generic,
    Iterator,
    Optional,
    Tuple,
    TypeVar,
)

import pyauto   # type: ignore

from extension.domain.share import (
    AbstractMeta,
)
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
    WM_CLOSE,
    WM_SYSCOMMAND,
    SC_CLOSE,
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
    IsHungAppWindow,
    SetForegroundWindow,
    GetForegroundWindow,
    BringWindowToTop,
    GetWindow,
    IsIconic,
    IsWindow,
    ShowWindow,
    GetParent,
    WindowFromPoint,
    GetFocus,
    EnumChildWindows,
    IsWindowVisible,
    GetCursorPos,
    AttachThreadInput,
    ImmGetDefaultIMEWnd,
    PostMessage,
)


MAX_CLASS_NAME_LENGTH = 256
"""クラス名の最大長"""


T = TypeVar('T')


class AttachThreadInputError(DomainRuntimeError):
    """スレッド入力の接続に失敗したときのエラー"""

    def __init__(self, err_code: int, thread_id1: int, thread_id2: int):
        err = SystemError(err_code)
        msg = f'{err.name}: Thread({thread_id1}), Thread({thread_id2})'
        super().__init__(msg)


class SetForegroundError(DomainRuntimeError):
    """フォアグラウンド化に失敗したときのエラー"""

    def __init__(self, target: Window):
        msg = f'{repr(target)}'
        super().__init__(msg)


class WindowApiCache(Generic[T], metaclass=AbstractMeta):
    """
    ウィンドウに関する情報をAPIで取得し、キャッシュする
    APIは実行コストが大きいので、必要なときだけ実行し、それ以降はキャッシュを利用する

    @param window_id    対象ウィンドウのID
    """
    def __init__(self, window_id: WindowId):
        self._window_id = window_id
        self._value: Optional[T] = None

    @abstractmethod
    def _fetch(self) -> T:
        raise NotImplementedError

    @property
    def value(self) -> T:
        if self._value is None:
            self._value = self._fetch()
        return self._value


class ProcessInfo(WindowApiCache[Tuple['Thread', str]]):
    """
    ウィンドウのプロセス情報を取得する

    @param window_id    対象ウィンドウのID
    """
    def _fetch(self) -> Tuple['Thread', str]:
        pid = c_ulong()
        thread_id = GetWindowThreadProcessId(self._window_id.value, pointer(pid))
        thread = Thread(thread_id)
        pyauto_window = pyauto.Window.fromHWND(self._window_id.value)
        exe_name = pyauto_window.getProcessName()
        return thread, exe_name

    @property
    def thread(self) -> 'Thread':
        thread, _ = self.value
        return thread

    @property
    def exe_name(self) -> str:
        _, exe_name = self.value
        return exe_name


class ClassName(WindowApiCache[str]):
    """
    ウィンドウのクラス名を取得する

    @param window_id    対象ウィンドウのID
    """
    def _fetch(self) -> str:
        buff = create_unicode_buffer(MAX_CLASS_NAME_LENGTH + 1)
        GetClassNameW(self._window_id.value, buff, MAX_CLASS_NAME_LENGTH + 1)
        return buff.value


class WindowText(WindowApiCache[str]):
    """
    ウィンドウテキストを取得する

    @param window_id    対象ウィンドウのID
    """
    def _fetch(self) -> str:
        length = GetWindowTextLengthW(self._window_id.value)
        buff = create_unicode_buffer(length + 1)
        GetWindowTextW(self._window_id.value, buff, length + 1)
        return buff.value


class WindowWin32(Window):
    def __init__(self, window_id: WindowId):
        if not IsWindow(window_id.value):
            raise WindowNotFoundError(f'hwnd={window_id.value} is not window')
        self._window_id = window_id
        self._process_info = ProcessInfo(window_id)
        self._class_name = ClassName(window_id)
        self._window_text = WindowText(window_id)

    def activate(self) -> bool:
        platform = WindowPlatform()
        if self._is_hang():
            raise DomainRuntimeError(f'Window not responding: {repr(self)}')

        if self._is_minimized():
            platform.restore_window(self)

        current_wnd = WindowServiceWin32().from_active()
        if current_wnd == self:
            return True

        try:
            with Thread.from_current().attach(current_wnd.thread):
                with current_wnd.thread.attach(self.thread):
                    platform.set_foreground(self)
                    return True
        except AttachThreadInputError:
            pass

        platform.set_foreground(self)
        return True

    def ime_on(self) -> bool:
        ime = IME(self.window_id)
        ime.status = IME.Status.ON
        return True

    def ime_off(self) -> bool:
        ime = IME(self.window_id)
        ime.status = IME.Status.OFF
        return True

    def close(self) -> bool:
        ignores = tuple((
            WindowQuery('explorer.exe', 'SysListView32', 'FolderView'),
        ))
        if any((w.match(self) for w in ignores)):
            return False

        platform = WindowPlatform()
        garbages = tuple(itertools.chain(
            platform.find_ancestor_window(WindowQuery('explorer.exe', 'CabinetWClass'), self),
            platform.find_ancestor_window(WindowQuery('OUTLOOK.EXE', 'rctrl_renwnd32'), self),
        ))

        SendMessage(self.window_id.value, WM_CLOSE, 0, 0)

        wnd = WindowServiceWin32().from_active()
        if any((w == wnd for w in garbages)):
            SendMessage(wnd.window_id.value, WM_CLOSE, 0, 0)

        return True

    @property
    def window_id(self) -> WindowId:
        return self._window_id

    @property
    def thread(self) -> 'Thread':
        return self._process_info.thread

    @property
    def exe_name(self) -> str:
        return self._process_info.exe_name

    @property
    def window_text(self) -> str:
        return self._window_text.value

    @property
    def class_name(self) -> str:
        return self._class_name.value

    def _is_minimized(self) -> bool:
        return IsIconic(self.window_id.value)

    def _is_visible(self) -> bool:
        return IsWindowVisible(self.window_id.value)

    def _is_cloaked(self) -> bool:
        cloaked = BOOL()
        DWMWA_CLOAKED = 14
        result: HRESULT = DwmGetWindowAttribute(
            self.window_id.value, DWMWA_CLOAKED, pointer(cloaked), sizeof(cloaked))
        return bool(result) and bool(cloaked)

    def _is_uwp_frame(self) -> bool:
        """
        UWPアプリのトップレベルウィンドウであるかを判定する
        """
        return self.class_name == 'ApplicationFrameWindow'

    def _is_hang(self) -> bool:
        return IsHungAppWindow(self.window_id.value)

    def _has_ancestor(self, other: Window) -> bool:
        for w in WindowPlatform().get_ancestors_of(self):
            if w == other:
                return True
        return False

    def _owned_by(self, other: Window) -> bool:
        owner = WindowPlatform().get_owner_of(self)
        if owner is None:
            return False
        return other.window_id == owner.window_id


class WindowServiceWin32(WindowService):
    def from_id(self, window_id: WindowId) -> WindowWin32:
        wnd = WindowWin32(window_id)
        if not wnd._is_visible() or wnd._is_cloaked():
            raise WindowNotFoundError(f'hwnd={window_id.value} is not visible')
        return wnd

    def from_pointer(self) -> WindowWin32:
        platform = WindowPlatform()
        wnd = platform.get_window_on_pointer()
        orig_wnd = platform.get_origin_of(wnd)
        if orig_wnd is None or orig_wnd._is_uwp_frame():
            return wnd
        return orig_wnd

    def from_active(self) -> WindowWin32:
        wnd = WindowPlatform().get_active_window()
        if wnd is None:
            raise WindowNotFoundError('Active window not found') from None
        return wnd

    def from_query(self, query: WindowQuery) -> WindowWin32:
        return WindowPlatform().find_window(query, None)


class WindowPlatform:
    """
    ウィンドウに関するAPI呼び出しを代行する
    """

    def get_active_window(self) -> Optional[WindowWin32]:
        """
        アクティブウィンドウを取得する

        @return         アクティブウィンドウ（無ければNone）
        """
        platform = WindowPlatform()
        foreground = platform.get_foreground_window()
        if foreground is None:
            return None
            # raise WindowNotFoundError('Foreground window not found')
        focus = platform.get_focus_window(foreground)
        if focus is None:
            return foreground
        return focus

    def get_foreground_window(self) -> Optional[WindowWin32]:
        """
        フォアグラウンドウィンドウを取得する

        @return         フォアグラウンドウィンドウ（無ければNone）
        """
        hwnd = GetForegroundWindow()
        try:
            return WindowWin32(WindowId(hwnd))
        except DomainValueError:
            return None

    def get_focus_window(self, fore_wnd: WindowWin32) -> Optional[WindowWin32]:
        """
        キーボードフォーカスがあるウィンドウを取得する

        @param fore_wnd フォアグラウンドウィンドウ
        @return         キーボードフォーカスがあるウィンドウ（無ければNone）
        """
        try:
            with fore_wnd.thread.attach(Thread.from_current()):
                focus_window_id = WindowId(GetFocus())
        except (AttachThreadInputError, DomainValueError):
            return None
        return WindowWin32(focus_window_id)

    def get_window_on_pointer(self) -> WindowWin32:
        """
        マウスカーソル下のウィンドウを取得する

        @return         マウスカーソル下のウィンドウ
        """
        point = POINT()
        GetCursorPos(pointer(point))
        hwnd = WindowFromPoint(point)
        window_id = WindowId(hwnd)
        return WindowWin32(window_id)

    def find_window(
        self,
        query: WindowQuery,
        parent: Optional[Window],
    ) -> WindowWin32:
        """
        queryに合致する最初のウィンドウを取得する
        parentを指定すると、子ウィンドウのみを検索対象とする

        @param query    検索条件
        @param parent   親ウィンドウ
        @return         queryに合致した最初のウィンドウ
        """
        founds = []

        def _callback(hwnd: int, lParam: LPARAM) -> bool:
            try:
                wnd = WindowWin32(WindowId(hwnd))
            except (DomainValueError, WindowNotFoundError):
                return True
            if wnd._is_uwp_frame():
                # UWPアプリはトップレベルウィンドウが本体ではないので、子ウィンドウを検索する
                return EnumChildWindows(hwnd, WNDENUMPROC(_callback), 0)
            elif query.match(wnd):
                founds.append(wnd)
                return False
            return True

        hwnd = parent.window_id.value if parent is not None else 0
        EnumChildWindows(hwnd, WNDENUMPROC(_callback), 0)

        if len(founds) > 0:
            return founds[0]
        raise WindowNotFoundError(f'No window matched: {repr(query)}')

    def set_foreground(self, aim: Window) -> None:
        """
        aimをフォアグラウンドウィンドウにすることを試みる
        結果がaim以外でも、以下は期待通りなので許容する
        - aimの所有者ウィンドウ
        - aimの祖先ウィンドウ
        参考: AutoHotkey/source/window.cpp: AttemptSetForeground

        @param aim  フォアグラウンドに設定するウィンドウ
        """
        if not SetForegroundWindow(aim.window_id.value):
            raise SetForegroundError(aim)
        new = None
        for i in range(0, 3):
            new = self.get_active_window()
            if new is None:
                continue
            if new == aim or new._owned_by(aim) or new._has_ancestor(aim):
                BringWindowToTop(new.window_id.value)
                return
        raise DomainRuntimeError(f'Unexpected foreground: expected to {aim}, but {new}')

    def get_parent_of(self, wnd: Window) -> Optional[WindowWin32]:
        """
        wndの親ウィンドウを取得する

        @param wnd  子ウィンドウ
        @return     親ウィンドウ（無ければNone）
        """
        hwnd = GetParent(wnd.window_id.value)
        try:
            window_id = WindowId(hwnd)
        except DomainValueError:
            return None
        return WindowWin32(window_id)

    def get_ancestors_of(self, wnd: Window) -> Iterator[WindowWin32]:
        """
        wndの祖先ウィンドウを若い順に取得する

        @param wnd  子ウィンドウ
        @return     祖先ウィンドウを若い順に返すイテレーター
        """
        parent = wnd
        while True:
            parent = self.get_parent_of(parent)
            if parent is None:
                break
            yield parent

    def find_ancestor_window(
        self,
        query: WindowQuery,
        child: Window,
    ) -> Iterator[WindowWin32]:
        """
        queryに合致する最初の祖先ウィンドウを取得する

        @param query    検索条件
        @param child    子ウィンドウ
        @return         queryに合致した最初のウィンドウ
        """
        for w in self.get_ancestors_of(child):
            if query.match(w):
                yield w

    def get_origin_of(self, wnd: Window) -> Optional[WindowWin32]:
        """
        wndの始祖ウィンドウを取得する

        @param wnd  子ウィンドウ
        @return     始祖ウィンドウ（無ければNone）
        """
        ancestor_wnds = deque(self.get_ancestors_of(wnd), maxlen=1)
        try:
            return ancestor_wnds.pop()
        except IndexError:
            return None

    def get_owner_of(self, wnd: Window) -> Optional[WindowWin32]:
        """
        wndの所有者ウィンドウを取得する

        @param wnd  被所有ウィンドウ
        @return     所有者ウィンドウ（無ければNone）
        """
        hwnd = GetWindow(wnd.window_id.value, GetWindowCmd.GW_OWNER)
        try:
            window_id = WindowId(hwnd)
        except DomainValueError:
            return None
        return WindowWin32(window_id)

    def restore_window(self, wnd: Window) -> None:
        """
        最小化、最大化、または配置されている場合は、元のサイズと位置に復元する

        @param wnd  対象ウィンドウ
        """
        ShowWindow(wnd.window_id.value, ShowWindowCmd.SW_RESTORE)


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
                    raise AttachThreadInputError(
                        GetLastError(), self._self_thread_id, self._other_thread_id)

        def __exit__(self, exc_type, exc_value, traceback):
            if self._attached:
                AttachThreadInput(self._self_thread_id, self._other_thread_id, False)
