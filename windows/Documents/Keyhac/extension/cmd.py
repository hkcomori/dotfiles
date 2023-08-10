from logging import getLogger
import os
from typing import (
    List,
    Optional,
    Union,
)

from .keyhac_interface import (
    CmdFunc,
)
from .keyhac_helper import KeymapEx
from .window import (
    WindowNotFoundError,
    Cursor,
    Window,
)
from .infrastructure.win32.desktop import (
    DesktopServiceWin32,
)


logger = getLogger(__name__)


OBSIDIAN_PATH = f'{os.getenv("USERPROFILE")}/AppData/Local/Obsidian/Obsidian.exe'
DOCUMENTS_PATH = os.popen(
    'powershell.exe -Command "([Environment])::GetFolderPath("""MyDocuments""")"'
).read().rstrip('\n').replace(os.sep, '/')


keymap: KeymapEx = None


def init(_keymap: KeymapEx):
    global keymap
    keymap = _keymap


def open(
    file: str,
    param: Optional[str] = None,
    directory: Optional[str] = None,
    swmode: Optional[str] = 'normal',
):
    """ファイルを開く、またはプログラムを起動する"""
    verb = 'open'
    keymap.ShellExecuteCommand(verb, file, param, directory, swmode)()


def make(*args: Union[str, CmdFunc]) -> CmdFunc:
    """キー入力、または関数を順番に実行する関数を生成する"""
    func_seq: List[CmdFunc] = []
    input_seq: List[str] = []
    for i in args:
        if isinstance(i, str):
            input_seq.append(i)
        else:
            if len(input_seq) > 0:
                send_func = keymap.InputKeyCommand(*input_seq)
                func_seq.append(send_func)
                input_seq = []
            func_seq.append(i)

    def _new_func():
        for i in func_seq:
            i()

    return _new_func


def launch_or_activate(process_path: str, **kwargs: str):
    """kwargsに合致するウィンドウがないときにprocess_pathを起動する"""
    process_name = os.path.basename(process_path)
    find_kwargs = kwargs if kwargs else {'process_name': process_name}
    try:
        window = Window.from_find(**find_kwargs)
    except WindowNotFoundError:
        open(process_path)
    else:
        window.set_foreground()


def launch_calc():
    """電卓を起動またはアクティブ化する"""
    logger.debug("launch_calc")
    calc_path = "calc.exe"
    launch_or_activate(
        calc_path,
        process_name='ApplicationFrameHost.exe',
        title='電卓'
    )


def launch_obsidian():
    """Obsidianを起動またはアクティブ化する"""
    logger.debug(f'launch_obsidian: {OBSIDIAN_PATH}')
    launch_or_activate(OBSIDIAN_PATH)


def open_documents():
    """Documentsフォルダを開く"""
    logger.debug(f'open_documents: {DOCUMENTS_PATH}')
    open(DOCUMENTS_PATH)


def open_onedrive():
    """OneDriveフォルダを開く"""
    logger.debug("open_onedrive")
    onedrive_path = os.getenv("OneDrive")
    if onedrive_path is None:
        raise RuntimeError('Env:OneDrive not found')
    open(onedrive_path)


def toggle_always_on_top():
    """アクティブウィンドウを常に最前面に配置する設定をトグルする"""
    pass


def activate_window_under_mouse_pointer():
    """マウスカーソル座標のウィンドウをアクティブにする"""
    cursor = Cursor()
    target_window = Window.from_point(cursor.point)
    target_window.set_foreground()


def send_under_mouse_pointer(*keys: str):
    """マウスカーソル座標のウィンドウにキー入力する"""
    _send_input = keymap.InputKeyCommand(*keys)

    def _send_under_mouse_pointer():
        cursor = Cursor()
        target_window = Window.from_point(cursor.point)
        target_window.set_foreground()
        _send_input()
    return _send_under_mouse_pointer


def close_window_under_mouse_pointer():
    """マウスカーソル座標のウィンドウを閉じる"""
    cursor = Cursor()
    target_window = Window.from_point(cursor.point)
    target_window.set_foreground()
    target_window.close()


def wheel_right():
    logger.debug("wheel_right")


def wheel_left():
    logger.debug("wheel_left")


def ime_on():
    wnd = Window.from_focus()
    wnd.ime_on()


def ime_off():
    wnd = Window.from_focus()
    wnd.ime_off()


def ime_toggle():
    logger.debug("ime_toggle")


def turn_off_monitor():
    desktop = DesktopServiceWin32().from_active()
    desktop.lock_on()
