from logging import getLogger
import os

import pyauto
import keyhac

from .task import background_task
from .keyhac_helper import KeymapEx
from .window import (
    WindowNotFoundError,
    Cursor,
    Window,
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


@background_task
def shellExecute(*args, **kwargs):
    return keyhac.shellExecute(*args, **kwargs)


def launch_or_activate(process_path: str, **kwargs: str):
    """kwargsに合致するウィンドウがないときにprocess_pathを起動する"""
    process_name = os.path.basename(process_path)
    find_kwargs = kwargs if kwargs else {'process_name': process_name}
    try:
        window = Window.from_find(**find_kwargs)
    except WindowNotFoundError:
        shellExecute(None, process_path)
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
    shellExecute(None, DOCUMENTS_PATH)


def open_onedrive():
    """OneDriveフォルダを開く"""
    logger.debug("open_onedrive")
    onedrive_path = os.getenv("OneDrive")
    shellExecute(None, onedrive_path)


def toggle_always_on_top():
    """アクティブウィンドウを常に最前面に配置する設定をトグルする"""
    pass


@background_task
def activate_window_under_mouse_pointer():
    """マウスカーソル座標のウィンドウをアクティブにする"""
    cursor = Cursor()
    target_window = Window.from_point(cursor.point)
    target_window.set_foreground()


def send_under_mouse_pointer(*keys: str):
    """マウスカーソル座標のウィンドウにキー入力する"""
    _send_input = keymap.sendInput_FromString(keys)

    @background_task
    def _send_under_mouse_pointer():
        cursor = Cursor()
        target_window = Window.from_point(cursor.point)
        target_window.set_foreground()
        _send_input()
    return _send_under_mouse_pointer


def wheel_right():
    logger.debug("wheel_right")


def wheel_left():
    logger.debug("wheel_left")


def ime_on():
    logger.debug("ime_on")


def ime_off():
    logger.debug("ime_off")


def ime_toggle():
    logger.debug("ime_toggle")
