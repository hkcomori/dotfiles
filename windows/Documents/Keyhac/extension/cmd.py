from logging import getLogger
import os

import pyauto
import keyhac

from .task import background_task
from .window import (
    WindowNotFoundError,
    Cursor,
    Window,
)


logger = getLogger(__name__)


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
    logger.debug("launch_obsidian")
    userprofile = os.getenv("USERPROFILE")
    obsidian_path = f'{userprofile}/AppData/Local/Obsidian/Obsidian.exe'
    launch_or_activate(obsidian_path)


@background_task
def activate_window_under_mouse_pointer():
    """マウスカーソル座標のウィンドウをアクティブにする"""
    cursor = Cursor()
    target_window = Window.from_point(cursor.point)
    target_window.set_foreground()


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
