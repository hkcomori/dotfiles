from logging import getLogger
import os

import pyauto
from keyhac import (
    shellExecute,
)

from .task import background_task
from .window import (
    Cursor,
    Window,
)


logger = getLogger(__name__)


def launch_calc():
    """電卓を起動する"""
    logger.debug("launch_calc")
    calc_path = "calc.exe"
    shellExecute(None, calc_path)


def launch_obsidian():
    """Obsidianを起動する"""
    logger.debug("launch_obsidian")
    userprofile = os.getenv("USERPROFILE")
    obsidian_path = f'{userprofile}/AppData/Local/Obsidian/Obsidian.exe'
    shellExecute(None, obsidian_path)


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
