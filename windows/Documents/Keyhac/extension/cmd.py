from logging import getLogger
import os

from keyhac import *    # noqa: F403


logger = getLogger(__name__)


def launch_calc():
    logger.debug("launch_calc")
    shellExecute(None, "calc.exe")    # noqa: F405


def launch_obsidian():
    logger.debug("launch_obsidian")
    obsidian = f'{os.getenv("USERPROFILE")}/AppData/Local/Obsidian/Obsidian.exe'
    shellExecute(None, obsidian)    # noqa: F405


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
