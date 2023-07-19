from ..keyhac_helper import KeymapEx
from . import (
    def_global,
    def_windows,
    def_browser,
    def_vscode,
    def_obsidian,
    def_office,
    def_spark,
    def_captureontouch,
)


def init(keymap: KeymapEx):
    keymap.defineModifier("Apps", "User0")
    keymap.defineModifier("Kana", "User1")
    keymap.defineModifier("Henkan", "User2")
    keymap.defineModifier("Muhenkan", "User3")

    for m in (
        def_global,
        def_windows,
        def_browser,
        def_vscode,
        def_obsidian,
        def_office,
        def_spark,
        def_captureontouch,
    ):
        m.init(keymap)
