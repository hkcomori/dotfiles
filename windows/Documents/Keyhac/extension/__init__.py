from . import (
    keymap_global,
)
from .key import KeymapConverter


def init(keymap):
    keymap.defineModifier(KeymapConverter.convert("Apps"), "User0")
    keymap.defineModifier(KeymapConverter.convert("Kana"), "User1")
    keymap.defineModifier(KeymapConverter.convert("Henkan"), "User2")
    keymap.defineModifier(KeymapConverter.convert("Muhenkan"), "User3")

    for m in (
        keymap_global,
    ):
        target_window = m.TARGET_WINDOW
        window_keymap = keymap.defineWindowKeymap(**target_window)
        m.configure_keymap(KeymapConverter(window_keymap))
