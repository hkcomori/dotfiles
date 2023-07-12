from ..keymap_helper import (
    KeymapEx,
)
from . import (
    def_global,
)


def init(keymap: KeymapEx):
    keymap.defineModifier("Apps", "User0")
    keymap.defineModifier("Kana", "User1")
    keymap.defineModifier("Henkan", "User2")
    keymap.defineModifier("Muhenkan", "User3")

    for m in (
        def_global,
    ):
        m.init(keymap)
