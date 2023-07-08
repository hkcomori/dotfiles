import keyhac

from ..keymap_helper import (    # noqa: F401
    KeymapDefinition,
    KeyCondition,
)
from . import (          # noqa: F401
    def_global,
)


def init(keymap: keyhac.Keymap):
    keymap.defineModifier(KeyCondition("Apps").to_keyhac(), "User0")
    keymap.defineModifier(KeyCondition("Kana").to_keyhac(), "User1")
    keymap.defineModifier(KeyCondition("Henkan").to_keyhac(), "User2")
    keymap.defineModifier(KeyCondition("Muhenkan").to_keyhac(), "User3")

    for m in (
        def_global,
    ):
        keymap_definition = KeymapDefinition(keymap.defineWindowKeymap(
            m.exe_name,
            m.class_name,
            m.window_text,
            m.check_func,
        ))
        keymap_definition.update(m.window_keymap)
