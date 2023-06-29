from .keymap import (
    KeymapDefinition,
    KeyCondition,
)


def init(keymap):
    keymap.defineModifier(KeyCondition("Apps").to_keyhac(), "User0")
    keymap.defineModifier(KeyCondition("Kana").to_keyhac(), "User1")
    keymap.defineModifier(KeyCondition("Henkan").to_keyhac(), "User2")
    keymap.defineModifier(KeyCondition("Muhenkan").to_keyhac(), "User3")

    for keymap_definition in KeymapDefinition.all():
        window_keymap = keymap.defineWindowKeymap(
            keymap_definition.exe_name,
            keymap_definition.class_name,
            keymap_definition.window_text,
            keymap_definition.check_func,
        )
        for k, v in keymap_definition.items():
            window_keymap[k] = v
