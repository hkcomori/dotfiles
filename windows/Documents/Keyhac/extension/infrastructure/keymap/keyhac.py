from extension.domain.keymap import (
    Keymap,
    KeymapRegistry,
)
from extension.domain.keymap import KeyInput
from extension.domain.action import (
    Action,
)
from extension.domain.window import WindowQuery
from extension.infrastructure.keyhac import (
    KeymapKeyhac,
)


class KeymapRepositoryKeyhac(KeymapRegistry):
    def __init__(self, query: WindowQuery, keymap: KeymapKeyhac):
        self._window_keymap = keymap.defineWindowKeymap(
            query.exe_name, query.class_name, query.window_text)
        super().__init__()

    def assign(self, *keymap: Keymap):
        for k in keymap:
            self._window_keymap[k.keys.value] = k.action.perform

    def registerOnActive(self, action: Action):
        self._window_keymap.applying_func = action.perform


class KeyConditionKeyhac():
    def __init__(self, key: KeyInput):
        self._key = key

    @property
    def value(self) -> str:
        return ''

    MOD_KEY_DICTT = {
        'Apps': 'U0',
        'Kana': 'U1',
        'Henkan': 'U2',
        'Muhenkan': 'U3',
    }

    MAIN_KEY_DICT = {
        'Apps': 'U0',
        'Kana': 'U1',
        'Henkan': 'U2',
        'Muhenkan': 'U3',
    }
