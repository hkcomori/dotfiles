from typing import (
    Callable,
    Dict,
)

from extension.domain.keymap import (
    Keymap,
    KeymapRegistry,
)
from extension.domain.action import (
    Action,
)
from extension.domain.window import WindowQuery


class KeymapRepositoryMock(KeymapRegistry):

    def __init__(self, query: WindowQuery):
        self.query = query
        self.window_keymap: Dict[str, Callable[[], None]] = dict()
        self.applying_func = None

    def assign(self, *keymap: Keymap):
        for k in keymap:
            self.window_keymap[k.key.value] = k.action.perform

    def registerOnActive(self, action: Action):
        self.applying_func = action.perform
