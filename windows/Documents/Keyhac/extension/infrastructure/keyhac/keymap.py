from extension.vendor.injector import (
    inject,
)

from extension.domain.keymap import (
    KeymapRegistry,
    KeymapService,
)
from extension.domain.action import (
    Action,
)
from extension.domain.input import (
    Input,
)
from extension.domain.window import WindowQuery
from .share import (
    KeymapKeyhac,
    WindowKeymapKeyhac,
)


class KeymapRegistryKeyhac(KeymapRegistry):
    @inject
    def __init__(self, keymap: KeymapKeyhac, window_keymap: WindowKeymapKeyhac) -> None:
        self._keymap = keymap
        self._window_keymap = window_keymap

    def __setitem__(self, input: Input, action: Action):
        key = input.value
        self._window_keymap[key] = action.perform

    @property
    def applying_func(self):
        return self._window_keymap.applying_func

    @applying_func.setter
    def applying_func(self, action: Action):
        self._window_keymap.applying_func = action.perform


class KeymapServiceKeyhac(KeymapService):
    @inject
    def __init__(self, keymap: KeymapKeyhac):
        self._keymap = keymap

    def from_query(self, query: WindowQuery) -> KeymapRegistry:
        window_keymap = self._keymap.defineWindowKeymap(
            exe_name=query.exe_name,
            class_name=query.class_name,
            window_text=query.window_text,
        )
        return KeymapRegistryKeyhac(self._keymap, window_keymap)
