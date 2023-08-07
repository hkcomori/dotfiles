from extension.vendor.injector import (
    inject,
)

from extension.domain.keymap import (
    KeymapRegistry,
)
from extension.domain.keymap import Action
# from extension.domain.window import WindowQuery
# from .share import (
#     KeymapKeyhac,
# )
from extension.keyhac_helper import (
    WindowKeymapEx,
)


class KeymapRegistryKeyhac(KeymapRegistry):
    @inject
    def __init__(self, window_keymap: WindowKeymapEx) -> None:
        self._window_keymap = window_keymap

    def __setitem__(self, keys: str, action: Action):
        self._window_keymap[keys] = action.perform

    @property
    def applying_func(self):
        return self._window_keymap.applying_func

    @applying_func.setter
    def applying_func(self, action: Action):
        self._window_keymap.applying_func = action.perform
