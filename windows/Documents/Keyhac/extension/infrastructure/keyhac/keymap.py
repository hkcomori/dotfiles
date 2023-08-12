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

    def __setitem__(self, key: str, action: Action):
        input = Input(key)
        self._window_keymap[input.value] = action.value

    @property
    def applying_func(self):
        return self._window_keymap.applying_func

    @applying_func.setter
    def applying_func(self, action: Action):
        self._window_keymap.applying_func = action.value


class KeymapServiceKeyhac(KeymapService):
    @inject
    def __init__(self, keymap: KeymapKeyhac):
        self._keymap = keymap

    @property
    def user_modifier0(self) -> str:
        raise NotImplementedError

    @user_modifier0.setter
    def user_modifier0(self, key: str) -> None:
        input = Input(key)
        self._keymap.defineModifier(input.value, 'U0')

    @property
    def user_modifier1(self) -> str:
        raise NotImplementedError

    @user_modifier1.setter
    def user_modifier1(self, key: str) -> None:
        input = Input(key)
        self._keymap.defineModifier(input.value, 'U1')

    @property
    def user_modifier2(self) -> str:
        raise NotImplementedError

    @user_modifier2.setter
    def user_modifier2(self, key: str) -> None:
        input = Input(key)
        self._keymap.defineModifier(input.value, 'U2')

    @property
    def user_modifier3(self) -> str:
        raise NotImplementedError

    @user_modifier3.setter
    def user_modifier3(self, key: str) -> None:
        input = Input(key)
        self._keymap.defineModifier(input.value, 'U3')

    def from_query(self, query: WindowQuery) -> KeymapRegistry:
        window_keymap = self._keymap.defineWindowKeymap(
            exe_name=query.exe_name,
            class_name=query.class_name,
            window_text=query.window_text,
        )
        return KeymapRegistryKeyhac(self._keymap, window_keymap)
