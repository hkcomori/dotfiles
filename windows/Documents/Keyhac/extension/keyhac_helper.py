import collections.abc
from functools import singledispatch
from typing import (
    Callable,
    Optional,
    Sequence,
)

import keyhac_hook

from .singleton import MetaSingleton
from .keyhac_interface import (
    KeymapValue,
    KeymapInterface,
    CheckFunc,
    WindowKeymapInterface,
)


def nop():
    """Function that does nothing to assign to a key"""
    pass


class KeymapEx(KeymapInterface):
    def __init__(self, keymap: KeymapInterface):
        self._keymap = keymap

        self.__add_hook_mousedown()

    def __add_hook_mousedown(self):
        """Add hook to detect change window focus by mouse buttons"""
        orig_hook = keyhac_hook.hook.mousedown

        def _new_hook(x, y, vk):
            orig_hook(x, y, vk)
            self.setTimer(self._updateFocusWindow, 0)

        keyhac_hook.hook.mousedown = _new_hook

    def defineModifier(self, src_key: str, dest_key: str):
        converted_src_key = KeyCondition(src_key).to_keyhac()
        return self._keymap.defineModifier(converted_src_key, dest_key)

    def defineWindowKeymap(
        self,
        exe_name: Optional[str] = None,
        class_name: Optional[str] = None,
        window_text: Optional[str] = None,
        check_func: Optional[CheckFunc] = None,
    ):
        return WindowKeymapEx(self._keymap.defineWindowKeymap(
            exe_name, class_name, window_text, check_func))

    def setTimer(self, func: Callable[[], None], sec: int):
        self._keymap.setTimer(func, sec)

    def _updateFocusWindow(self):
        self._keymap._updateFocusWindow()

    def _onKeyDown(self, vk: int):
        self._keymap._onKeyDown(vk)

    def beginInput(self) -> None:
        self._keymap.beginInput()

    def setInput_FromString(self, s: str) -> None:
        self._keymap.setInput_FromString(s)

    def endInput(self) -> None:
        self._keymap.endInput()

    def sendInput_FromString(self, keys: Sequence[str]) -> Callable[[], None]:
        def _sendInput_FromString():
            self.beginInput()
            for key in keys:
                self.setInput_FromString(key)
            self.endInput()
        return _sendInput_FromString


class WindowKeymapEx(WindowKeymapInterface):
    def __init__(self, keymap: WindowKeymapInterface):
        self.__keymap = keymap

    def __setitem__(self, keys: str, value: Optional[KeymapValue]):
        if keys.startswith('*-'):
            explicit_keys = keys[2:]
            for m in ['C-', 'A-', 'S-', 'W-', 'U0-', 'U1-', 'U2-', 'U3-']:
                self[m + explicit_keys] = value
        else:
            converted_value = _convert(value) if value is not None else nop
            self.__keymap[KeyCondition(keys).to_keyhac()] = converted_value

    def __getitem__(self, keys: str) -> KeymapValue:
        return self.__keymap[KeyCondition(keys).to_keyhac()]

    @property
    def applying_func(self):
        return self.__keymap.applying_func

    @applying_func.setter
    def applying_func(self, callback: Callable[[], None]):
        self.__keymap.applying_func = callback


class WindowKeymapGroup(WindowKeymapInterface):
    """Groups WindowKeymapInterface objects"""
    def __init__(self, *keymaps: WindowKeymapInterface):
        self.__keymaps = keymaps

    def __setitem__(self, keys: str, value: Optional[KeymapValue]):
        for keymap in self.__keymaps:
            keymap[keys] = value

    def __getitem__(self, keys: str) -> KeymapValue:
        values = [keymap[keys] for keymap in self.__keymaps]
        if values.count(values[0]) != len(values):
            raise ValueError('Grouped value mismatch')
        return values[0]

    @property
    def applying_func(self):
        funcs = [keymap.applying_func for keymap in self.__keymaps]
        if funcs.count(funcs[0]) != len(funcs):
            raise ValueError('Grouped applying_func mismatch')
        return funcs[0]

    @applying_func.setter
    def applying_func(self, callback: Callable[[], None]):
        for keymap in self.__keymaps:
            keymap.applying_func = callback


class KeyCondition(metaclass=MetaSingleton):
    def __init__(self, keys: str):
        orig_keys = keys.split('-')
        uniq_mods = list(dict.fromkeys(orig_keys[:-1]))
        self.__keys = [Key(k) for k in (*uniq_mods, orig_keys[-1])]

    def to_keyhac(self) -> str:
        return '-'.join(k.to_keyhac() for k in self.__keys)


class Key(metaclass=MetaSingleton):
    def __init__(self, key: str):
        self.__key = key

    def to_keyhac(self) -> str:
        return self.__class__.__lookup.get(self.__key, self.__key)

    __lookup = {
        'BackSpace':            'Back',              # noqa: E241
        'XButton1':             f"({str(0x05)})",    # noqa: E241
        'XButton2':             f"({str(0x06)})",    # noqa: E241
        'Kana':                 f"({str(0x15)})",    # noqa: E241
        'Henkan':               f"({str(0x1C)})",    # noqa: E241
        'Muhenkan':             f"({str(0x1D)})",    # noqa: E241
        'F13':                  f"({str(0x7C)})",    # noqa: E241
        'F14':                  f"({str(0x7D)})",    # noqa: E241
        'F15':                  f"({str(0x7E)})",    # noqa: E241
        'F16':                  f"({str(0x7F)})",    # noqa: E241
        'F17':                  f"({str(0x80)})",    # noqa: E241
        'F18':                  f"({str(0x81)})",    # noqa: E241
        'F19':                  f"({str(0x82)})",    # noqa: E241
        'F20':                  f"({str(0x83)})",    # noqa: E241
        'F21':                  f"({str(0x84)})",    # noqa: E241
        'F22':                  f"({str(0x85)})",    # noqa: E241
        'F23':                  f"({str(0x86)})",    # noqa: E241
        'F24':                  f"({str(0x87)})",    # noqa: E241
        'WheelUp':              f"({str(0x9F)})",    # noqa: E241
        'WheelDown':            f"({str(0x9E)})",    # noqa: E241
        'BrowserBack':          f"({str(0xA6)})",    # noqa: E241
        'BrowserForward':       f"({str(0xA7)})",    # noqa: E241
        'BrowserReload':        f"({str(0xA8)})",    # noqa: E241
        'BrowserStop':          f"({str(0xA9)})",    # noqa: E241
        'BrowserSearch':        f"({str(0xAA)})",    # noqa: E241
        'BrowserFavorite':      f"({str(0xAB)})",    # noqa: E241
        'BrowserHome':          f"({str(0xAC)})",    # noqa: E241
        'VolumeMute':           f"({str(0xAD)})",    # noqa: E241
        'VolumeDown':           f"({str(0xAE)})",    # noqa: E241
        'VolumeUp':             f"({str(0xAF)})",    # noqa: E241
        'MediaNextTrack':       f"({str(0xB0)})",    # noqa: E241
        'MediaPrevTrack':       f"({str(0xB1)})",    # noqa: E241
        'MediaStop':            f"({str(0xB2)})",    # noqa: E241
        'MediaPlay':            f"({str(0xB3)})",    # noqa: E241
        'LaunchMail':           f"({str(0xB4)})",    # noqa: E241
        'LaunchMediaSelect':    f"({str(0xB5)})",    # noqa: E241
        'LaunchApp1':           f"({str(0xB6)})",    # noqa: E241
        'LaunchApp2':           f"({str(0xB7)})",    # noqa: E241
    }


@singledispatch
def _convert(value: KeymapValue) -> KeymapValue:
    raise NotImplementedError


@_convert.register
def _convert_str(value: str) -> str:
    return KeyCondition(value).to_keyhac()


@_convert.register
def _convert_tuple(value: tuple) -> tuple:
    return tuple(KeyCondition(v).to_keyhac() for v in value)


@_convert.register
def _convert_func(value: collections.abc.Callable) -> collections.abc.Callable:
    return value
