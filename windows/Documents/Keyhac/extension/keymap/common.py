from collections.abc import Callable
from functools import singledispatch
from typing import (
    Dict,
    Tuple,
    Union,
)
import typing

import pyauto

from ..singleton import MetaSingleton


CmdFunc = typing.Callable[[], None]
KeymapValue = Union[str, Tuple[str, ...], CmdFunc]
WindowKeymap = Dict[str, KeymapValue]
CheckFunc = typing.Callable[[pyauto.Window], bool]


class KeymapDefinition():
    def __init__(self, keymap: WindowKeymap):
        self.__keymap = keymap

    def __setitem__(self, keys: str, value: KeymapValue):
        if keys.startswith('*-'):
            explicit_keys = keys[2:]
            for m in ['C-', 'A-', 'S-', 'W-', 'U0-', 'U1-', 'U2-', 'U3-']:
                self[m + explicit_keys] = value
        else:
            self.__keymap[KeyCondition(keys).to_keyhac()] = _convert(value)

    def __getitem__(self, keys: str) -> KeymapValue:
        return self.__keymap[KeyCondition(keys).to_keyhac()]

    def update(self, keymap: WindowKeymap):
        for k, v in keymap.items():
            self[k] = v


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
def _convert_func(value: Callable) -> Callable:
    return value
