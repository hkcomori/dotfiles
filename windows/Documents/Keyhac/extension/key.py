from collections.abc import Callable
from functools import singledispatch
from typing import (
    Dict,
    Tuple,
    Union,
)
import typing


KeymapCommand = Union[str, Tuple[str, ...], typing.Callable[[], None]]
WindowKeymap = Dict[str, KeymapCommand]


class KeymapConverter:
    def __init__(self, keymap: WindowKeymap):
        self.keymap = keymap

    def __setitem__(self, key: str, value):
        conv_key = _convert_key(key)
        self.keymap[conv_key] = self.__class__.convert(value)

    def __getitem__(self, key: str) -> KeymapCommand:
        return self.keymap[_convert_key(key)]

    # When using `@singledispatchmethod` and `@classmethod` together,
    # it can cause an error in the `functools` module.
    # This issue has been resolved in Python 3.9.
    # https://bugs.python.org/issue39679
    @classmethod
    def convert(cls, value: KeymapCommand) -> KeymapCommand:
        return _convert(value)


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
def _convert(value: KeymapCommand) -> KeymapCommand:
    raise NotImplementedError


@_convert.register
def _convert_str(value: str) -> str:
    return _convert_key(value)


@_convert.register
def _convert_tuple(value: tuple) -> tuple:
    return tuple(_convert_key(v) for v in value)


@_convert.register
def _convert_func(value: Callable) -> Callable:
    return value


def _convert_key(key: str) -> str:
    src_keys: list = key.split('-')
    dest_keys = tuple(__lookup.get(k, k) for k in src_keys)
    return '-'.join(dest_keys)
