from collections.abc import Callable
from functools import singledispatch
from typing import (
    Dict,
    Tuple,
    Union,
    Iterable,
    List,
)
import typing

import pyauto


CmdFunc = typing.Callable[[], None]
KeymapValue = Union[str, Tuple[str, ...], CmdFunc]
WindowKeymap = Dict[str, KeymapValue]
CheckFunc = typing.Callable[[pyauto.Window], bool]


class KeymapDefinition(dict):
    __instances: Dict[
        Tuple[
            Union[str, None],
            Union[str, None],
            Union[str, None],
            Union[CheckFunc, None]
        ],
        'KeymapDefinition'
    ] = dict()

    def __init__(
        self,
        exe_name: Union[str, None] = None,
        class_name: Union[str, None] = None,
        window_text: Union[str, None] = None,
        check_func: Union[CheckFunc, None] = None,
    ):
        """
        キーマップ適用対象ウィンドウの指定

        Args:
            - exe_name: 実行ファイル名 (*1)(*2)
            - class_name: クラス名 (*1)(*2)
            - window_text: タイトル文字列 (*1)(*2)
            - check_func: 識別関数 (*2)

        Description:
            - (*1): ワイルドカード ( * ? ) が使えます。
            - (*2): None の場合は、その条件を無視します。
        """
        super().__init__()
        self.exe_name = exe_name
        self.class_name = class_name
        self.window_text = window_text
        self.check_func = check_func

    def __new__(
        cls,
        exe_name: Union[str, None] = None,
        class_name: Union[str, None] = None,
        window_text: Union[str, None] = None,
        check_func: Union[CheckFunc, None] = None,
    ) -> 'KeymapDefinition':
        dict_key = (exe_name, class_name, window_text, check_func)
        if dict_key not in cls.__instances:
            cls.__instances[dict_key] = super().__new__(cls, *dict_key)
        return cls.__instances[dict_key]

    def __setitem__(self, keys: str, value: KeymapValue):
        super().__setitem__(KeyCondition(keys).to_keyhac(), _convert(value))

    def __getitem__(self, keys: str) -> KeymapValue:
        return super().__getitem__(KeyCondition(keys).to_keyhac())

    @classmethod
    def all(cls) -> Iterable['KeymapDefinition']:
        return (i for i in cls.__instances.values())


class KeyCondition:
    def __init__(self, keys: str):
        self.keys = [Key(k) for k in keys.split('-')]

    def to_keyhac(self) -> str:
        return '-'.join(k.to_keyhac() for k in self.keys)


class Key:
    def __init__(self, key: str):
        self._user = key

    def to_keyhac(self) -> str:
        return self.__class__.__lookup.get(self._user, self._user)

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
