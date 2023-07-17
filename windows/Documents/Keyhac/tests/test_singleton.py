import pytest   # noqa: F401

import tests.mock   # noqa: F401

from extension.singleton import (    # noqa: E402
    MetaSingleton,
)


class Hoge(metaclass=MetaSingleton):
    def __init__(self, hoge):
        self.__hoge = hoge

    @property
    def hoge(self):
        return self.__hoge


class Fuga(metaclass=MetaSingleton):
    def __init__(self, fuga):
        self.__fuga = fuga

    @property
    def fuga(self):
        return self.__fuga


def test_instance_identification():
    hoge11 = Hoge(1)
    hoge12 = Hoge(1)
    hoge21 = Hoge(2)
    fuga11 = Fuga(1)
    assert hoge11 is hoge12
    assert hoge11 is not hoge21
    assert hoge11 is not fuga11
    assert id(hoge11) == id(hoge12)
    assert id(hoge11) != id(hoge21)
    assert id(hoge11) != id(fuga11)
