import pytest   # noqa: F401

import tests.mock   # noqa: F401

from extension.singleton import (    # noqa: E402
    MetaSingleton,
)


class MyClass(metaclass=MetaSingleton):
    def __init__(self, hoge):
        self.__hoge = hoge

    @property
    def hoge(self):
        return self.__hoge


def test_instance_identification():
    obj11 = MyClass(1)
    obj12 = MyClass(1)
    obj21 = MyClass(2)
    assert obj11 is obj12
    assert obj11 is not obj21
    assert id(obj11) == id(obj12)
    assert id(obj11) != id(obj21)
