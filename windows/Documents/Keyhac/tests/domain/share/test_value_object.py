import pytest

from extension.domain.share import ValueObject
from extension.domain.window import (
    WindowId,
    WindowQuery,
)
from extension.domain.input import (
    Input,
)
from extension.domain.shell import (
    Command,
)


class DummyValueObject(ValueObject):
    def __init__(self, *args) -> None:
        self._args = args

    def __hash__(self) -> int:
        return hash(self._args)


@pytest.mark.parametrize((
    'cls',      # テスト対象のValueObjectクラス
    'args',     # 比較元のValueObjectの値
    'args_ne',  # 不一致となるValueObjectの値
), [(
    WindowId,   # value が不一致
    (1,),
    (2,),
), (
    WindowQuery,    # exe_name が不一致
    ('Exe1', 'Class', 'Window Text'),
    ('Exe2', 'Class', 'Window Text'),
), (
    WindowQuery,    # class_name が不一致
    ('Exe', 'Class1', 'Window Text'),
    ('Exe', 'Class2', 'Window Text'),
), (
    WindowQuery,    # class_name が不一致
    ('Exe', 'Class', 'Window Text 1'),
    ('Exe', 'Class', 'Window Text 2'),
), (
    Input,  # value が不一致
    ('A-Z',),
    ('C-X',),
), (
    Command,    # file が不一致
    ('file', 'param1 param2', '/path/to'),
    ('/path/to/file', 'param1 param2', '/path/to'),
), (
    Command,    # param が不一致
    ('file', 'param1', '/path/to'),
    ('file', 'param1 param2', '/path/to'),
), (
    Command,    # working_directory が不一致
    ('file', 'param1 param2', '/path/to/1'),
    ('file', 'param1 param2', '/path/to/2'),
)])
def test_ValueObject(cls, args, args_ne):
    """ValueObjectの共通インターフェースをテストする"""
    value = cls(*args)
    value_eq = cls(*args)
    value_ne = cls(*args_ne)
    dummy = DummyValueObject(*args)

    # __hash__()
    assert hash(value) == hash(value_eq)
    assert hash(value) != hash(value_ne)

    # __eq__()
    assert value == value
    assert value == value_eq
    assert value != value_ne
    assert value != dummy       # 引数が同じでも、型が異なると不一致
