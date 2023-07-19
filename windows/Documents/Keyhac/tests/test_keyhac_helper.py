from typing import (
    Callable,
    Sequence,
)
import pytest

import tests.mock   # noqa: F401

from extension.keyhac_interface import (
    WindowInterface,
    WindowKeymapInterface,
)
from extension.keyhac_helper import (
    KeymapValue,
    WindowKeymapEx,
    WindowKeymapGroup,
    KeyCondition,
)


class MyWindow(WindowInterface):
    pass


class MyWindowKeymap(dict, WindowKeymapInterface):
    def __init__(self):
        self.check_result = True

    @property
    def applying_func(self):
        return self._applying_func

    @applying_func.setter
    def applying_func(self, callback: Callable[[], None]):
        self._applying_func = callback

    def check(self, wnd: WindowInterface) -> bool:
        return self.check_result


def nop():
    pass


@pytest.mark.parametrize(('src', 'expected'), [
    ('A-Tab', 'A-Tab'),
    ('A-A-Tab', 'A-Tab'),
    ('W-S-F24', 'W-S-(135)'),
    # (('A-WheelUp', 'Up', 'F24'), ('A-(159)', 'Up', '(135)')),
    ('LaunchApp2', '(183)'),
])
def test_KeyCondition(src: str, expected: str):
    assert KeyCondition(src).to_keyhac() == expected


@pytest.mark.parametrize(('key', 'expected_key', 'value', 'expected_value'), [
    ('A-Tab', 'A-Tab', 'A-C-Tab', 'A-C-Tab'),
    ('W-S-F24', 'W-S-(135)', 'C-A-F13', 'C-A-(124)'),
    ('A-WheelUp', 'A-(159)', ('WheelUp', 'WheelUp'), ('(159)', '(159)')),
    ('LaunchApp2', '(183)', nop, nop),
])
def test_get_set(
    key: str, expected_key: str,
    value: KeymapValue, expected_value: KeymapValue
):
    keymap = WindowKeymapEx(MyWindowKeymap())
    keymap[key] = value
    assert keymap[key] == expected_value
    assert keymap[key] == expected_value


def test_set_none():
    """Set function to do nothing for keys mapped to None"""
    keymap = WindowKeymapEx(MyWindowKeymap())
    keymap['Insert'] = None
    assert keymap['Insert']() is None


@pytest.mark.parametrize(('key', 'expected_key', 'value', 'expected_value'), [
    ('*-A-Tab', ('C-A-Tab', 'A-Tab', 'S-A-Tab', 'W-A-Tab', 'U0-A-Tab', 'U1-A-Tab', 'U2-A-Tab', 'U3-A-Tab'), 'A-C-WheelUp', 'A-C-(159)'),
])
def test_wildcard_keymap(
    key: str, expected_key: Sequence[str],
    value: KeymapValue, expected_value: KeymapValue
):
    keymap = WindowKeymapEx(MyWindowKeymap())
    keymap[key] = value
    for k in expected_key:
        assert keymap[k] == expected_value


def test_grouping():
    keymap1 = WindowKeymapEx(MyWindowKeymap())
    keymap2 = WindowKeymapEx(MyWindowKeymap())
    keymap1['Insert'] = 'Insert'
    keymap2['Insert'] = 'Enter'
    assert keymap1['Insert'] == 'Insert'
    assert keymap2['Insert'] == 'Enter'
    group = WindowKeymapGroup(keymap1, keymap2)
    group['Tab'] = 'Pause'
    assert group['Tab'] == 'Pause'
    assert keymap1['Tab'] == 'Pause'
    assert keymap2['Tab'] == 'Pause'
    group.applying_func = nop
    assert group.applying_func == nop
    assert keymap1.applying_func == nop
    assert keymap2.applying_func == nop


def test_check():
    mock_keymap1 = MyWindowKeymap()
    keymap1 = WindowKeymapEx(mock_keymap1)
    mock_keymap1.check_result = True
    assert keymap1.check(MyWindow()) is True
    mock_keymap1.check_result = False
    assert keymap1.check(MyWindow()) is not True

    mock_keymap2 = MyWindowKeymap()
    keymap2 = WindowKeymapEx(mock_keymap2)

    group = WindowKeymapGroup(keymap1, keymap2)
    (mock_keymap1.check_result, mock_keymap2.check_result) = (False, False)
    assert group.check(MyWindow()) is not True
    (mock_keymap1.check_result, mock_keymap2.check_result) = (False, True)
    assert group.check(MyWindow()) is not True
    (mock_keymap1.check_result, mock_keymap2.check_result) = (True, False)
    assert group.check(MyWindow()) is not True
    (mock_keymap1.check_result, mock_keymap2.check_result) = (True, True)
    assert group.check(MyWindow()) is True
