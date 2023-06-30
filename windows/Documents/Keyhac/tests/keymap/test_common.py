import sys
from typing import (
    Sequence,
)
from unittest.mock import Mock

import pytest

sys.modules['ctypes.windll'] = Mock()
sys.modules['ctypes.WINFUNCTYPE'] = Mock()
sys.modules['pyauto'] = Mock()
sys.modules['keyhac'] = Mock()

from extension.keymap.common import (    # noqa: E402
    KeymapValue,
    KeymapDefinition,
    KeyCondition,
)


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
    keymap = KeymapDefinition()
    keymap[key] = value
    assert keymap[key] == expected_value
    assert keymap[key] == expected_value
    KeymapDefinition.clear()


@pytest.mark.parametrize(('key', 'expected_key', 'value', 'expected_value'), [
    ('*-A-Tab', ('C-A-Tab', 'A-Tab', 'S-A-Tab', 'W-A-Tab', 'U0-A-Tab', 'U1-A-Tab', 'U2-A-Tab', 'U3-A-Tab'), 'A-C-WheelUp', 'A-C-(159)'),
])
def test_wildcard_keymap(
    key: str, expected_key: Sequence[str],
    value: KeymapValue, expected_value: KeymapValue
):
    keymap = KeymapDefinition()
    keymap[key] = value
    for k in expected_key:
        assert keymap[k] == expected_value
    KeymapDefinition.clear()


def test_instance_identification():
    keymap1 = KeymapDefinition()
    keymap2 = KeymapDefinition()
    assert keymap1 is keymap2
    KeymapDefinition.clear()
