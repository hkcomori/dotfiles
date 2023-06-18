import pytest

from extension.key import (
    KeymapCommand,
    WindowKeymap,
    KeymapConverter,
)


def nop():
    pass


@pytest.mark.parametrize(('src', 'expected'), [
    ('A-Tab', 'A-Tab'),
    ('W-S-F24', 'W-S-(135)'),
    (('A-WheelUp', 'Up', 'F24'), ('A-(159)', 'Up', '(135)')),
    ('LaunchApp2', '(183)'),
])
def test_convert(src: str, expected: str):
    assert KeymapConverter.convert(src) == expected


@pytest.mark.parametrize(('key', 'expected_key', 'value', 'expected_value'), [
    ('A-Tab', 'A-Tab', 'A-C-Tab', 'A-C-Tab'),
    ('W-S-F24', 'W-S-(135)', 'C-A-F13', 'C-A-(124)'),
    ('A-WheelUp', 'A-(159)', ('WheelUp', 'WheelUp'), ('(159)', '(159)')),
    ('LaunchApp2', '(183)', nop, nop),
])
def test_get_set(
    key: str, expected_key: str,
    value: KeymapCommand, expected_value: KeymapCommand
):
    orig_keymap: WindowKeymap = dict()
    keymap = KeymapConverter(orig_keymap)
    keymap[key] = value
    assert orig_keymap[expected_key] == expected_value
    assert keymap[expected_key] == expected_value
