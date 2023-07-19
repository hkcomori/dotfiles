from ..keyhac_helper import (
    KeymapEx,
    WindowKeymapGroup,
)
from .. import cmd


def init(keymap: KeymapEx):
    word = keymap.defineWindowKeymap(exe_name='WORD.EXE')
    excel = keymap.defineWindowKeymap(exe_name='EXCEL.EXE')
    powerpoint = keymap.defineWindowKeymap(exe_name='POWERPNT.EXE')
    outlook_main = keymap.defineWindowKeymap(exe_name='OUTLOOK.EXE', window_text='* - Outlook')
    outlook_child = keymap.defineWindowKeymap(exe_name='OUTLOOK.EXE', check_func=lambda wnd: not outlook_main.check(wnd))
    office = WindowKeymapGroup(     # noqa: F841
        word,
        excel,
        powerpoint,
    )

    excel['A-WheelDown'] = 'PageDown'
    excel['U0-WheelDown'] = 'PageDown'
    excel['A-WheelUp'] = 'PageUp'
    excel['U0-WheelUp'] = 'PageUp'

    outlook_main['C-E'] = cmd.make('C-E', cmd.ime_off)

    # Ctrl+F to search instead of forwarding
    outlook_child['C-F'] = 'F4'

    # Close message window by pressing both back and forward
    outlook_child['F19'] = cmd.send_under_mouse_pointer('A-F4')

    outlook_child['XButton1'] = cmd.send_under_mouse_pointer('C-S-Comma')
    outlook_child['XButton1'] = cmd.send_under_mouse_pointer('C-S-Period')
