from ..keyhac_helper import (
    KeymapEx,
    WindowKeymapGroup,
)
from .. import cmd


def init(keymap: KeymapEx):
    browser = WindowKeymapGroup(
        keymap.defineWindowKeymap(exe_name='msedge.exe'),
        keymap.defineWindowKeymap(exe_name='chrome.exe'),
        keymap.defineWindowKeymap(exe_name='firefox.exe'),
        keymap.defineWindowKeymap(exe_name='vivaldi.exe'),
    )
    # Switch tabs by back/forward buttons
    browser['XButton1'] = cmd.send_under_mouse_pointer('C-S-Tab')
    browser['XButton2'] = cmd.send_under_mouse_pointer('C-Tab')

    # Close/open/reopen tabs by pressing both back and forward
    browser['F19'] = cmd.send_under_mouse_pointer('C-W')
    browser['C-F19'] = cmd.send_under_mouse_pointer('C-T')
    browser['S-F19'] = cmd.send_under_mouse_pointer('C-S-T')

    # Switch tabs by wheel
    browser['C-WheelUp'] = cmd.send_under_mouse_pointer('C-S-Tab')
    browser['C-WheelDown'] = cmd.send_under_mouse_pointer('C-Tab')

    devtools = WindowKeymapGroup(
        keymap.defineWindowKeymap(exe_name='msedge.exe', window_text='DevTools - *'),
        keymap.defineWindowKeymap(exe_name='chrome.exe', window_text='DevTools - *'),
        keymap.defineWindowKeymap(exe_name='vivaldi.exe', window_text='Developer Tools - *'),
    )
    # Close window by pressing both back and forward
    devtools['F19'] = cmd.send_under_mouse_pointer('C-W')
