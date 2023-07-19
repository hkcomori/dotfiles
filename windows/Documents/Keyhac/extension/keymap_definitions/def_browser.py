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

    # Disable IME when search box activate
    browser['C-E'] = cmd.make('C-E', cmd.ime_off)
    browser['C-F'] = cmd.make('C-F', cmd.ime_off)
    browser['C-L'] = cmd.make('C-L', cmd.ime_off)
    browser['C-T'] = cmd.make('C-T', cmd.ime_off)
    browser['F1'] = cmd.make('F1', cmd.ime_off)
    browser['F3'] = cmd.make('F3', cmd.ime_off)
    browser['S-F3'] = cmd.make('S-F3', cmd.ime_off)
    browser['C-S-P'] = cmd.make('C-S-P', cmd.ime_off)

    # Switch tabs by back/forward buttons
    browser['C-XButton1'] = cmd.send_under_mouse_pointer('C-S-Tab')
    browser['C-XButton2'] = cmd.send_under_mouse_pointer('C-Tab')

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
