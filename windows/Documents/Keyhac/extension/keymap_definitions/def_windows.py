from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
    explorer = keymap.defineWindowKeymap(
        exe_name='explorer.exe',
        class_name='DirectUIHWND',
    )
    # explorer['F19'] = cmd.send_under_mouse_pointer('C-W')
    explorer['F19'] = cmd.close_window_under_mouse_pointer

    explorer_location = keymap.defineWindowKeymap(
        exe_name='explorer.exe',
        class_name='Edit',
    )
    explorer_location.applying_func = cmd.ime_off

    explorer_search = keymap.defineWindowKeymap(
        exe_name='SearchApp.exe',
        class_name='Windows.UI.Core.CoreWindow',
        window_text='検索'
    )
    explorer_search.applying_func = cmd.ime_off
