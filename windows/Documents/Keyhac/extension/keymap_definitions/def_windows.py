from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
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
