from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
    touch = keymap.defineWindowKeymap(
        exe_name='TouchDR.exe',
    )
    touch['XButton1'] = cmd.send_under_mouse_pointer('Up')
    touch['XButton2'] = cmd.send_under_mouse_pointer('Down')
