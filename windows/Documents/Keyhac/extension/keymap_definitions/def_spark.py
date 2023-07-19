from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
    spark = keymap.defineWindowKeymap(
        exe_name='Spark Desktop.exe',
        class_name='Chrome_WidgetWin_1',
    )
    spark['XButton1'] = cmd.send_under_mouse_pointer('Esc')
