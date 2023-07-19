from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
    obsidian = keymap.defineWindowKeymap(
        exe_name='Obsidian.exe',
    )
    obsidian['F19'] = 'C-W'

    obsidian['C-E'] = cmd.make('C-E', cmd.ime_off)
    obsidian['C-F'] = cmd.make('C-F', cmd.ime_off)
    obsidian['C-H'] = cmd.make('C-H', cmd.ime_off)
    obsidian['C-O'] = cmd.make('C-O', cmd.ime_off)
    obsidian['C-S-F'] = cmd.make('C-S-F', cmd.ime_off)
    obsidian['C-S-P'] = cmd.make('C-S-P', cmd.ime_off)
