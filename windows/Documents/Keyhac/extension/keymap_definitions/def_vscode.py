from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
    vscode = keymap.defineWindowKeymap(
        exe_name='Code.exe',
    )
    # Swap lines
    vscode['U2-Up'] = 'A-Up'
    vscode['U2-Down'] = 'A-Down'

    # Add multiple cursors
    vscode['U2-C-Up'] = 'C-A-Up'
    vscode['U2-C-Down'] = 'C-A-Down'

    # Change focus editors
    vscode['U2-1'] = 'C-1'
    vscode['U2-2'] = 'C-2'
    vscode['U2-3'] = 'C-3'
    vscode['U2-4'] = 'C-4'
    vscode['U2-5'] = 'C-5'
    vscode['U2-6'] = 'C-6'
    vscode['U2-7'] = 'C-7'
    vscode['U2-8'] = 'C-8'
    vscode['U2-9'] = 'C-9'
    vscode['U2-0'] = 'C-0'

    vscode['C-E'] = cmd.make('C-E', cmd.ime_off)
    vscode['C-F'] = cmd.make('C-F', cmd.ime_off)
    vscode['C-H'] = cmd.make('C-H', cmd.ime_off)
    vscode['C-K'] = cmd.make('C-K', cmd.ime_off)
    vscode['C-P'] = cmd.make('C-P', cmd.ime_off)
    vscode['C-T'] = cmd.make('C-T', cmd.ime_off)
    vscode['C-S-F'] = cmd.make('C-S-F', cmd.ime_off)
    vscode['C-S-H'] = cmd.make('C-S-H', cmd.ime_off)
    vscode['C-S-P'] = cmd.make('C-S-P', cmd.ime_off)
    vscode['C-Atmark'] = cmd.make('C-Atmark', cmd.ime_off)
    vscode['F1'] = cmd.make('F1', cmd.ime_off)

    # Switch tabs by back/forward buttons
    vscode['C-XButton1'] = cmd.send_under_mouse_pointer('C-PageUp')
    vscode['C-XButton2'] = cmd.send_under_mouse_pointer('C-PageDown')

    # Close/open/reopen tabs by pressing both back and forward
    vscode['F19'] = cmd.send_under_mouse_pointer('C-W')
    vscode['C-F19'] = cmd.send_under_mouse_pointer('C-T')
    vscode['S-F19'] = cmd.send_under_mouse_pointer('C-S-T')

    # Switch tabs by wheel
    vscode['C-WheelUp'] = 'C-PageUp'
    vscode['C-WheelDown'] = 'C-PageDown'

    vscode['U2-WheelUp'] = 'A-WheelUp'
    vscode['U0-WheelUp'] = 'A-WheelUp'

    vscode['U2-WheelDown'] = 'A-WheelDown'
    vscode['U0-WheelDown'] = 'A-WheelDown'
