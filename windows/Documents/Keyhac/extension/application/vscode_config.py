from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class VScodeConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        vscode = self._keymap_service.from_query(WindowQuery(
            exe_name='Code.exe',
        ))

        # Swap lines
        vscode['U2-Up'] = act.send('A-Up')
        vscode['U2-Down'] = act.send('A-Down')

        # Add multiple cursors
        vscode['U2-C-Up'] = act.send('C-A-Up')
        vscode['U2-C-Down'] = act.send('C-A-Down')

        # Change focus editors
        vscode['U2-1'] = act.send('C-1')
        vscode['U2-2'] = act.send('C-2')
        vscode['U2-3'] = act.send('C-3')
        vscode['U2-4'] = act.send('C-4')
        vscode['U2-5'] = act.send('C-5')
        vscode['U2-6'] = act.send('C-6')
        vscode['U2-7'] = act.send('C-7')
        vscode['U2-8'] = act.send('C-8')
        vscode['U2-9'] = act.send('C-9')
        vscode['U2-0'] = act.send('C-0')

        vscode['C-E'] = act.send('C-E') + act.ime_off()
        vscode['C-F'] = act.send('C-F') + act.ime_off()
        vscode['C-H'] = act.send('C-H') + act.ime_off()
        vscode['C-K'] = act.send('C-K') + act.ime_off()
        vscode['C-P'] = act.send('C-P') + act.ime_off()
        vscode['C-T'] = act.send('C-T') + act.ime_off()
        vscode['C-S-F'] = act.send('C-S-F') + act.ime_off()
        vscode['C-S-H'] = act.send('C-S-H') + act.ime_off()
        vscode['C-S-P'] = act.send('C-S-P') + act.ime_off()
        vscode['C-Atmark'] = act.send('C-Atmark') + act.ime_off()
        vscode['F1'] = act.send('F1') + act.ime_off()

        # Switch tabs by back/forward buttons
        vscode['C-XButton1'] = act.activate_window() + act.send('C-PageUp')
        vscode['C-XButton2'] = act.activate_window() + act.send('C-PageDown')

        # Close/open/reopen tabs by pressing both back and forward
        vscode['F22'] = act.send('C-W')
        vscode['C-F22'] = act.send('C-T')
        vscode['S-F22'] = act.send('C-S-T')

        # Switch tabs by wheel
        vscode['C-WheelUp'] = act.send('C-PageUp')
        vscode['C-WheelDown'] = act.send('C-PageDown')
