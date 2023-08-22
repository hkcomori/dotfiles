from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class AcrobatConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        obsidian = self._keymap_service.from_query(WindowQuery(
            exe_name='Acrobat.exe',
            class_name='AVL_AVView',
        ))

        # Close Tab
        obsidian['F22'] = act.send('C-W')

        # Switch Tabs
        obsidian['C-PageUp'] = act.send('C-S-Tab')
        obsidian['C-PageDown'] = act.send('C-Tab')

        # Zoom to fit page width
        obsidian['C-F22'] = act.send('C-0')

        # Back / Forward
        obsidian['XButton1'] = act.send('A-Left')
        obsidian['XButton2'] = act.send('A-Right')

        # Rotate pages
        obsidian['C-S-WheelDown'] = act.send('C-S-Minus')
        obsidian['C-S-WheelUp'] = act.send('C-S-Semicolon')
