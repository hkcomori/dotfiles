from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class AcrobatConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        acrobat = self._keymap_service.from_query(WindowQuery(
            exe_name='Acrobat.exe',
            class_name='AVL_AVView',
        ))

        # Close Tab
        acrobat['F22'] = act.send('C-W')

        # Switch Tabs
        acrobat['C-PageUp'] = act.send('C-S-Tab')
        acrobat['C-PageDown'] = act.send('C-Tab')

        # Zoom to fit page width
        acrobat['C-F22'] = act.send('C-0')

        # Back / Forward
        acrobat['XButton1'] = act.send('A-Left')
        acrobat['XButton2'] = act.send('A-Right')

        # Rotate pages
        acrobat['C-S-WheelDown'] = act.send('C-S-Minus')
        acrobat['C-S-WheelUp'] = act.send('C-S-Semicolon')
