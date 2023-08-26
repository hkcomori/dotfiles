from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class HoneyviewConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        honeyview = self._keymap_service.from_query(WindowQuery(
            exe_name='Honeyview.exe',
        ))

        # Close window
        honeyview['F22'] = act.send('C-W')
