from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class TouchConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        touch = self._keymap_service.from_query(WindowQuery(
            exe_name='TouchDR.exe',
        ))

        touch['XButton1'] = act.activate_window() + act.send('Up')
        touch['XButton2'] = act.activate_window() + act.send('Down')
