from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class LineConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        line_main = self._keymap_service.from_query(WindowQuery(
            exe_name='LINEAPP.exe',
            class_name='Qt5152QWindowIcon',
        ))

        line_main['C-Enter'] = act.send('A-Enter')
        line_main['U3-C-M'] = act.send('A-Enter')
