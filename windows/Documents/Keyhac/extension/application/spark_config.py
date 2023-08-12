from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class SparkConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        spark = self._keymap_service.from_query(WindowQuery(
            exe_name='Spark Desktop.exe',
            class_name='Chrome_WidgetWin_1',
        ))

        spark['XButton1'] = act.activate_window() + act.send('Esc')
