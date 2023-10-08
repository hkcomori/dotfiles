from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class CalculatorConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        explorer = self._keymap_service.from_query(WindowQuery(
            exe_name='Calculator*.exe',
            class_name='Windows.UI.Core.CoreWindow',
            window_text='電卓',
        ))
        explorer['F22'] = act.close_window()
