from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class WindowsConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        explorer = self._keymap_service.from_query(WindowQuery(
            exe_name='explorer.exe',
            class_name='DirectUIHWND',
        ))
        explorer['F19'] = act.activate_window() + act.send('C-W')

        explorer_location = self._keymap_service.from_query(WindowQuery(
            exe_name='explorer.exe',
            class_name='Edit',
        ))
        explorer_location.applying_func = act.ime_off()

        explorer_search = self._keymap_service.from_query(WindowQuery(
            exe_name='SearchApp.exe',
            class_name='Windows.UI.Core.CoreWindow',
            window_text='検索'
        ))
        explorer_search.applying_func = act.ime_off()
