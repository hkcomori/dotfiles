from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class OfficeConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        word = self._keymap_service.from_query(WindowQuery(exe_name='WORD.EXE'))
        excel = self._keymap_service.from_query(WindowQuery(exe_name='EXCEL.EXE'))
        powerpoint = self._keymap_service.from_query(WindowQuery(exe_name='POWERPNT.EXE'))
        outlook_main = self._keymap_service.from_query(WindowQuery(exe_name='OUTLOOK.EXE', class_name='OutlookGrid'))
        outlook_child = self._keymap_service.from_query(WindowQuery(exe_name='OUTLOOK.EXE', class_name='_WwG'))
        office = word + excel + powerpoint

        excel_search = self._keymap_service.from_query(WindowQuery(
            exe_name='EXCEL.EXE',
            class_name='EDTBX',
        ))

        excel['A-WheelDown'] = act.send('PageDown')
        excel['U2-WheelDown'] = act.send('PageDown')
        excel['A-WheelUp'] = act.send('PageUp')
        excel['U2-WheelUp'] = act.send('PageUp')

        excel_search.applying_func = act.ime_off()

        office['F22'] = act.close_window()

        outlook_main['C-E'] = act.send('C-E') + act.ime_off()

        # Ctrl+F to search instead of forwarding
        outlook_child['C-F'] = act.send('F4')

        # Close message window by pressing both back and forward
        outlook_child['F22'] = act.close_window()

        outlook_child['XButton1'] = act.send('C-S-Comma')
        outlook_child['XButton2'] = act.send('C-S-Period')
