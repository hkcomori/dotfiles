from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class BrowserConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        browser = (
            self._keymap_service.from_query(WindowQuery(exe_name='msedge.exe'))
            + self._keymap_service.from_query(WindowQuery(exe_name='chrome.exe'))   # noqa: W503
            + self._keymap_service.from_query(WindowQuery(exe_name='firefox.exe'))  # noqa: W503
            + self._keymap_service.from_query(WindowQuery(exe_name='vivaldi.exe'))  # noqa: W503
        )

        # Disable IME when search box activate
        browser['C-E'] = act.send('C-E') + act.ime_off()
        browser['C-F'] = act.send('C-F') + act.ime_off()
        browser['C-L'] = act.send('C-L') + act.ime_off()
        browser['C-T'] = act.send('C-T') + act.ime_off()
        browser['F1'] = act.send('F1') + act.ime_off()
        browser['F3'] = act.send('F3') + act.ime_off()
        browser['S-F3'] = act.send('S-F3') + act.ime_off()
        browser['C-S-P'] = act.send('C-S-P') + act.ime_off()

        # Switch tabs by back/forward buttons
        browser['C-XButton1'] = act.activate_window() + act.send('C-S-Tab')
        browser['C-XButton2'] = act.activate_window() + act.send('C-Tab')

        # Close/open/reopen tabs by pressing both back and forward
        browser['F22'] = act.send('C-W')
        browser['C-F22'] = act.send('C-T')
        browser['S-F22'] = act.send('C-S-T')

        # Switch tabs by wheel
        browser['C-WheelUp'] = act.activate_window() + act.send('C-S-Tab')
        browser['C-WheelDown'] = act.activate_window() + act.send('C-Tab')

        devtools = (
            self._keymap_service.from_query(WindowQuery(exe_name='msedge.exe', window_text='DevTools - *'))             # noqa: W503
            + self._keymap_service.from_query(WindowQuery(exe_name='chrome.exe', window_text='DevTools - *'))           # noqa: W503
            + self._keymap_service.from_query(WindowQuery(exe_name='vivaldi.exe', window_text='Developer Tools - *'))   # noqa: W503
        )
        # Close window by pressing both back and forward
        devtools['F22'] = act.send('C-W')
