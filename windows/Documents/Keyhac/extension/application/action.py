import os
from typing import (
    Optional,
)

from extension.vendor.injector import (
    inject,
)

from extension.domain.keymap import (
    Action,
)
from extension.domain.window import (
    WindowQuery,
    WindowFactory,
    WindowNotFoundError,
)
from extension.infrastructure.win32.desktop import (
    DesktopFactoryWin32,
)
from extension.keyhac_helper import (
    KeymapEx,
)


class ActionService():
    @inject
    def __init__(self, keymap: KeymapEx, window_factory: WindowFactory):
        self._keymap = keymap
        self._window_factory = window_factory

    def _open_file(
        self,
        file: str,
        param: str = '',
        directory: str = '',
        swmode: str = 'normal',
    ):
        """ファイルを開く、またはプログラムを起動する"""
        verb = 'open'
        return self._keymap.ShellExecuteCommand(
            verb, file, param, directory, swmode)

    def _launch_or_activate(
        self,
        process_path: str,
        query: Optional[WindowQuery] = None
    ):
        """kwargsに合致するウィンドウがないときにprocess_pathを起動する"""
        exe_name = os.path.basename(process_path)
        _query = WindowQuery(exe_name=exe_name) if query is None else query
        try:
            window = self._window_factory.from_find(_query)
        except WindowNotFoundError:
            return self._open_file(process_path)
        else:
            return window.activate

    @property
    def open_onedrive(self) -> Action:
        """OneDriveフォルダを開く"""
        return Action(self._open_file(
            f'{os.getenv("OneDrive")}',
        ))

    @property
    def open_documents(self) -> Action:
        """Documentsフォルダを開く"""
        return Action(self._open_file(
            os.popen(
                'powershell.exe -Command "([Environment])::GetFolderPath("""MyDocuments""")"'
            ).read().rstrip('\n').replace(os.sep, '/'),
        ))

    @property
    def launch_obsidian(self) -> Action:
        """Obsidianを開く"""
        return Action(self._open_file(
            f'{os.getenv("USERPROFILE")}/AppData/Local/Obsidian/Obsidian.exe',
        ))

    @property
    def launch_calc(self) -> Action:
        """電卓を開く"""
        return Action(self._launch_or_activate(
            'calc.exe',
            WindowQuery(
                exe_name='ApplicationFrameHost.exe',
                window_text='電卓'
            ),
        ))

    @property
    def ime_on(self) -> Action:
        """IMEをONにする"""
        return Action(
            self._window_factory.from_active().ime_on
        )

    @property
    def ime_off(self) -> Action:
        """IMEをOFFにする"""
        return Action(
            self._window_factory.from_active().ime_off
        )

    @property
    def turn_off_monitor(self) -> Action:
        """モニターの電源を切る"""
        return Action(
            DesktopFactoryWin32().from_active().lock_on
        )
