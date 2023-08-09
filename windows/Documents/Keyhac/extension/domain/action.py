import os
from typing import (
    Optional,
)

from extension.vendor.injector import (
    inject,
)

from .keymap import (
    Action,
)
from .exception import (
    WindowNotFoundError,
)
from .window import (
    WindowQuery,
    WindowFactory,
)
from .desktop import (
    DesktopFactory,
)
from .shell import (
    Command,
    ShellService,
)
from extension.keyhac_helper import (
    KeymapEx,
)


class ActionService():
    @inject
    def __init__(
        self,
        keymap: KeymapEx,
        window_factory: WindowFactory,
        desktop_factory: DesktopFactory,
        shell_service: ShellService,
    ):
        self._keymap = keymap
        self._window_factory = window_factory
        self._desktop_factory = desktop_factory
        self._shell_service = shell_service

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
            return self._shell_service.run(Command(process_path))
        else:
            return window.activate

    @property
    def open_onedrive(self) -> Action:
        """OneDriveフォルダを開く"""
        file = f'{os.getenv("OneDrive")}'
        return Action(self._shell_service.run(Command(file)))

    @property
    def open_documents(self) -> Action:
        """Documentsフォルダを開く"""
        file = os.popen(
            'powershell.exe -Command "([Environment])::GetFolderPath("""MyDocuments""")"'
        ).read().rstrip('\n').replace(os.sep, '/')
        return Action(self._shell_service.run(Command(file)))

    @property
    def launch_obsidian(self) -> Action:
        """Obsidianを開く"""
        file = f'{os.getenv("USERPROFILE")}/AppData/Local/Obsidian/Obsidian.exe'
        return Action(self._shell_service.run(Command(file)))

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
            self._desktop_factory.from_active().lock_on
        )
