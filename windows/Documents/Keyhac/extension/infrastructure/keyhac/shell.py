from typing import (
    Callable,
)

from injector import (
    inject,
)

from extension.domain.shell import (
    Command,
    ShellService,
)
from .share import (
    KeymapKeyhac,
)


class ShellServiceKeyhac(ShellService):
    @inject
    def __init__(self, keymap: KeymapKeyhac) -> None:
        self._keymap = keymap

    def run(self, command: Command) -> Callable[[], None]:
        """ファイルを開く、またはプログラムを起動する"""
        return self._keymap.ShellExecuteCommand(
            'open',
            command.file,
            command.param,
            command.working_directory,
            'normal',
        )
