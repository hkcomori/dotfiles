from typing import (
    Callable,
)

from extension.domain.shell import (
    Command,
    ShellService,
)


class ShellServiceMock(ShellService):
    def run(self, command: Command) -> Callable[[], None]:
        return lambda: None
