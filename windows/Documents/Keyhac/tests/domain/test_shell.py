from extension.domain.shell import (
    Command,
    ShellService,
)
from .dependency import (
    Dependency,
)


class Test_Command():
    pass


class Test_ShellService:
    def test_run(self):
        command1 = Command(
            'file\\NAME',
            'param',
            'working/directory',
        )
        shell_service: ShellService = Dependency().resolve(ShellService)
        shell_service.run(command1)
