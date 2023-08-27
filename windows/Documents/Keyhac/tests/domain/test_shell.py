from extension.domain.shell import (
    Command,
    ShellService,
)
from .dependency import (
    Dependency,
)


def test_Command():
    pass


def test_ShellService():
    command1 = Command(
        'file\\NAME',
        'param',
        'working/directory',
    )
    shell_service: ShellService = Dependency().resolve(ShellService)
    shell_service.run(command1)
