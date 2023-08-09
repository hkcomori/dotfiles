from extension.domain.shell import (
    Command,
)


def test_command():
    command1 = Command(
        'file\\NAME',
        'param',
        'working/directory',
    )
    command2 = Command(
        'FileName',
        'class2 NAME',
        'working/directory',
    )
    command3 = Command(
        'file\\NAME',
        'param',
        'working/directory',
    )
    assert command1.file == 'file\\NAME'
    assert command1.param == 'param'
    assert command1.working_directory == 'working/directory'

    assert command1 == command1
    assert command1 == command3
    assert command1 != command2
    assert command1 is command1
    assert command1 is not command3
    assert command1 is not command2
