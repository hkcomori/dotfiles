import pytest

from extension.domain.exception import (
    DomainValueError,
)
from extension.domain.window import (
    WindowId,
    WindowQuery,
)
from extension.infrastructure.mock.window import (
    WindowMock,
)


class Test_WindowId:
    def test_repr_and_str(self):
        window_id = WindowId(3)

        assert repr(window_id) == 'WindowId(3)'
        assert str(window_id) == '3'

    @pytest.mark.parametrize('value', [
        0,
        -1,
        -32768,
    ])
    def test_DomainValueError(self, value):
        with pytest.raises(DomainValueError):
            WindowId(value)


class Test_WindowQuery:
    def test_repr_and_str(self):
        query = WindowQuery('EXE NAME1', 'class NAME', 'WindowText')

        assert repr(query) == "WindowQuery('EXE NAME1', 'class NAME', 'WindowText')"
        assert str(query) == "('EXE NAME1', 'class NAME', 'WindowText')"

    @pytest.mark.parametrize((
        'args',
        'args_ne',
    ), [(
        ('EXE NAME1', 'class NAME', 'WindowText'),
        ('EXE NAME2', 'class NAME', 'WindowText'),
    ), (
        ('EXE NAME', 'class NAME1', 'WindowText'),
        ('EXE NAME', 'class NAME2', 'WindowText'),
    ), (
        ('EXE NAME', 'class NAME', 'WindowText1'),
        ('EXE NAME', 'class NAME', 'WindowText2'),
    )])
    def test_match(self, args, args_ne):
        query = WindowQuery(*args)
        wnd_eq = WindowMock(WindowId(1), *args)
        wnd_ne = WindowMock(WindowId(1), *args_ne)

        assert query.match(wnd_eq) is True
        assert query.match(wnd_ne) is False
