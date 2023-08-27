from extension.domain.exception import (
    DomainError,
    DomainTypeError,
    DomainRuntimeError,
    EnvironmentNotFoundError,
    WindowNotFoundError,
)


def test_DomainError():
    e = DomainError('This is domain error')
    assert str(e) == 'This is domain error'


def test_DomainTypeError():
    i = 1
    e = DomainTypeError(i, str)
    assert isinstance(e, DomainError)
    assert isinstance(e, TypeError)
    assert str(e) == '1: expect to str, but int'


def test_DomainRuntimeError():
    e = DomainRuntimeError('This is runtime error')
    assert isinstance(e, DomainError)
    assert isinstance(e, RuntimeError)
    assert str(e) == 'This is runtime error'


def test_EnvironmentNotFoundError():
    e = EnvironmentNotFoundError('OneDrive')
    assert isinstance(e, DomainError)
    assert isinstance(e, RuntimeError)
    assert str(e) == 'OneDrive'


def test_WindowNotFoundError():
    e = WindowNotFoundError('This is window not found error')
    assert isinstance(e, DomainError)
    assert isinstance(e, RuntimeError)
    assert str(e) == 'This is window not found error'
