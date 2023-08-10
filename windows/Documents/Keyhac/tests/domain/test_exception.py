from extension.domain.exception import (
    DomainError,
    DomainTypeError,
    DomainRuntimeError,
    EnvironmentNotFoundError,
    WindowNotFoundError,
)


def test_domain_error():
    e = DomainError('This is domain error')
    assert str(e) == 'This is domain error'


def test_domain_type_error():
    i = 1
    e = DomainTypeError(i, str)
    assert isinstance(e, DomainError)
    assert isinstance(e, TypeError)
    assert str(e) == '1: expect to str, but int'


def test_domain_runtime_error():
    e = DomainRuntimeError('This is runtime error')
    assert isinstance(e, DomainError)
    assert isinstance(e, RuntimeError)
    assert str(e) == 'This is runtime error'


def test_environment_not_found_error():
    e = EnvironmentNotFoundError('OneDrive')
    assert isinstance(e, DomainError)
    assert isinstance(e, RuntimeError)
    assert str(e) == 'OneDrive'


def test_window_not_found_error():
    e = WindowNotFoundError('This is window not found error')
    assert isinstance(e, DomainError)
    assert isinstance(e, RuntimeError)
    assert str(e) == 'This is window not found error'
