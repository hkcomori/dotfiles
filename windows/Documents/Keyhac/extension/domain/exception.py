class DomainError(Exception):
    """
    ドメインモデル内で発生する例外の基底クラス。
    このクラスの例外を直接発生させることはないが、
    例外を補足する際に使用する。
    """
    pass


class DomainTypeError(DomainError, TypeError):
    """
    ドメインモデル内で発生する TypeError で使用する。
    """
    def __init__(self, variable: object, expect: type):
        msg = f'{variable}: expect to {expect.__name__}, but {type(variable).__name__}'
        super().__init__(msg)


class DomainValueError(DomainError, ValueError):
    """
    ドメインモデル内で発生する TypeError で使用する。
    """

    def __init__(self, variable: object):
        msg = f'{variable} is out of range'
        super().__init__(msg)


class DomainRuntimeError(DomainError, RuntimeError):
    """
    ドメインモデル内で発生する RuntimeError で使用する。
    """
    pass


class EnvironmentNotFoundError(DomainRuntimeError):
    """
    環境変数が見つからなかった場合に発生する。
    """
    def __init__(self, environment_name: str):
        super().__init__(environment_name)


class WindowNotFoundError(DomainRuntimeError):
    """
    Window が見つからなかった場合に発生する。
    """
    pass
