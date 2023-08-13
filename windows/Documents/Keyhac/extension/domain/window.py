from abc import abstractmethod

from .exception import (
    DomainTypeError,
    DomainValueError,
)
from .share import (
    Entity,
    ValueObject,
    Service,
)


class WindowId(ValueObject):
    """ウィンドウの識別番号"""
    def __init__(self, value: int):
        if not value > 0:
            raise DomainValueError(value)
        self._value = value

    def __hash__(self) -> int:
        return hash(self.value)

    @property
    def value(self) -> int:
        return self._value


class WindowQuery(ValueObject):
    """ウィンドウの検索条件"""
    def __init__(
        self,
        exe_name: str = '',
        class_name: str = '',
        window_text: str = '',
    ):
        self._exe_name = exe_name
        self._class_name = class_name
        self._window_text = window_text

    def __hash__(self) -> int:
        return hash((
            self._exe_name,
            self._class_name,
            self._window_text,
        ))

    @property
    def exe_name(self):
        """ウィンドウのプロセス名"""
        return self._exe_name

    @property
    def class_name(self):
        """ウィンドウのクラス名"""
        return self._class_name

    @property
    def window_text(self):
        """ウィンドウのタイトル"""
        return self._window_text


class Window(Entity):
    """ウィンドウを操作するためのインターフェース"""
    def __init__(self, window_id: WindowId):
        self._window_id = window_id

    def __hash__(self) -> int:
        return hash(self._window_id)

    def __eq__(self, other) -> bool:
        if not isinstance(other, Window):
            raise DomainTypeError(other, Window)
        return self._window_id == other._window_id

    @property
    def window_id(self) -> WindowId:
        """ウィンドウの識別番号"""
        return self._window_id

    @abstractmethod
    def activate(self) -> bool:
        """アクティブにする"""
        raise NotImplementedError

    @abstractmethod
    def ime_on(self) -> bool:
        """IMEをONにする"""
        raise NotImplementedError

    @abstractmethod
    def ime_off(self) -> bool:
        """IMEをOFFにする"""
        raise NotImplementedError


class WindowService(Service):
    """ウィンドウを取得するためのインターフェース"""
    @abstractmethod
    def from_id(self, window_id: int) -> 'Window':
        """WindowIdが一致するウィンドウを取得する"""
        raise NotImplementedError

    @abstractmethod
    def from_active(self) -> 'Window':
        """アクティブウィンドウを取得する"""
        raise NotImplementedError

    @abstractmethod
    def from_pointer(self) -> 'Window':
        """ポインター座標にある最前面のウィンドウを取得する"""
        raise NotImplementedError

    @abstractmethod
    def from_query(self, query: WindowQuery) -> 'Window':
        """WindowQueryが一致する最初のウィンドウを取得する"""
        raise NotImplementedError
