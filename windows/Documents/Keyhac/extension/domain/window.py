from abc import abstractmethod
from fnmatch import fnmatch

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

    def __repr__(self) -> str:
        return f'WindowId({self.value})'

    def __str__(self) -> str:
        return str(self.value)

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

    def __repr__(self) -> str:
        return f"WindowQuery('{self.exe_name}', '{self.class_name}', '{self.window_text}')"

    def __str__(self) -> str:
        return f"('{self.exe_name}', '{self.class_name}', '{self.window_text}')"

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

    def match(self, wnd: 'Window') -> bool:
        """ウィンドウが検索条件にマッチするかを判定する"""
        return all((
            self.exe_name == '' or fnmatch(wnd.exe_name, self.exe_name),
            self.window_text == '' or fnmatch(wnd.window_text, self.window_text),
            self.class_name == '' or fnmatch(wnd.class_name, self.class_name),
        ))


class Window(Entity):
    """ウィンドウを操作するためのインターフェース"""
    def __hash__(self) -> int:
        return hash(self.window_id)

    def __eq__(self, other) -> bool:
        if not isinstance(other, Window):
            raise DomainTypeError(other, Window)
        return self.window_id == other.window_id

    def __repr__(self) -> str:
        return f"Window({self.window_id}, '{self.exe_name}', '{self.class_name}', '{self.window_text}')"

    def __str__(self) -> str:
        return f"('{self.exe_name}', '{self.class_name}', '{self.window_text}')"

    @property
    def window_id(self) -> WindowId:
        """ウィンドウの識別番号"""
        raise NotImplementedError

    @property
    @abstractmethod
    def exe_name(self) -> str:
        """ウィンドウのプロセス名"""
        raise NotImplementedError

    @property
    @abstractmethod
    def class_name(self) -> str:
        """ウィンドウのクラス名"""
        raise NotImplementedError

    @property
    @abstractmethod
    def window_text(self) -> str:
        """ウィンドウのタイトル"""
        raise NotImplementedError

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
