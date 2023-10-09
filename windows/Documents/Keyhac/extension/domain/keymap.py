from abc import abstractmethod

from .share import (
    Repository,
    Service,
)
from .exception import (
    DomainTypeError,
)
from .action import (
    Action,
)
from .window import (
    WindowQuery,
)


class KeymapRegistry(Repository):
    """キーマップを登録するインターフェース"""
    def __add__(self, other) -> 'KeymapRegistry':
        if not isinstance(other, KeymapRegistry):
            raise DomainTypeError(other, KeymapRegistry)
        return KeymapRegistryGroup(self, other)

    @abstractmethod
    def __setitem__(self, key: str, action: Action):
        raise NotImplementedError

    @abstractmethod
    def replace(self, src: str, dst: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def update(self) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def applying_func(self):
        """
        キーマップが有効になるウィンドウがアクティブになったときに実行するアクション
        """
        raise NotImplementedError

    @applying_func.setter
    def applying_func(self, action: Action):
        raise NotImplementedError


class KeymapRegistryGroup(KeymapRegistry):
    """複数の KeymapRegistry を束ねて一括操作するインターフェース"""
    def __init__(self, *regs: KeymapRegistry):
        self._regs = regs

    def __add__(self, other) -> 'KeymapRegistry':
        if isinstance(other, KeymapRegistryGroup):
            return KeymapRegistryGroup(*self._regs, *other._regs)
        if isinstance(other, KeymapRegistry):
            return KeymapRegistryGroup(*self._regs, other)
        raise DomainTypeError(other, KeymapRegistry)

    def __setitem__(self, key: str, action: Action):
        for reg in self._regs:
            reg[key] = action

    def replace(self, src: str, dst: str) -> None:
        for reg in self._regs:
            reg.replace(src, dst)

    def update(self) -> None:
        for reg in self._regs:
            reg.update()

    @property
    def applying_func(self):
        return self._regs[0].applying_func

    @applying_func.setter
    def applying_func(self, action: Action):
        for reg in self._regs:
            reg.applying_func = action


class KeymapService(Service):
    """キーマップを操作するインターフェース"""
    @property
    @abstractmethod
    def user_modifier0(self) -> str:
        """ユーザー定義モディファイアキー0"""
        raise NotImplementedError

    @user_modifier0.setter
    def user_modifier0(self, key: str) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def user_modifier1(self) -> str:
        """ユーザー定義モディファイアキー1"""
        raise NotImplementedError

    @user_modifier1.setter
    def user_modifier1(self, key: str) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def user_modifier2(self) -> str:
        """ユーザー定義モディファイアキー2"""
        raise NotImplementedError

    @user_modifier2.setter
    def user_modifier2(self, key: str) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def user_modifier3(self) -> str:
        """ユーザー定義モディファイアキー3"""
        raise NotImplementedError

    @user_modifier3.setter
    def user_modifier3(self, key: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def from_query(self, query: WindowQuery) -> KeymapRegistry:
        """WindowQueryに合致するウィンドウのKeymapRegistryを取得する"""
        raise NotImplementedError
