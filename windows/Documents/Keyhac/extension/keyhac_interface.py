from abc import (
    ABCMeta,
    abstractmethod,
)

from typing import (
    Callable,
    Optional,
    Sequence,
    Union,
)


CmdFunc = Callable[[], None]
KeymapValue = Union[str, Sequence[str], CmdFunc]


class WindowInterface(metaclass=ABCMeta):
    pass


class WindowKeymapInterface(metaclass=ABCMeta):
    """keyhac.WindowKeymap"""

    def __setitem__(self, keys: str, value: Optional[KeymapValue]):
        raise NotImplementedError

    def __getitem__(self, keys: str) -> KeymapValue:
        raise NotImplementedError

    @property
    @abstractmethod
    def applying_func(self):
        raise NotImplementedError

    @applying_func.setter
    def applying_func(self, callback: Callable[[], None]):
        raise NotImplementedError

    @abstractmethod
    def check(self, wnd: WindowInterface) -> bool:
        raise NotImplementedError


CheckFunc = Callable[[WindowInterface], bool]


class KeymapInterface(metaclass=ABCMeta):
    """keyhac.Keymap"""
    @abstractmethod
    def defineModifier(self, src_key: str, dest_key: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def defineWindowKeymap(
        self,
        exe_name: Optional[str] = None,
        class_name: Optional[str] = None,
        window_text: Optional[str] = None,
        check_func: Optional[CheckFunc] = None,
    ) -> WindowKeymapInterface:
        raise NotImplementedError

    @abstractmethod
    def _hook_onKeyDown(self, vk: int, scan: int) -> bool:
        raise NotImplementedError

    @abstractmethod
    def _hook_onKeyUp(self, vk: int, scan: int) -> bool:
        raise NotImplementedError

    @abstractmethod
    def setTimer(self, func: Callable[[], None], sec: int):
        raise NotImplementedError

    @abstractmethod
    def _updateFocusWindow(self):
        raise NotImplementedError

    @abstractmethod
    def beginInput(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def setInput_FromString(self, s: str) -> None:
        raise NotImplementedError

    @abstractmethod
    def endInput(self) -> None:
        raise NotImplementedError
