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


class WindowKeymapInterface(metaclass=ABCMeta):
    """keyhac.WindowKeymap"""

    def __setitem__(self, keys: str, value: Optional[KeymapValue]):
        raise NotImplementedError

    def __getitem__(self, keys: str) -> KeymapValue:
        raise NotImplementedError


class WindowInterface(metaclass=ABCMeta):
    pass


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

    def beginInput(self) -> None:
        raise NotImplementedError

    def setInput_FromString(self, s: str) -> None:
        raise NotImplementedError

    def endInput(self) -> None:
        raise NotImplementedError
