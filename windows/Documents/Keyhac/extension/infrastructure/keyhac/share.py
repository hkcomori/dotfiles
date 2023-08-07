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


class KeymapKeyhac(metaclass=ABCMeta):
    """
    class Keymap in keyhac
    https://crftwr.github.io/keyhac/doc/ja/classkeyhac__keymap_1_1_keymap.html
    """
    @abstractmethod
    def defineWindowKeymap(
        self,
        exe_name: Optional[str] = None,
        class_name: Optional[str] = None,
        window_text: Optional[str] = None,
    ) -> 'WindowKeymapKeyhac':
        raise NotImplementedError


class WindowKeymapKeyhac(metaclass=ABCMeta):
    """
    class WindowKeymap in keyhac
    https://crftwr.github.io/keyhac/doc/ja/classkeyhac__keymap_1_1_window_keymap.html
    """
    @abstractmethod
    def __setitem__(
        self,
        key: str,
        action: Union[str, Sequence[str], Callable[[], None]],
    ) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def applying_func(self):
        raise NotImplementedError

    @applying_func.setter
    def applying_func(self, func: Callable[[], None]):
        raise NotImplementedError
