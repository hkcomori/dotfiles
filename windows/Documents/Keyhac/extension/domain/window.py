from abc import abstractmethod
from typing import (
    Optional,
)

from .share import (
    Entity,
    ValueObject,
    Factory,
)


class Window(Entity):
    def __init__(self, window_id: int):
        self._window_id = window_id

    def __hash__(self) -> int:
        return hash(self._window_id)

    @abstractmethod
    def activate(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def ime_on(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def ime_off(self) -> None:
        raise NotImplementedError


class WindowQuery(ValueObject):
    def __init__(
        self,
        exe_name: Optional[str] = None,
        class_name: Optional[str] = None,
        window_text: Optional[str] = None,
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
        return self._exe_name

    @property
    def class_name(self):
        return self._class_name

    @property
    def window_text(self):
        return self._window_text


class WindowFactory(Factory):
    @abstractmethod
    def from_id(cls, window_id: int) -> 'Window':
        raise NotImplementedError

    @abstractmethod
    def from_active(cls) -> 'Window':
        raise NotImplementedError

    @abstractmethod
    def from_pointer(cls) -> 'Window':
        raise NotImplementedError

    @abstractmethod
    def from_find(cls, query: WindowQuery) -> 'Window':
        raise NotImplementedError
