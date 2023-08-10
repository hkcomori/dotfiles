from abc import abstractmethod

from .share import (
    Entity,
    ValueObject,
    Service,
)


class WindowId(ValueObject):
    def __init__(self, value: int):
        self._value = value

    def __hash__(self) -> int:
        return hash(self.value)

    @property
    def value(self) -> int:
        return self._value


class WindowQuery(ValueObject):
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
        return self._exe_name

    @property
    def class_name(self):
        return self._class_name

    @property
    def window_text(self):
        return self._window_text


class Window(Entity):
    def __init__(self, window_id: WindowId):
        self._window_id = window_id

    def __hash__(self) -> int:
        return hash(self._window_id)

    @property
    def window_id(self) -> WindowId:
        return self._window_id

    @abstractmethod
    def activate(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def ime_on(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def ime_off(self) -> None:
        raise NotImplementedError


class WindowService(Service):
    @abstractmethod
    def from_id(self, window_id: int) -> 'Window':
        raise NotImplementedError

    @abstractmethod
    def from_active(self) -> 'Window':
        raise NotImplementedError

    @abstractmethod
    def from_pointer(self) -> 'Window':
        raise NotImplementedError

    @abstractmethod
    def from_find(self, query: WindowQuery) -> 'Window':
        raise NotImplementedError
