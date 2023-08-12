from abc import abstractmethod
from typing import (
    Callable,
    Sequence,
)

from .share import (
    ValueObject,
    Service,
)


class Input(ValueObject):
    def __init__(self, value: str):
        self._value = value

    @property
    def value(self) -> str:
        return self._value

    @classmethod
    def from_sequence(cls, *values: str) -> Sequence['Input']:
        return tuple((cls(s) for s in values))


class InputService(Service):
    @abstractmethod
    def send(self, *inputs: Input) -> Callable[[], None]:
        raise NotImplementedError
