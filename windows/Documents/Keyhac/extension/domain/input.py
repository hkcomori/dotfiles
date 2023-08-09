from abc import abstractmethod
from typing import (
    Callable,
)

from .share import (
    Service,
)


class InputService(Service):
    @abstractmethod
    def send(self, *keys: str) -> Callable[[], None]:
        raise NotImplementedError
