from typing import (
    Callable,
)

from extension.domain.input import (
    InputService,
)


class InputServiceMock(InputService):
    def send(self, *keys: str) -> Callable[[], None]:
        return lambda: None
