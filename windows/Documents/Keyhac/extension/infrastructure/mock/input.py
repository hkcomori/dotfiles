from typing import (
    Callable,
)

from extension.domain.input import (
    Input,
    InputService,
)


class InputServiceMock(InputService):
    def send(self, *inputs: Input) -> Callable[[], None]:
        return lambda: None
