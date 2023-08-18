from typing import (
    Callable,
)

from extension.vendor.injector import (
    inject,
)

from extension.domain.input import (
    Input,
    InputService,
)
from .share import (
    KeymapKeyhac,
)


class InputServiceKeyhac(InputService):
    @inject
    def __init__(self, keymap: KeymapKeyhac) -> None:
        self._keymap = keymap

    def send(self, *inputs: Input) -> Callable[[], None]:
        keys = [i.value for i in inputs]
        return self._keymap.InputKeyCommand(*keys)

    def trigger(self, input: Input) -> Callable[[], None]:
        key = input.value
        return self._keymap.TriggerActionCommand(key)
