from typing import (
    Callable,
)

from extension.vendor.injector import (
    inject,
)

from extension.domain.input import (
    InputService,
)
from .share import (
    KeymapKeyhac,
)


class InputServiceKeyhac(InputService):
    @inject
    def __init__(self, keymap: KeymapKeyhac) -> None:
        self._keymap = keymap

    def send(self, *keys: str) -> Callable[[], None]:
        return self._keymap.InputKeyCommand(*keys)
