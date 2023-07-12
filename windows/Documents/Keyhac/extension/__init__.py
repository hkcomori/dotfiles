from .keyhac_interface import (
    KeymapInterface,
)
from .keymap_helper import (
    KeymapEx,
)
from . import keymap_definitions


def init(_keymap: KeymapInterface):
    keymap = KeymapEx(_keymap)
    keymap_definitions.init(keymap)
