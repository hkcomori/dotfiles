from .keyhac_interface import (
    KeymapInterface,
)
from .keyhac_helper import KeymapEx
from . import cmd
from . import keymap_definitions


def init(_keymap: KeymapInterface):
    keymap = KeymapEx(_keymap)
    cmd.init(keymap)
    keymap_definitions.init(keymap)
