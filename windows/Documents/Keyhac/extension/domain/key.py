from typing import (
    Collection,
)

from .share import ValueObject


class KeyInput(ValueObject):
    def __init__(self, mod_keys: Collection[str], main_key: str):
        if main_key not in self.__class__.SUPPORTED_MAIN_KEYS:
            raise ValueError(f'Invalid main key: {main_key}')

        for k in mod_keys:
            if k not in self.__class__.SUPPORTED_MOD_KEYS:
                raise ValueError(f'Invalid modifier key: {k}')

        self._mod_keys = mod_keys
        self._main_key = main_key

    def __hash__(self) -> int:
        return hash((self._mod_keys, self._main_key))

    @classmethod
    def create(cls, keys: str) -> 'KeyInput':
        splitted = tuple(keys.split('-'))
        tokens = splitted if splitted[-1:] != '' else (*splitted[:-2], '-')
        if tokens[0] == 'O':
            mod_keys, main_key = tokens[1:-1], tokens[-1]
        elif tokens[0] in {'D', 'U'}:
            mod_keys, main_key = tokens[1:], tokens[-1]
        else:
            mod_keys, main_key = tokens[1:], tokens[-1]

        return cls(mod_keys, main_key)

    SUPPORTED_MOD_KEYS: Collection[str] = {
        'C', 'A', 'S', 'W',                     # System modifier
        'Apps', 'Kana', 'Henkan', 'Muhenkan',   # User modifier
        '*',                                    # Any modifier
    }
    SUPPORTED_MAIN_KEYS: Collection[str] = {
        # Alphabet
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        # Number
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        # Punctuation
        '-', '^', 'Yen', '@', '[', ']', ';', ':', ',', '.', '/', '\\',
        # Function
        'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10',
        'F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F17', 'F18', 'F19',
        'F20', 'F21', 'F22', 'F23', 'F24',
        # Not printable
        'Esc', 'Tab', 'CapsLock', 'BackSpace', 'Enter',
        'Insert', 'Delete', 'Home', 'End', 'PageUp', 'PageDown',
        'Left', 'Right', 'Up', 'Down',
        # Mouse
        'LButton', 'RButton', 'MButton', 'XButton1', 'XButton2',
        'WheelUp', 'WheelDown',
        # Multimedia
        'Play', 'VolumeMute', 'VolumeDown', 'VolumeUp',
        'LaunchApp1', 'LaunchApp2',
        # Any main key
        '*',
    }
