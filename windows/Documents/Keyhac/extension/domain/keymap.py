from abc import abstractmethod

from .share import (
    ValueObject,
    Repository,
)
from .key import KeyInput
from .action import Action


class Keymap(ValueObject):
    """
    Keymap
    """
    def __init__(self, keys: str, action: Action):
        self.keys = tuple(KeyInput.create(k) for k in keys.split(' '))
        self.action = action


class KeymapRegistry(Repository):
    @abstractmethod
    def assign(self, *keymap: Keymap):
        raise NotImplementedError

    @abstractmethod
    def registerOnActive(self, action: Action):
        raise NotImplementedError


class KeymapRegistryGroup(KeymapRegistry):
    def __init__(self, *repos: KeymapRegistry):
        self._repos = repos

    def assign(self, *keymap: Keymap):
        for repo in self._repos:
            repo.assign(*keymap)

    def registerOnActive(self, action: Action):
        for repo in self._repos:
            repo.registerOnActive(action)
