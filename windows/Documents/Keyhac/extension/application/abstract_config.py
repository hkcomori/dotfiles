from abc import ABCMeta, abstractmethod

from injector import inject

from extension.domain.keymap import KeymapService
from extension.domain.action import ActionService


class AbstractConfig(metaclass=ABCMeta):
    @inject
    def __init__(
        self,
        keymap_service: KeymapService,
        action_service: ActionService,
    ):
        self._keymap_service = keymap_service
        self._action_service = action_service

    @abstractmethod
    def apply(self) -> None:
        raise NotImplementedError
