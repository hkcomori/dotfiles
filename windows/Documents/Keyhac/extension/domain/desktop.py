from abc import abstractmethod

from .share import (
    Entity,
    Factory,
)


class Desktop(Entity):
    def __eq__(self, other) -> bool:
        if not isinstance(other, Desktop):
            return False
        return self.__class__ == other.__class__

    def __hash__(self) -> int:
        return hash(self.__class__)

    @abstractmethod
    def lock_on(self) -> None:
        raise NotImplementedError


class DesktopFactory(Factory):
    @abstractmethod
    def from_active(self) -> Desktop:
        raise NotImplementedError
