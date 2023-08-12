from abc import abstractmethod

from .share import (
    Repository,
    Service,
)
from .exception import (
    DomainTypeError,
)
from .action import (
    Action,
)
from .input import (
    Input,
)
from .window import (
    WindowQuery,
)


class KeymapRegistry(Repository):
    def __add__(self, other) -> 'KeymapRegistry':
        if not isinstance(other, KeymapRegistry):
            raise DomainTypeError(other, KeymapRegistry)
        return KeymapRegistryGroup(self, other)

    @abstractmethod
    def __setitem__(self, input: Input, action: Action):
        raise NotImplementedError

    @property
    @abstractmethod
    def applying_func(self):
        raise NotImplementedError

    @applying_func.setter
    def applying_func(self, action: Action):
        raise NotImplementedError


class KeymapRegistryGroup(KeymapRegistry):
    def __init__(self, *regs: KeymapRegistry):
        self._regs = regs

    def __add__(self, other) -> 'KeymapRegistry':
        if isinstance(other, KeymapRegistryGroup):
            return KeymapRegistryGroup(*self._regs, *other._regs)
        if isinstance(other, KeymapRegistry):
            return KeymapRegistryGroup(*self._regs, other)
        raise DomainTypeError(other, KeymapRegistry)

    def __setitem__(self, input: Input, action: Action):
        for reg in self._regs:
            reg[input] = action

    @property
    def applying_func(self):
        return self._regs[0].applying_func

    @applying_func.setter
    def applying_func(self, action: Action):
        for reg in self._regs:
            reg.applying_func = action


class KeymapService(Service):
    @abstractmethod
    def from_query(self, query: WindowQuery) -> KeymapRegistry:
        raise NotImplementedError
