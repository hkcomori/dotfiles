from abc import abstractmethod
from typing import (
    Callable,
)

from extension.vendor.injector import (
    inject,
)

from .share import (
    ValueObject,
    Repository,
)


class Action(ValueObject):
    """
    Action
    """

    def __init__(self, value: Callable[[], None]) -> None:
        self._value = value

    def __add__(self, other) -> 'Action':
        """Group two Actions"""
        if not isinstance(other, Action):
            raise TypeError(f'other: expect to Action, but {type(other).__name__}')
        return ActionSequence(self, other)

    def __mul__(self, times) -> 'Action':
        """Repeat Actions"""
        if not isinstance(times, int):
            raise TypeError(f'times: expect to int, but {type(times).__name__}')
        return ActionSequence(*([self] * times))

    @ValueObject.final
    def __call__(self) -> None:
        """Performs action"""
        self.perform()

    def perform(self) -> None:
        """Performs action"""
        self._value()


class ActionSequence(Action):
    """
    Provide an interface to group multiple Actions and operate them all at once
    """

    def __init__(self, *actions: 'Action'):
        self._actions = actions

    @Action.final
    def __hash__(self) -> int:
        return hash(self._actions)

    @Action.final
    def __add__(self, other) -> 'Action':
        """Group two Actions"""
        if isinstance(other, ActionSequence):
            return ActionSequence(*self._actions, *other._actions)
        if isinstance(other, Action):
            return ActionSequence(*self._actions, other)
        raise TypeError(f'other: expect to Action, but {type(other).__name__}')

    @Action.final
    def __mul__(self, times) -> 'Action':
        """Repeat Actions"""
        if not isinstance(times, int):
            raise TypeError(f'times: expect to int, but {type(times).__name__}')
        return ActionSequence(*([*self._actions] * times))

    @Action.final
    def perform(self):
        """Performs grouped Actions in order"""
        for action in self._actions:
            action.perform()


class KeymapRegistry(Repository):
    @abstractmethod
    def __setitem__(self, keys: str, action: Action):
        raise NotImplementedError

    @property
    @abstractmethod
    def applying_func(self):
        raise NotImplementedError

    @applying_func.setter
    def applying_func(self, action: Action):
        raise NotImplementedError


class KeymapRegistryGroup(KeymapRegistry):
    @inject
    def __init__(self, *regs: KeymapRegistry):
        self._regs = regs

    def __setitem__(self, keys: str, action: Action):
        for reg in self._regs:
            reg[keys] = action

    @property
    def applying_func(self):
        return self._regs[0].applying_func

    @applying_func.setter
    def applying_func(self, action: Action):
        for reg in self._regs:
            reg.applying_func = action
