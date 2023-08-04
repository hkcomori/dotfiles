from abc import abstractmethod

from .share import ValueObject
from .key import KeyInput
from .window import WindowFactory


class Action(ValueObject):
    """
    [abstract] Action
    """
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

    @abstractmethod
    def perform(self) -> None:
        """Performs action"""
        raise NotImplementedError


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


class OpenDirectoryAction(Action):
    """
    [abstract] Open the directory
    """
    @Action.final
    def __init__(self, directory: str):
        self._directory = directory

    @Action.final
    def __hash__(self) -> int:
        return hash(self._directory)


class LaunchAppAction(Action):
    """
    [abstract] Launch the application
    """
    @Action.final
    def __init__(self, command: str, working_directory: str):
        self._command = command
        self._working_directory = working_directory

    @Action.final
    def __hash__(self) -> int:
        return hash((self._command, self._working_directory))


class ImeOnAction(Action):
    """
    Enable IME for active window
    """
    @Action.final
    def __init__(self, window_factory: WindowFactory):
        self._window_factory = window_factory

    @Action.final
    def __hash__(self) -> int:
        return id(self._window_factory)

    @Action.final
    def perform(self):
        window = self._window_factory.from_active()
        window.ime_on()


class ImeOffAction(Action):
    """
    Disable IME for active window
    """
    @Action.final
    def __init__(self, window_factory: WindowFactory):
        self._window_factory = window_factory

    @Action.final
    def __hash__(self) -> int:
        return id(self._window_factory)

    @Action.final
    def perform(self):
        window = self._window_factory.from_active()
        window.ime_off()


class SendKeyAction(Action):
    """
    [abstract] Send key sequence
    """
    @Action.final
    def __init__(self, *keys: KeyInput):
        self._keys = keys

    @Action.final
    def __hash__(self) -> int:
        return hash(self._keys)


class WindowActivateAction(Action):
    """
    Active window on mouse pointer
    """
    @Action.final
    def __init__(self, window_factory: WindowFactory):
        self._window_factory = window_factory

    @Action.final
    def __hash__(self) -> int:
        return id(self._window_factory)

    @Action.final
    def perform(self):
        window = self._window_factory.from_pointer()
        window.activate()
