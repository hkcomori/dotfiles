import os
from typing import (
    Callable,
    Optional,
)

from extension.vendor.injector import (
    inject,
)

from .share import (
    ValueObject,
    Service,
)
from .exception import (
    DomainTypeError,
    WindowNotFoundError,
)
from .window import (
    WindowQuery,
    WindowService,
)
from .desktop import (
    DesktopService,
)
from .shell import (
    Command,
    ShellService,
)
from .input import (
    Input,
    InputService,
)


class Action(ValueObject):
    """
    Action
    """

    def __init__(self, value: Callable[[], None]) -> None:
        self._value = value

    def __hash__(self) -> int:
        return hash(self._value)

    def __add__(self, other) -> 'Action':
        """Group two Actions"""
        if not isinstance(other, Action):
            raise DomainTypeError(other, Action)
        return ActionSequence(self, other)

    def __mul__(self, times) -> 'Action':
        """Repeat Actions"""
        if not isinstance(times, int):
            raise DomainTypeError(times, int)
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
        raise DomainTypeError(other, Action)

    @Action.final
    def __mul__(self, times) -> 'Action':
        """Repeat Actions"""
        if not isinstance(times, int):
            raise DomainTypeError(times, int)
        return ActionSequence(*([*self._actions] * times))

    @Action.final
    def perform(self):
        """Performs grouped Actions in order"""
        for action in self._actions:
            action.perform()


class ActionService(Service):
    @inject
    def __init__(
        self,
        window_service: WindowService,
        desktop_service: DesktopService,
        shell_service: ShellService,
        input_service: InputService,
    ):
        self._window_service = window_service
        self._desktop_service = desktop_service
        self._shell_service = shell_service
        self._input_service = input_service

    def _launch_or_activate(
        self,
        process_path: str,
        query: Optional[WindowQuery] = None
    ):
        """kwargsに合致するウィンドウがないときにprocess_pathを起動する"""
        exe_name = os.path.basename(process_path)
        _query = WindowQuery(exe_name=exe_name) if query is None else query
        try:
            window = self._window_service.from_find(_query)
        except WindowNotFoundError:
            return self._shell_service.run(Command(process_path))
        else:
            return window.activate

    def open_onedrive(self) -> Action:
        """OneDriveフォルダを開く"""
        file = f'{os.getenv("OneDrive")}'
        return Action(self._shell_service.run(Command(file)))

    def open_documents(self) -> Action:
        """Documentsフォルダを開く"""
        file = os.popen(
            'powershell.exe -Command "([Environment])::GetFolderPath("""MyDocuments""")"'
        ).read().rstrip('\n').replace(os.sep, '/')
        return Action(self._shell_service.run(Command(file)))

    def launch_obsidian(self) -> Action:
        """Obsidianを開く"""
        file = f'{os.getenv("USERPROFILE")}/AppData/Local/Obsidian/Obsidian.exe'
        return Action(self._shell_service.run(Command(file)))

    def launch_calc(self) -> Action:
        """電卓を開く"""
        return Action(self._launch_or_activate(
            'calc.exe',
            WindowQuery(
                exe_name='ApplicationFrameHost.exe',
                window_text='電卓'
            ),
        ))

    def send(self, *keys: str) -> Action:
        """架空のキー入力を送信する"""
        inputs = Input.from_sequence(*keys)
        return Action(self._input_service.send(*inputs))

    def ime_on(self) -> Action:
        """IMEをONにする"""
        return Action(
            self._window_service.from_active().ime_on
        )

    def ime_off(self) -> Action:
        """IMEをOFFにする"""
        return Action(
            self._window_service.from_active().ime_off
        )

    def turn_off_monitor(self) -> Action:
        """モニターの電源を切る"""
        return Action(
            self._desktop_service.from_active().lock_on
        )
