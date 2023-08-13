from abc import abstractmethod
import os
from typing import (
    Callable,
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
    Window,
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

    @property
    def value(self) -> Callable[[], None]:
        """Performs action without return value"""
        def _value():
            self.perform()

        return _value

    @abstractmethod
    def perform(self) -> bool:
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
        raise DomainTypeError(other, Action)

    @Action.final
    def __mul__(self, times) -> 'Action':
        """Repeat Actions"""
        if not isinstance(times, int):
            raise DomainTypeError(times, int)
        return ActionSequence(*([*self._actions] * times))

    @Action.final
    def perform(self) -> bool:
        """Performs grouped Actions in order"""
        for action in self._actions:
            if not action.perform():
                return False
        return True


class ReverseAction(Action):
    """
    アクションの成否を反転する。
    """
    def __init__(self, action: 'Action'):
        self._action = action

    @Action.final
    def __hash__(self) -> int:
        return hash(self._action)

    @Action.final
    def perform(self) -> bool:
        return not self._action.perform()


class NopAction(Action):
    """何もしない"""
    def __hash__(self) -> int:
        return hash(self.__class__)

    def perform(self) -> bool:
        return True


class CommandAction(Action):
    """コマンドを実行する"""
    def __init__(self, shell_service: ShellService, command: Command):
        self._shell_service = shell_service
        self._command = command

    def __hash__(self) -> int:
        return hash(self._shell_service)

    def perform(self) -> bool:
        self._shell_service.run(self._command)()
        return True


class InputAction(Action):
    """架空の入力を実行する"""
    def __init__(self, input_service: InputService, *keys: str):
        self._input_service = input_service
        self._inputs = Input.from_sequence(*keys)

    def __hash__(self) -> int:
        return hash(self._input_service)

    def perform(self) -> bool:
        self._input_service.send(*self._inputs)()
        return True


class ImeOnAction(Action):
    """IMEをONにする"""
    def __init__(self, get_window_func: Callable[[], Window]):
        self.get_window_func = get_window_func

    def __hash__(self) -> int:
        return hash(self.get_window_func)

    def perform(self) -> bool:
        return self.get_window_func().ime_on()


class ImeOffAction(Action):
    """IMEをOFFにする"""
    def __init__(self, get_window_func: Callable[[], Window]):
        self.get_window_func = get_window_func

    def __hash__(self) -> int:
        return hash(self.get_window_func)

    def perform(self) -> bool:
        return self.get_window_func().ime_off()


class WindowActivateAction(Action):
    """ウィンドウをアクティブにする"""
    def __init__(self, get_window_func: Callable[[], Window]):
        self.get_window_func = get_window_func

    def __hash__(self) -> int:
        return hash(self.get_window_func)

    def perform(self) -> bool:
        try:
            wnd = self.get_window_func()
        except WindowNotFoundError:
            return False
        else:
            return wnd.activate()


class MonitorOffAction(Action):
    """モニターをOFFにする"""
    @inject
    def __init__(self, desktop_service: DesktopService) -> None:
        self._desktop_service = desktop_service

    def __hash__(self) -> int:
        return hash(self._desktop_service)

    def perform(self) -> bool:
        desktop = self._desktop_service.from_active()
        return desktop.lock_on()


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

    @staticmethod
    def _get_special_folder(name: str) -> str:
        """特殊フォルダーパスを取得する"""
        cmd = f'powershell.exe -Command "([Environment])::GetFolderPath("""{name}""")"'
        folder = os.popen(cmd).read().rstrip('\n').replace(os.sep, '/')
        return folder

    def nop(self) -> Action:
        return NopAction()

    def open_onedrive(self) -> Action:
        """OneDriveフォルダを開く"""
        file = f'{os.getenv("OneDrive")}'
        return CommandAction(self._shell_service, Command(file))

    def open_documents(self) -> Action:
        """Documentsフォルダを開く"""
        file = self._get_special_folder('MyDocuments')
        return CommandAction(self._shell_service, Command(file))

    def launch_obsidian(self) -> Action:
        """Obsidianを開く"""
        file = f'{os.getenv("USERPROFILE")}/AppData/Local/Obsidian/Obsidian.exe'
        return CommandAction(self._shell_service, Command(file))

    def launch_calc(self) -> Action:
        """電卓を開く"""
        return ReverseAction(WindowActivateAction(
            lambda: self._window_service.from_query(WindowQuery(
                exe_name='ApplicationFrameHost.exe',
                window_text='電卓'
            )),
        )) + CommandAction(self._shell_service, Command('calc.exe'))

    def send(self, *keys: str) -> Action:
        """架空のキー入力を送信する"""
        return InputAction(self._input_service, *keys)

    def ime_on(self) -> Action:
        """IMEをONにする"""
        return ImeOnAction(self._window_service.from_active)

    def ime_off(self) -> Action:
        """IMEをOFFにする"""
        return ImeOffAction(self._window_service.from_active)

    def activate_window(self) -> Action:
        """マウスカーソル下のウィンドウをアクティブにする"""
        return WindowActivateAction(self._window_service.from_pointer)

    def turn_off_monitor(self) -> Action:
        """モニターの電源を切る"""
        return MonitorOffAction(self._desktop_service)
