from .dependency import Dependency
from extension.domain.require_injection import (
    ShellService,
    InputService,
    WindowService,
    DesktopService,
)
from extension.domain.action import (
    Action,
    ReverseAction,
    DelayAction,
    NopAction,
    CommandAction,
    InputAction,
    ImeOnAction,
    ImeOffAction,
    WindowActivateAction,
    MonitorOffAction,
    ActionService,
)
from extension.domain.shell import (
    Command,
)


dependency = Dependency()
shell_service = dependency.resolve(ShellService)
input_service = dependency.resolve(InputService)
window_service = dependency.resolve(WindowService)
desktop_service = dependency.resolve(DesktopService)


class Count:
    def __init__(self) -> None:
        self.value = 0

    def inc(self):
        self.value += 1

    def dec(self):
        self.value -= 1


class IncAction(Action):
    def __init__(self, count: Count) -> None:
        super().__init__()
        self.count = count

    def __hash__(self) -> int:
        return hash(self.__class__)

    def perform(self) -> bool:
        self.count.inc()
        return True


class DecAction(Action):
    def __init__(self, count: Count) -> None:
        super().__init__()
        self.count = count

    def __hash__(self) -> int:
        return hash(self.__class__)

    def perform(self) -> bool:
        self.count.dec()
        return True


def test_action():
    count = Count()
    inc = IncAction(count)
    assert inc.perform() is True
    assert count.value == 1

    dec = DecAction(count)
    assert ReverseAction(dec).perform() is not True
    assert count.value == 0

    assert (inc + inc).value() is None
    assert count.value == 2

    (inc * 3).perform()
    assert count.value == 5

    (inc + dec * 2).perform()
    assert count.value == 4

    (inc + ReverseAction(inc) + inc).perform()
    assert count.value == 6

    inc2 = IncAction(count)
    assert inc == inc
    assert inc == inc2
    assert inc != dec
    assert inc is inc
    assert inc is not inc2
    assert inc is not dec


def test_delay():
    act = DelayAction(0.1, NopAction())
    act.perform()


def test_nop():
    act = NopAction()
    act.perform()


def test_command():
    command = Command('hoge')
    act = CommandAction(shell_service, command)
    act.perform()


def test_input():
    act = InputAction(input_service, 'C-A')
    act.perform()


def test_ime_on():
    act = ImeOnAction(window_service.from_active)
    act.perform()


def test_ime_off():
    act = ImeOffAction(window_service.from_active)
    act.perform()


def test_window_activate():
    act = WindowActivateAction(window_service.from_pointer)
    act.perform()


def test_monitor_off():
    act = MonitorOffAction(desktop_service)
    act.perform()


def test_action_service():
    action_service = ActionService(
        window_service,
        desktop_service,
        shell_service,
        input_service,
    )

    action_service.nop().perform()
    action_service.open_onedrive().perform()
    action_service.open_documents().perform()
    action_service.launch_obsidian().perform()
    action_service.launch_calc().perform()
    action_service.send('C-X', 'C-S').perform()
    action_service.ime_on().perform()
    action_service.ime_off().perform()
    action_service.activate_window().perform()
    action_service.turn_off_monitor().perform()
