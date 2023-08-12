from extension.domain.action import (
    Action,
)


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
    inc.perform()
    assert count.value == 1

    dec = DecAction(count)
    dec.perform()
    assert count.value == 0

    (inc + inc).perform()
    assert count.value == 2

    (inc * 3).perform()
    assert count.value == 5

    (inc + dec * 2).perform()
    assert count.value == 4

    inc2 = IncAction(count)
    assert inc == inc
    assert inc == inc2
    assert inc != dec
    assert inc is inc
    assert inc is not inc2
    assert inc is not dec
