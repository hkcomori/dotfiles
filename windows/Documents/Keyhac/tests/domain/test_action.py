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


def test_action():
    count = Count()
    inc = Action(count.inc)
    inc.perform()
    assert count.value == 1

    dec = Action(count.dec)
    dec.perform()
    assert count.value == 0

    (inc + inc).perform()
    assert count.value == 2

    (inc * 3).perform()
    assert count.value == 5

    (inc + dec * 2).perform()
    assert count.value == 4

    inc2 = Action(count.inc)
    assert inc == inc
    assert inc == inc2
    assert inc != dec
    assert inc is inc
    assert inc is not inc2
    assert inc is not dec
