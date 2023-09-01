from abc import ABCMeta, abstractmethod
from typing import (
    Type,
    TypeVar,
)

from injector import (
    Injector,
    Binder,
)

from .keymap import (   # noqa: F401
    KeymapRegistry,
    KeymapService,
)
from .window import (   # noqa: F401
    Window,
    WindowService,
)
from .desktop import (  # noqa: F401
    Desktop,
    DesktopService,
)
from .shell import (    # noqa: F401
    ShellService,
)
from .input import (    # noqa: F401
    InputService,
)


T1 = TypeVar('T1')


class AbstractDependency(metaclass=ABCMeta):
    """依存性の注入を定義するクラス"""

    @abstractmethod
    def config(self, binder: Binder) -> None:
        """
        依存性を注入する抽象クラスと具象クラスの対応付けを定義する

        eg.
        - binder.bind(AbstractClass, to=ConcreteClass)
        - binder.bind(AbstractClass, to=InstanceProvider(instance))
        """
        raise NotImplementedError

    def resolve(self, class_: Type[T1]) -> T1:
        injector = Injector(self.config)
        return injector.get(class_)
