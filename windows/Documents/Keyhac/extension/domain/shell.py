from abc import abstractmethod
from typing import (
    Callable,
)

from .share import (
    ValueObject,
    Service,
)


class Command(ValueObject):
    def __init__(
        self,
        file: str,
        param: str = '',
        working_directory: str = '',
    ):
        self._file = file
        self._param = param
        self._working_directory = working_directory

    def __hash__(self) -> int:
        return hash((
            self._file,
            self._param,
            self._working_directory,
        ))

    @property
    def file(self) -> str:
        return self._file

    @property
    def param(self) -> str:
        return self._param

    @property
    def working_directory(self) -> str:
        return self._working_directory


class ShellService(Service):
    @abstractmethod
    def run(self, command: Command) -> Callable[[], None]:
        raise NotImplementedError
