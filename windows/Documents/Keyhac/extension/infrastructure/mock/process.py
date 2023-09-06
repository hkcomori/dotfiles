from typing import (
    Iterator,
)

from extension.domain.process import (
    Process,
    ProcessService,
)


class ProcessServiceMock(ProcessService):
    def __init__(self, *procs: Process):
        self._procs = procs

    def from_all(self) -> Iterator[Process]:
        for p in self._procs:
            yield p
