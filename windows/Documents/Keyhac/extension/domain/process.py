from abc import abstractmethod
import os
import errno
from fnmatch import fnmatch
from typing import (
    Final,
    Iterator,
    Optional,
    Sequence,
)

from .share import (
    ValueObject,
    Service,
)
from .exception import (
    DomainRuntimeError,
)


class Process(ValueObject):
    """
    プロセスに対応したオブジェクト

    @param pid      プロセスID
    @param path     実行ファイルのパス
    @param params   パラメータ
    """
    def __init__(self, pid: int, path: str, params: Sequence[str]):
        self._pid: Final = pid
        self._path: Final = path
        self._params: Final = params

    def __hash__(self) -> int:
        return hash((self.pid, self.path, self.params))

    def __repr__(self) -> str:
        return f"Process({self.pid}, '{self.path}', {self.params})"

    def __str__(self) -> str:
        return f"({self.pid}, '{self.path}', {self.params})"

    @property
    def pid(self) -> int:
        return self._pid

    @property
    def path(self) -> str:
        return self._path

    @property
    def params(self) -> Sequence[str]:
        return self._params


class ProcessQuery(ValueObject):
    """
    Processを検索する条件

    @param pid      プロセスID(完全一致)
    @param path     実行ファイルのパス(ワイルドカードが使用できます)
    @param params   パラメータ(各パラメータ内でワイルドカードが使用できます)
    """
    def __init__(
        self,
        pid: Optional[int] = None,
        path: str = '',
        params: Optional[Sequence[str]] = None,
    ):
        self._pid: Final = pid
        self._path: Final = path
        self._params: Final = params

    def __hash__(self) -> int:
        return hash((self.pid, self.path, self.params))

    def __repr__(self) -> str:
        return f"ProcessQuery({self.pid}, '{self.path}', {self.params})"

    def __str__(self) -> str:
        return f"({self.pid}, '{self.path}', {self.params})"

    @property
    def pid(self) -> Optional[int]:
        return self._pid

    @property
    def path(self) -> str:
        return self._path

    @property
    def params(self) -> Optional[Sequence[str]]:
        return self._params

    def match(self, proc: Process) -> bool:
        """
        Processが条件に合致するかを判定する

        @return True: 合致する, False: 合致しない
        """
        return all((
            self.pid is None or self.pid == proc.pid,
            self.path == '' or fnmatch(proc.path, self.path),
            self.params is None or (
                len(self.params) == len(proc.params)
                and all((fnmatch(val, pat) for val, pat in zip(proc.params, self.params)))  # noqa: W503
            ),
        ))


class ProcessService(Service):
    """
    Process関連の操作を提供する
    """
    def from_pid(self, pid: int) -> Process:
        """
        プロセスIDを指定してProcessを取得する

        @param pid プロセスID
        """
        proc = tuple(
            p for p in self.from_all()
            if p.pid == pid
        )
        if len(proc) != 1:
            raise DomainRuntimeError(f'Process not found: pid={pid}')
        return proc[0]

    def from_query(
        self,
        include: ProcessQuery,
        exclude: Optional[ProcessQuery],
    ) -> Sequence[Process]:
        """
        条件に合致するProcessをすべて取得する

        @param include 取得したい条件
        @param exclude 除外したい条件
        """
        return tuple(
            p for p in self.from_all()
            if include.match(p) and (exclude is None or not exclude.match(p))
        )

    @abstractmethod
    def from_all(self) -> Iterator[Process]:
        raise NotImplementedError

    def terminate(self, proc: Process):
        """
        指定したプロセスを終了する
        """
        try:
            os.kill(proc.pid, Signals.SIGTERM)
        except OSError as e:
            # 指定したプロセスが既に存在しない場合は問題ないので、
            # それ以外のエラーだけをraiseする
            if e.errno != errno.ESRCH:
                raise e from None

