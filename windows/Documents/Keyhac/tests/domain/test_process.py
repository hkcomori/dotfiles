import pytest

from extension.domain.process import (
    Process,
    ProcessQuery,
    ProcessService,
)
from extension.infrastructure.mock.process import (
    ProcessServiceMock,
)


class Test_Process:
    def test_repr_and_str(self):
        proc = Process(1, '/path/to/exe', ('param1', 'param2'))

        assert repr(proc) == "Process(1, '/path/to/exe', ('param1', 'param2'))"
        assert str(proc) == "(1, '/path/to/exe', ('param1', 'param2'))"


class Test_ProcessQuery:
    def test_repr_and_str(self):
        query = ProcessQuery(1, '/path/to/exe', ('param1', 'param2'))

        assert repr(query) == "ProcessQuery(1, '/path/to/exe', ('param1', 'param2'))"
        assert str(query) == "(1, '/path/to/exe', ('param1', 'param2'))"

    @pytest.mark.parametrize((
        'args',         # 引数全部指定
        'args_wild',    # ワイルドカードを使用して一致するクエリ
        'args_ne',      # 一致しないクエリ
    ), [(   # 不一致理由: pid
        (1, '/path/to/exe', ('param1', 'param2')),
        (1, '', None),
        (2, '/path/to/exe', ('param1', 'param2')),
    ), (    # 不一致理由: exe
        (1, '/path/to/exe1', ('param1', 'param2')),
        (None, '/path/*/ex?1', None),
        (1, '/path/to/exe2', ('param1', 'param2')),
    ), (    # 不一致理由: cmdline
        (1, '/path/to/exe', ('param1', 'param2')),
        (None, '', ('param1', '*2')),
        (1, '/path/to/exe', ('param1', '*3')),
    )])
    def test_ProcessQuery_match(self, args, args_wild, args_ne):
        query = ProcessQuery(*args)
        query_wild = ProcessQuery(*args_wild)
        proc_eq = Process(*args)
        proc_ne = Process(*args_ne)

        assert query.match(proc_eq) is True
        assert query_wild.match(proc_eq) is True
        assert query.match(proc_ne) is not True


class Test_ProcessService:
    def process_service(self) -> ProcessService:
        return ProcessServiceMock(
            Process(1, '/path/to/exe1', tuple()),
            Process(2, '/path/to/exe1', ('param1',)),
            Process(3, '/path/to/exe1', ('param1', 'param2')),
            Process(4, '/path/to/exe2', tuple()),
            Process(5, '/path/to/exe2', ('param1',)),
            Process(6, '/path/to/exe2', ('param1', 'param2')),
            Process(7, '/path/to/exe2', ('param1', 'param2', 'param3')),
        )

    def test_from_all(self):
        proc_service = self.process_service()
        assert tuple(proc_service.from_all()) == (
            Process(1, '/path/to/exe1', tuple()),
            Process(2, '/path/to/exe1', ('param1',)),
            Process(3, '/path/to/exe1', ('param1', 'param2')),
            Process(4, '/path/to/exe2', tuple()),
            Process(5, '/path/to/exe2', ('param1',)),
            Process(6, '/path/to/exe2', ('param1', 'param2')),
            Process(7, '/path/to/exe2', ('param1', 'param2', 'param3')),
        )

    @pytest.mark.parametrize((
        'pid',          # プロセスID
        'expected',     # 期待結果
    ), [(
        1,
        Process(1, '/path/to/exe1', tuple()),
    ), (
        7,
        Process(7, '/path/to/exe2', ('param1', 'param2', 'param3')),
    )])
    def test_from_pid(self, pid, expected):
        proc_service = self.process_service()
        assert proc_service.from_pid(pid) == expected

    @pytest.mark.parametrize((
        'include',      # 一致条件
        'exclude',      # 除外条件
        'expected',     # 期待結果
    ), [(
        ProcessQuery(pid=1),
        None,
        (
            Process(1, '/path/to/exe1', tuple()),
        ),
    ), (
        ProcessQuery(path='/path/*/ex?1'),
        ProcessQuery(params=('*', 'param2')),
        (
            Process(1, '/path/to/exe1', tuple()),
            Process(2, '/path/to/exe1', ('param1',)),
        ),
    ), (
        ProcessQuery(params=('param1', '*2')),
        None,
        (
            Process(3, '/path/to/exe1', ('param1', 'param2')),
            Process(6, '/path/to/exe2', ('param1', 'param2')),
        ),
    )])
    def test_from_query(self, include, exclude, expected):
        proc_service = self.process_service()
        assert proc_service.from_query(include, exclude) == expected
