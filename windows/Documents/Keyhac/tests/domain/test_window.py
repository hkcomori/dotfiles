from extension.domain.window import (
    WindowId,
    WindowQuery,
)


def test_window_id():
    window_id1 = WindowId(3)
    window_id2 = WindowId(3)
    window_id3 = WindowId(5)

    assert window_id3.value == 5

    assert window_id1 == window_id1
    assert window_id1 == window_id2
    assert window_id1 != window_id3
    assert window_id1 is window_id1
    assert window_id1 is not window_id2
    assert window_id1 is not window_id3


def test_window_query():
    query1 = WindowQuery(
        'EXE NAME',
        'class NAME',
        'WindowText',
    )
    query2 = WindowQuery(
        'EXE NAME',
        'class2 NAME',
        'WindowText',
    )
    query3 = WindowQuery(
        'EXE NAME',
        'class NAME',
        'WindowText',
    )
    assert query1.exe_name == 'EXE NAME'
    assert query1.class_name == 'class NAME'
    assert query1.window_text == 'WindowText'

    assert query1 == query1
    assert query1 == query3
    assert query1 != query2
    assert query1 is query1
    assert query1 is not query3
    assert query1 is not query2
