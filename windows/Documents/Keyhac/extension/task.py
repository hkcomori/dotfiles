from typing import Callable
from logging import getLogger

from keyhac import (
    JobItem,
    JobQueue,
)


logger = getLogger(__name__)


def task_finished(job_item):
    pass


def background_task(func: Callable[..., None]):
    def decorated_func(*args, **kw):
        def task(job_item):
            return func(*args, **kw)

        job_item = JobItem(task, task_finished)
        JobQueue.defaultQueue().enqueue(job_item)

    return decorated_func
