import functools
from logging import getLogger
import time


logger = getLogger(__name__)


def profile(func):
    timer = time.perf_counter

    @functools.wraps(func)
    def _profile(*args, **kw):
        begin_sec = timer()
        ret = func(*args, **kw)
        elapsed_ms = 1000 * (timer() - begin_sec)
        if elapsed_ms > 99:
            logger.debug(f'{func.__name__}: {elapsed_ms:.3f} [ms] elapsed')
        return ret

    return _profile
