from logging import basicConfig, getLogger, DEBUG

# from keyhac import *    # noqa: F403

from extension import init


basicConfig(
    level=DEBUG,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S',
)
logger = getLogger(__name__)


def configure(keymap):
    init(keymap)
