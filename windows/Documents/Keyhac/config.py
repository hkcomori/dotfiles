from logging import basicConfig, getLogger, DEBUG

from extension.dependency import Dependency
from extension import (
    KeymapKeyhac,
)
from extension import (
    AllConfig,
)


basicConfig(
    level=DEBUG,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S',
)
logger = getLogger(__name__)


def configure(keymap: KeymapKeyhac):
    config = Dependency(keymap).resolve(AllConfig)
    config.apply()
