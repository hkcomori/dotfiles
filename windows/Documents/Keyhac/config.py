import os
import sys

current_directory = os.getcwd()
sys.path.append(os.path.join(current_directory, 'extension', 'vendor'))

from logging import basicConfig, getLogger, DEBUG    # noqa: E402

from extension.dependency import Dependency    # noqa: E402
from extension import (    # noqa: E402
    KeymapKeyhac,
)
from extension import (    # noqa: E402
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
