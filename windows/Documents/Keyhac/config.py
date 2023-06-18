from logging import basicConfig, getLogger, DEBUG

from keyhac import *    # noqa: F403

from extension.key import KeymapConverter
import extension.global_keymap


basicConfig(
    level=DEBUG,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S',
)
logger = getLogger(__name__)


def configure(keymap):
    logger.info('load configure')
    keymap.defineModifier(KeymapConverter.convert('Apps'), "User0")
    keymap.defineModifier(KeymapConverter.convert('Kana'), "User1")
    keymap.defineModifier(KeymapConverter.convert('Henkan'), "User2")
    keymap.defineModifier(KeymapConverter.convert('Muhenkan'), "User3")
    extension.global_keymap.configure(keymap)
