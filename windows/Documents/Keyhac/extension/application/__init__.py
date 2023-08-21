from extension.vendor.injector import inject

from extension.domain.action import ActionService
from extension.domain.keymap import KeymapService
from .abstract_config import AbstractConfig
from .global_config import GlobalConfig
from .explorer_config import ExplorerConfig
from .calculator_config import CalculatorConfig
from .browser_config import BrowserConfig
from .vscode_config import VScodeConfig
from .spark_config import SparkConfig
from .office_config import OfficeConfig
from .acrobat_config import AcrobatConfig
from .obsidian_config import ObsidianConfig
from .captureontouch_config import TouchConfig


class AllConfig(AbstractConfig):
    @inject
    def __init__(self, keymap_service: KeymapService, action_service: ActionService):
        self._configs = (
            GlobalConfig(keymap_service, action_service),
            ExplorerConfig(keymap_service, action_service),
            CalculatorConfig(keymap_service, action_service),
            BrowserConfig(keymap_service, action_service),
            VScodeConfig(keymap_service, action_service),
            SparkConfig(keymap_service, action_service),
            OfficeConfig(keymap_service, action_service),
            AcrobatConfig(keymap_service, action_service),
            ObsidianConfig(keymap_service, action_service),
            TouchConfig(keymap_service, action_service),
        )
        keymap_service.user_modifier0 = 'Apps'
        keymap_service.user_modifier1 = 'Kana'
        keymap_service.user_modifier2 = 'Henkan'
        keymap_service.user_modifier3 = 'Muhenkan'

    def apply(self) -> None:
        for c in self._configs:
            c.apply()
