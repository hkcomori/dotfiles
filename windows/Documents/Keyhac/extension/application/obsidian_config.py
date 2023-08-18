from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class ObsidianConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        obsidian = self._keymap_service.from_query(WindowQuery(
            exe_name='Obsidian.exe',
        ))

        obsidian['F22'] = act.send('C-W')

        obsidian['C-E'] = act.send('C-E') + act.ime_off()
        obsidian['C-F'] = act.send('C-F') + act.ime_off()
        obsidian['C-H'] = act.send('C-H') + act.ime_off()
        obsidian['C-O'] = act.send('C-O') + act.ime_off()
        obsidian['C-S-F'] = act.send('C-S-F') + act.ime_off()
        obsidian['C-S-P'] = act.send('C-S-P') + act.ime_off()
