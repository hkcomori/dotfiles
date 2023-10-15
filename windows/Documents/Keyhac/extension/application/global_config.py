from itertools import combinations, chain

from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class GlobalConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        global_keymap = self._keymap_service.from_query(WindowQuery())
        global_keymap.replace('MediaPrevTrack', 'BrowserSearch')
        global_keymap.replace('MediaPlay', 'BrowserSearch')
        global_keymap.replace('MediaStop', 'LaunchApp2')
        global_keymap.replace('VolumeMute', 'MediaPlay')

        global_keymap["Insert"] = act.nop()

        global_keymap["F19"] = act.activate_window() + act.trigger("F22")
        global_keymap["F22"] = act.close_window()
        mods = ['C', 'A', 'S', 'W', 'U0', 'U1', 'U2', 'U3']
        for n in range(1, len(mods) + 1):
            for c in combinations(mods, n):
                prefix = '-'.join(c) + '-'
                global_keymap[prefix + "F19"] = act.activate_window() + act.trigger(prefix + "F22")

        global_keymap["W-Insert"] = act.activate_window() + act.send("C-A", "C-C")

        global_keymap["W-L"] = act.turn_off_monitor()

        global_keymap["BackSlash"] = act.send("S-BackSlash")

        global_keymap["O-Kana"] = act.send('Kana')
        global_keymap["O-Henkan"] = act.send('Henkan')
        global_keymap["O-Muhenkan"] = act.send('Muhenkan')

        global_keymap["U2-Left"] = act.send("A-Left")
        global_keymap["U2-Right"] = act.send("A-Right")
        global_keymap["U2-Up"] = act.send("A-Up")
        global_keymap["U2-Down"] = act.send("A-Down")
        global_keymap["U2-B"] = act.send("C-Left")
        global_keymap["U2-F"] = act.send("C-Right")
        global_keymap["U2-N"] = act.send("Down", "Down", "Down", "Down", "Down")
        global_keymap["U2-P"] = act.send("Up", "Up", "Up", "Up", "Up")
        global_keymap["U2-Comma"] = act.send("C-Home")
        global_keymap["U2-Period"] = act.send("C-End")
        global_keymap["U2-W"] = act.send("C-C")

        global_keymap["C-A-I"] = act.send("C-A-Up")
        global_keymap["C-A-J"] = act.send("C-A-Left")
        global_keymap["C-A-K"] = act.send("C-A-Down")
        global_keymap["C-A-L"] = act.send("C-A-Right")

        # Emoji
        global_keymap[f"U3-Period"] = act.send(f"W-Period")

        mods = ['', 'C-', 'A-', 'S-']
        for comb in chain(*(
            combinations(mods, i)
            for i in range(len(mods)+1)
        )):
            m = ''.join(comb)
            global_keymap[f"U3-{m}I"] = act.send(f"{m}Up")
            global_keymap[f"U3-{m}J"] = act.send(f"{m}Left")
            global_keymap[f"U3-{m}K"] = act.send(f"{m}Down")
            global_keymap[f"U3-{m}L"] = act.send(f"{m}Right")

            global_keymap[f"U3-{m}H"] = act.send(f"{m}Home")
            global_keymap[f"U3-{m}Semicolon"] = act.send(f"{m}End")
            global_keymap[f"U3-{m}OpenBracket"] = act.send(f"{m}PageUp")
            global_keymap[f"U3-{m}CloseBracket"] = act.send(f"{m}PageDown")

            global_keymap[f"U3-{m}M"] = act.send(f"{m}Enter")

            global_keymap[f"U3-{m}P"] = act.send(f"{m}BackSpace")
            global_keymap[f"U3-{m}Colon"] = act.send(f"{m}Delete")

            global_keymap[f"U3-{m}Q"] = act.send(f"{m}Escape")
            global_keymap[f"U3-{m}A"] = act.send(f"{m}Apps")

            global_keymap[f"U3-{m}1"] = act.send(f"{m}F1")
            global_keymap[f"U3-{m}2"] = act.send(f"{m}F2")
            global_keymap[f"U3-{m}3"] = act.send(f"{m}F3")
            global_keymap[f"U3-{m}4"] = act.send(f"{m}F4")
            global_keymap[f"U3-{m}5"] = act.send(f"{m}F5")
            global_keymap[f"U3-{m}6"] = act.send(f"{m}F6")
            global_keymap[f"U3-{m}7"] = act.send(f"{m}F7")
            global_keymap[f"U3-{m}8"] = act.send(f"{m}F8")
            global_keymap[f"U3-{m}9"] = act.send(f"{m}F9")
            global_keymap[f"U3-{m}0"] = act.send(f"{m}F10")
            global_keymap[f"U3-{m}Minus"] = act.send(f"{m}F11")
            global_keymap[f"U3-{m}Tilde"] = act.send(f"{m}F12")

        mods = ['', 'A-', 'S-']
        for comb in chain(*(
            combinations(mods, i)
            for i in range(len(mods)+1)
        )):
            m = ''.join(comb)
            global_keymap[f"U3-{m}U"] = act.send(f"C-{m}Left")
            global_keymap[f"U3-{m}O"] = act.send(f"C-{m}Right")

        global_keymap["U3-F"] = act.send("C-A-Tab")
        global_keymap["U3-S-F"] = act.send("C-A-S-Tab")

        global_keymap["U3-S"] = act.send("W-C-Left")
        global_keymap["U3-D"] = act.send("W-C-Right")
        global_keymap["U3-W"] = act.send("W-A-Left")
        global_keymap["U3-E"] = act.send("W-A-Right")

        global_keymap["U3-F6"] = act.send('BrowserSearch')
        global_keymap["U3-F7"] = act.send('BrowserSearch')
        global_keymap["U3-F8"] = act.launch_obsidian()
        global_keymap["U3-F9"] = act.launch_calc()
        global_keymap["U3-F10"] = act.send('MediaPlay')
        global_keymap["U3-F11"] = act.send('VolumeDown')
        global_keymap["U3-F12"] = act.send('VolumeUp')

        global_keymap["U0-F6"] = act.send('BrowserSearch')
        global_keymap["U0-F7"] = act.send('BrowserSearch')
        global_keymap["U0-F8"] = act.launch_obsidian()
        global_keymap["U0-F9"] = act.launch_calc()
        global_keymap["U0-F10"] = act.send("MediaPlay")
        global_keymap["U0-F11"] = act.send("VolumeDown")
        global_keymap["U0-F12"] = act.send("VolumeUp")

        global_keymap["A-Tab"] = act.send("C-A-Tab")

        # Expand window
        global_keymap["W-S-Up"] = act.send("W-C-A-Up")
        global_keymap["W-S-Left"] = act.send("W-C-A-Left")
        global_keymap["W-S-Down"] = act.send("W-C-A-Down")
        global_keymap["W-S-Right"] = act.send("W-C-A-Right")
        global_keymap["U2-S-I"] = act.send("W-C-A-Up")
        global_keymap["U2-S-J"] = act.send("W-C-A-Left")
        global_keymap["U2-S-K"] = act.send("W-C-A-Down")
        global_keymap["U2-S-L"] = act.send("W-C-A-Right")

        # Disable minimize window
        global_keymap["W-C-Down"] = act.nop()

        # Move window to adjacent zone
        global_keymap["U2-I"] = act.send("W-Up")
        global_keymap["U2-J"] = act.send("W-Left")
        global_keymap["U2-K"] = act.send("W-Down")
        global_keymap["U2-L"] = act.send("W-Right")

        # Switch virtual desktop
        global_keymap["U2-Comma"] = act.send("W-C-Left")
        global_keymap["U2-Period"] = act.send("W-C-Right")

        # Switch window in current zone
        global_keymap["U2-OpenBracket"] = act.send("W-PageUp")
        global_keymap["U2-CloseBracket"] = act.send("W-PageDown")

        # global_keymap["W-Z"] = act.toggle_always_on_top
        global_keymap["W-D"] = act.open_documents()
        global_keymap["W-O"] = act.open_onedrive()

        global_keymap["W-F6"] = act.send('BrowserSearch')
        global_keymap["W-F7"] = act.send('BrowserSearch')
        global_keymap["W-F8"] = act.launch_obsidian()
        global_keymap["W-F9"] = act.launch_calc()
        global_keymap["MediaNextTrack"] = act.launch_obsidian()
        global_keymap["LaunchApp2"] = act.launch_calc()
