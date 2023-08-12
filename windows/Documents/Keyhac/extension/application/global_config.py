from extension.domain.window import WindowQuery
from .abstract_config import AbstractConfig


class GlobalConfig(AbstractConfig):
    def apply(self) -> None:
        act = self._action_service

        global_keymap = self._keymap_service.from_query(WindowQuery())

        global_keymap["Insert"] = act.nop()

        global_keymap["W-Insert"] = act.activate_window() + act.send("C-A", "C-C")

        global_keymap["W-L"] = act.turn_off_monitor()

        global_keymap["BackSlash"] = act.send("S-BackSlash")

        global_keymap["O-Kana"] = act.ime_on()
        global_keymap["O-Henkan"] = act.ime_on()
        global_keymap["O-Muhenkan"] = act.ime_off()

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
        global_keymap["U2-V"] = act.send("PageUp")
        global_keymap["U3-B"] = act.send("Left")
        global_keymap["U3-P"] = act.send("Up")
        global_keymap["U3-N"] = act.send("Down")
        global_keymap["U3-F"] = act.send("Right")
        global_keymap["U3-A"] = act.send("Home")
        global_keymap["U3-E"] = act.send("End")
        global_keymap["U3-V"] = act.send("PageDown")
        global_keymap["U3-M"] = act.send("Enter")
        global_keymap["U3-H"] = act.send("BackSpace")
        global_keymap["U3-D"] = act.send("Delete")
        global_keymap["U3-S"] = act.send("C-F")
        global_keymap["U3-Y"] = act.send("C-V")
        global_keymap["U3-OpenBracket"] = act.send("Esc")
        global_keymap["U3-Slash"] = act.send("C-Z")
        global_keymap["U3-Underscore"] = act.send("C-Y")
        global_keymap["U3-Left"] = act.send("C-Left")
        global_keymap["U3-Right"] = act.send("C-Right")
        global_keymap["U3-Up"] = act.send("Up", "Up", "Up", "Up", "Up")
        global_keymap["U3-Down"] = act.send("Down", "Down", "Down", "Down", "Down")
        global_keymap["U3-PageUp"] = act.send("C-PageUp")
        global_keymap["U3-PageDown"] = act.send("C-PageDown")
        global_keymap["U3-Atmark"] = act.send("C-Atmark")
        global_keymap["U3-BackSpace"] = act.send("C-BackSpace")
        global_keymap["U3-Delete"] = act.send("C-Delete")

        global_keymap["A-Tab"] = act.send("C-A-Tab")

        # Expand window
        global_keymap["W-S-Left"] = act.send("W-C-A-Left")
        global_keymap["W-S-Right"] = act.send("W-C-A-Right")
        global_keymap["W-S-Down"] = act.send("W-C-A-Down")

        # Vertical maximize window
        global_keymap["W-S-Up"] = act.send("W-S-Up")

        # Disable minimize window
        global_keymap["W-C-Down"] = act.nop()

        # Switch virtual desktop
        global_keymap["W-Home"] = act.send("W-C-Left")
        global_keymap["W-End"] = act.send("W-C-Right")
        global_keymap["U0-Home"] = act.send("W-C-Left")
        global_keymap["U0-End"] = act.send("W-C-Right")
        global_keymap["U0-XButton1"] = act.send("W-C-Left")
        global_keymap["U0-XButton2"] = act.send("W-C-Right")

        # Switch window in current zone
        global_keymap["U0-PageUp"] = act.send("W-PageUp")
        global_keymap["U0-PageDown"] = act.send("W-PageDown")

        global_keymap["U0-F1"] = act.send("MediaPlay")
        global_keymap["U0-F2"] = act.send("VolumeMute")
        global_keymap["U0-F3"] = act.send("VolumeDown")
        global_keymap["U0-F4"] = act.send("VolumeUp")

        # global_keymap["W-Z"] = act.toggle_always_on_top
        global_keymap["W-D"] = act.open_documents()
        global_keymap["W-O"] = act.open_onedrive()

        global_keymap["W-F11"] = act.launch_obsidian()
        global_keymap["U0-F11"] = act.launch_obsidian()
        global_keymap["W-F12"] = act.launch_calc()
        global_keymap["U0-F12"] = act.launch_calc()
        global_keymap["LaunchApp2"] = act.launch_calc()
