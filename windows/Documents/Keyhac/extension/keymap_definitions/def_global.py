from ..keyhac_helper import KeymapEx
from .. import cmd


def init(keymap: KeymapEx):
    global_keymap = keymap.defineWindowKeymap()

    global_keymap["Insert"] = None

    global_keymap["W-Insert"] = cmd.send_under_mouse_pointer("C-A", "C-C")

    global_keymap["BackSlash"] = "S-BackSlash"

    global_keymap["U2-Left"] = "A-Left"
    global_keymap["U2-Right"] = "A-Right"
    global_keymap["U2-Up"] = "A-Up"
    global_keymap["U2-Down"] = "A-Down"
    global_keymap["U2-B"] = "C-Left"
    global_keymap["U2-F"] = "C-Right"
    global_keymap["U2-N"] = "Down", "Down", "Down", "Down", "Down"
    global_keymap["U2-P"] = "Up", "Up", "Up", "Up", "Up"
    global_keymap["U2-Comma"] = "C-Home"
    global_keymap["U2-Period"] = "C-End"
    global_keymap["U2-W"] = "C-C"
    global_keymap["U2-V"] = "PageUp"
    global_keymap["U3-B"] = "Left"
    global_keymap["U3-P"] = "Up"
    global_keymap["U3-N"] = "Down"
    global_keymap["U3-F"] = "Right"
    global_keymap["U3-A"] = "Home"
    global_keymap["U3-E"] = "End"
    global_keymap["U3-V"] = "PageDown"
    global_keymap["U3-M"] = "Enter"
    global_keymap["U3-H"] = "BackSpace"
    global_keymap["U3-D"] = "Delete"
    global_keymap["U3-S"] = "C-F"
    global_keymap["U3-Y"] = "C-V"
    global_keymap["U3-OpenBracket"] = "Esc"
    global_keymap["U3-Slash"] = "C-Z"
    global_keymap["U3-Underscore"] = "C-Y"
    global_keymap["U3-Left"] = "C-Left"
    global_keymap["U3-Right"] = "C-Right"
    global_keymap["U3-Up"] = "Up", "Up", "Up", "Up", "Up"
    global_keymap["U3-Down"] = "Down", "Down", "Down", "Down", "Down"
    global_keymap["U3-PageUp"] = "C-PageUp"
    global_keymap["U3-PageDown"] = "C-PageDown"
    global_keymap["U3-Atmark"] = "C-Atmark"
    global_keymap["U3-BackSpace"] = "C-BackSpace"
    global_keymap["U3-Delete"] = "C-Delete"

    global_keymap["A-Tab"] = "C-A-Tab"

    # Expand window
    global_keymap["W-S-Left"] = "W-C-A-Left"
    global_keymap["W-S-Right"] = "W-C-A-Right"
    global_keymap["W-S-Down"] = "W-C-A-Down"

    # Vertical maximize window
    global_keymap["W-S-Up"] = "W-S-Up"

    # Disable minimize window
    global_keymap["W-C-Down"] = None

    global_keymap["W-Home"] = "W-C-Left"
    global_keymap["W-End"] = "W-C-Right"
    global_keymap["U0-Home"] = "W-C-Left"
    global_keymap["U0-End"] = "W-C-Right"
    global_keymap["U0-XButton1"] = "W-C-Left"
    global_keymap["U0-XButton2"] = "W-C-Right"
    global_keymap["U0-WheelUp"] = "W-PageUp"
    global_keymap["U0-WheelDown"] = "W-PageDown"
    global_keymap["U0-PageUp"] = "W-PageUp"
    global_keymap["U0-PageDown"] = "W-PageDown"

    global_keymap["U0-F1"] = "MediaPlay"
    global_keymap["U0-F2"] = "VolumeMute"
    global_keymap["U0-F3"] = "VolumeDown"
    global_keymap["U0-F4"] = "VolumeUp"

    global_keymap["W-Z"] = cmd.toggle_always_on_top
    global_keymap["W-D"] = cmd.open_documents
    global_keymap["W-O"] = cmd.open_onedrive

    global_keymap["W-F11"] = cmd.launch_obsidian
    global_keymap["U0-F11"] = cmd.launch_obsidian
    global_keymap["W-F12"] = cmd.launch_calc
    global_keymap["U0-F12"] = cmd.launch_calc
    global_keymap["LaunchApp2"] = cmd.launch_calc
