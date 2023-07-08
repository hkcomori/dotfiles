from ..keymap_helper import WindowKeymap
from .. import cmd


exe_name = None
class_name = None
window_text = None
check_func = None

window_keymap: WindowKeymap = dict()

window_keymap["Insert"] = None

window_keymap["U2-Left"] = "A-Left"
window_keymap["U2-Right"] = "A-Right"
window_keymap["U2-Up"] = "A-Up"
window_keymap["U2-Down"] = "A-Down"
window_keymap["U2-B"] = "C-Left"
window_keymap["U2-F"] = "C-Right"
window_keymap["U2-N"] = "Down", "Down", "Down", "Down", "Down"
window_keymap["U2-P"] = "Up", "Up", "Up", "Up", "Up"
window_keymap["U2-Comma"] = "C-Home"
window_keymap["U2-Period"] = "C-End"
window_keymap["U2-W"] = "C-C"
window_keymap["U2-V"] = "PageUp"
window_keymap["U3-B"] = "Left"
window_keymap["U3-P"] = "Up"
window_keymap["U3-N"] = "Down"
window_keymap["U3-F"] = "Right"
window_keymap["U3-A"] = "Home"
window_keymap["U3-E"] = "End"
window_keymap["U3-V"] = "PageDown"
window_keymap["U3-M"] = "Enter"
window_keymap["U3-H"] = "BackSpace"
window_keymap["U3-D"] = "Delete"
window_keymap["U3-S"] = "C-F"
window_keymap["U3-Y"] = "C-V"
window_keymap["U3-OpenBracket"] = "Esc"
window_keymap["U3-Slash"] = "C-Z"
window_keymap["U3-Underscore"] = "C-Y"
window_keymap["U3-Left"] = "C-Left"
window_keymap["U3-Right"] = "C-Right"
window_keymap["U3-Up"] = "Up", "Up", "Up", "Up", "Up"
window_keymap["U3-Down"] = "Down", "Down", "Down", "Down", "Down"
window_keymap["U3-PageUp"] = "C-PageUp"
window_keymap["U3-PageDown"] = "C-PageDown"
window_keymap["U3-Atmark"] = "C-Atmark"
window_keymap["U3-BackSpace"] = "C-BackSpace"
window_keymap["U3-Delete"] = "C-Delete"

window_keymap["A-Tab"] = "C-A-Tab"

# Expand window
window_keymap["W-S-Left"] = "W-C-A-Left"
window_keymap["W-S-Right"] = "W-C-A-Right"
window_keymap["W-S-Down"] = "W-C-A-Down"

# Vertical maximize window
window_keymap["W-S-Up"] = "W-S-Up"

# Disable minimize window
window_keymap["W-C-Down"] = None
window_keymap["W-D"] = None

window_keymap["W-Home"] = "W-C-Left"
window_keymap["W-End"] = "W-C-Right"
window_keymap["U0-Home"] = "W-C-Left"
window_keymap["U0-End"] = "W-C-Right"
window_keymap["U0-XButton1"] = "W-C-Left"
window_keymap["U0-XButton2"] = "W-C-Right"
window_keymap["U0-WheelUp"] = "W-PageUp"
window_keymap["U0-WheelDown"] = "W-PageDown"
window_keymap["U0-PageUp"] = "W-PageUp"
window_keymap["U0-PageDown"] = "W-PageDown"

window_keymap["U0-F1"] = "MediaPlay"
window_keymap["U0-F2"] = "VolumeMute"
window_keymap["U0-F3"] = "VolumeDown"
window_keymap["U0-F4"] = "VolumeUp"

window_keymap["W-F11"] = cmd.launch_obsidian
window_keymap["U0-F11"] = cmd.launch_obsidian
window_keymap["W-F12"] = cmd.launch_calc
window_keymap["U0-F12"] = cmd.launch_calc
window_keymap["LaunchApp2"] = cmd.launch_calc
