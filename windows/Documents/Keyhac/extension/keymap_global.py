from .key import KeymapConverter
from . import cmd


# キーマップ適用対象ウィンドウの指定
# - exe_name, class_name, window_text には、ワイルドカード ( * ? ) が使えます。
# - exe_name, class_name, window_text, check_func が None の場合は、
#   その条件を無視します。
TARGET_WINDOW = {
    # 実行ファイル名
    'exe_name': None,
    # クラス名
    'class_name': None,
    # タイトル文字列
    'window_text': None,
    # 識別関数: pyauto.Window オブジェクトを受け取り、True か False を返す
    'check_func': None,
}


def configure_keymap(window_keymap: KeymapConverter):
    window_keymap["Insert"] = cmd.activate_window_under_mouse_pointer

    cursor(window_keymap)
    task_switcher(window_keymap)
    fancy_zones(window_keymap)
    virtual_desktop(window_keymap)
    multimedia_key(window_keymap)


def cursor(window_keymap: KeymapConverter):
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


def task_switcher(window_keymap: KeymapConverter):
    window_keymap["A-Tab"] = "C-A-Tab"


def fancy_zones(window_keymap: KeymapConverter):
    # Expand window
    window_keymap["W-S-Left"] = "W-C-A-Left"
    window_keymap["W-S-Right"] = "W-C-A-Right"
    window_keymap["W-S-Down"] = "W-C-A-Down"

    # Vertical maximize window
    window_keymap["W-S-Up"] = "W-S-Up"


def virtual_desktop(window_keymap: KeymapConverter):
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


def multimedia_key(window_keymap: KeymapConverter):
    window_keymap["U0-F1"] = "MediaPlay"
    window_keymap["U0-F2"] = "VolumeMute"
    window_keymap["U0-F3"] = "VolumeDown"
    window_keymap["U0-F4"] = "VolumeUp"

    window_keymap["W-F11"] = cmd.launch_obsidian
    window_keymap["U0-F11"] = cmd.launch_obsidian
    window_keymap["W-F12"] = cmd.launch_calc
    window_keymap["U0-F12"] = cmd.launch_calc
    window_keymap["LaunchApp2"] = cmd.launch_calc
