#Include <config>

config := { global: { stableWait: 100, interval: 1000 }
        , autoreload: { enable: 1 }
        , imeoff: { enable: False, nonactive: 60, idle: 180 }
        , keepawake: { enable: False, interval: 300 }
        , fastscroll: { speed: 10 } }

files := [A_ScriptDir "\autohotkey.ini"
        , EnvGet("USERPROFILE") "\autohotkey.ini"]

Loop files.Length {
    file := files[A_Index]
    if (FileExist(file)) {
        config := ConfigUtil.read(file, config)
    }
}

Loop A_Args.Length {
    switch A_Args[A_Index] {
        case "--keepawake": config.keepawake.enable := True
        case "--nokeepawake": config.keepawake.enable := False
        default: ExitApp, 1
    }
}
