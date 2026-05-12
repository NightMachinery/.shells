qview_mode = ModalMode.createAppFocusMode{
    name = "qview",
    appName = "qView",
    bundleID = "com.interversehq.qView",
    auto_trigger_p = false,
    overlay = {
        text = "qView",
        position = "left-top",
        overlayMargin = 0,
    },
}

ModalMode.installGlobals(qview_mode, "qview")

qview_bind_v2{
    mods = {"shift"},
    key = "escape",
    auto_trigger_p = false,
    pressedfn = qview_exit,
}

qview_bind_v2{
    -- mods={},
    key="g",
    pressedfn=function()
        brishzeval('awaysh-fast h-hs-on-qview green')
    end,
}

qview_bind_v2{
    -- mods={},
    key="b",
    pressedfn=function()
        brishzeval('awaysh-fast h-hs-on-qview blue')
    end,
}

qview_bind_v2{
    -- mods={},
    key="m",
    pressedfn=function()
        brishzeval('awaysh-fast h-hs-on-qview lightsalmon')
    end,
}

qview_bind_v2{
    -- mods={},
    key="x",
    pressedfn=function()
        brishzeval('awaysh-fast h-hs-on-qview gray')
    end,
}

qview_bind_v2{
    -- mods={"shift"},
    key="d",
    pressedfn=function()
        brishzeval('awaysh-fast hs-reval-alert qview-trs')
    end,
}
