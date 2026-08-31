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
        brishz_eval_hs('awaysh-fast h-hs-on-qview green')
    end,
}

qview_bind_v2{
    -- mods={},
    key="b",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast h-hs-on-qview blue')
    end,
}

qview_bind_v2{
    -- mods={},
    key="r",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast h-hs-on-qview red')
    end,
}

qview_bind_v2{
    -- mods={},
    key="n",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast h-hs-on-qview navy')
    end,
}

qview_bind_v2{
    -- mods={},
    key="m",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast h-hs-on-qview lightsalmon')
    end,
}

qview_bind_v2{
    -- mods={},
    key="x",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast h-hs-on-qview gray')
    end,
}

qview_bind_v2{
    -- mods={"shift"},
    key="d",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast hs-reval-alert qview-trs')
    end,
}

qview_bind_v2{
    -- mods={"shift"},
    key="u",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast hs-reval-alert qview-restore-last')
    end,
}

qview_bind_v3{
    -- mods={},
    key={"SPC", "c", "c"},
    pressedfn=function()
        brishz_eval_hs('awaysh-fast h-hs-on-qview dup')
    end,
}
