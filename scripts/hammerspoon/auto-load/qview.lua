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
