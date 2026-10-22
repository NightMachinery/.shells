-- Global Hyper+Shift+Z. Hyper+Z retains the existing kitty panel action.
if OmniPrompter then OmniPrompter.stop() end
OmniPrompter=dofile(nightdir .. "/hammerspoon/omni-prompter/main.lua")({})
OmniPrompter.binding=hyper_bind_v2{mods={"shift"},key="z",pressedfn=function()
    if HyperFocusRestore then HyperFocusRestore.cancel() end
    hyper_exit()
    OmniPrompter.show()
end}
