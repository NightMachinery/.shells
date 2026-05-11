function display_off()
    brishzeval2bg("display-off")
end
hyper_bind_v2{mods={"cmd"}, key="l", pressedfn=display_off}
---
function install()
    -- @bootstrap installs the CLI binary
    -- https://www.hammerspoon.org/docs/hs.ipc.html#cliInstall
    -- This needs some dirs to be user-writable (see the docs), so using `ln -s /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs ~/bin/` directly is better,
    hs.ipc.cliUninstall()
    res = hs.ipc.cliInstall()
    -- res = hs.ipc.cliInstall('/Users/evar/bin', false)
    -- brishzeval(string.format("echo hs cli result: %s", res))
end
-- install()
---
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
hyper_bind_v2{mods={"cmd"}, key="r", pressedfn=hs.reload}
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
scriptsHammerspoonWatcher = hs.pathwatcher.new(nightdir .. "/hammerspoon/", reloadConfig):start()
---
function loadHammerspoonAutoLoad()
    local dir = nightdir .. "/hammerspoon/auto-load"
    local files = {}

    for file in hs.fs.dir(dir) do
        if file:match("%.lua$") then
            table.insert(files, file)
        end
    end

    table.sort(files)

    for _, file in ipairs(files) do
        dofile(dir .. "/" .. file)
    end
end

loadHammerspoonAutoLoad()
---
printLocation()
-- We need to call this here so that Hammerspoon appears in the System location permissions. The first call to it also sometimes doesn't work, and this solves that, too.
---
brishzeval("bell-lm-eternalhappiness")
--- @end
