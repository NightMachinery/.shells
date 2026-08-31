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
--- * Auto-reload
--- Editing a .lua under ~/.hammerspoon/ or $NIGHTDIR/hammerspoon/ reloads the
--- config. Two things temper that.
---
--- ** Coalescing
--- A save is rarely one write. An editor writes a temp file and renames it, and
--- anything editing several files at once fires the watcher several times in a
--- row - which used to mean reloading again while the previous reload was still
--- running. Waiting a moment for the burst to end and reloading once is both
--- calmer and faster.
hammerspoonReloadCoalesceSeconds = hammerspoonReloadCoalesceSeconds or 0.2

--- `x = x or true' cannot express a switch that defaults to on: `false or true'
--- is true, so the knob could never be turned off. Hence the long form, here
--- and for every other boolean below.
if hammerspoonReloadCoalesce == nil then
    hammerspoonReloadCoalesce = true
end

--- ** Holds
--- Any file in this directory whose mtime is in the *future* is a live claim on
--- the reloader, and while one exists nothing here reloads by itself. It is a
--- directory rather than a single flag because several agents edit this repo at
--- once: one file each means they cannot clobber each other, and whoever
--- finishes first does not re-enable reloading under someone still typing.
---
--- The deadline is the mtime rather than the contents so that this check is one
--- stat and no parsing. The contents are for humans - see [agfi:hs-reload-holds].
---
--- A claim expiring on its own is the point. An agent that crashes, is killed,
--- or simply forgets must not be able to leave auto-reload off for good; a hold
--- that ends early is a much smaller problem than one that never ends.
hammerspoonNoReloadDir = hammerspoonNoReloadDir
    or (os.getenv("HOME") .. "/.hs-no-reload")

--- Off by default: a band on every suppressed save is a lot of banding. Turn it
--- on while you are working on the holds themselves, or if you keep forgetting
--- that one is up.
if hammerspoonReloadHeldAlert == nil then
    hammerspoonReloadHeldAlert = false
end

--- Who is holding the reloader, or nil. Global so that "why did my save not do
--- anything" is one command away:
---   hs -c 'return hammerspoonReloadHeldBy()'
function hammerspoonReloadHeldBy()
    -- hs.fs.dir raises rather than returning nil when the directory is missing,
    -- and missing is the normal case: nothing has ever held the reloader.
    local ok, iter, dirObj = pcall(hs.fs.dir, hammerspoonNoReloadDir)
    if not ok then
        return nil
    end

    local now = os.time()
    local holder = nil
    -- Runs to the end rather than breaking out, so the directory handle is
    -- closed by the iterator itself. It holds one small file per agent.
    for entry in iter, dirObj do
        if entry ~= "." and entry ~= ".." then
            local attrs = hs.fs.attributes(hammerspoonNoReloadDir .. "/" .. entry)
            if attrs and attrs.modification > now then
                holder = holder or entry
            end
        end
    end
    return holder
end

local function reloadUnlessHeld()
    local holder = hammerspoonReloadHeldBy()
    if not holder then
        hs.reload()
        return
    end
    if hammerspoonReloadHeldAlert then
        -- One id, so a burst of saves refreshes a single band instead of
        -- stacking a wall of them; the engine's same-id rule also keeps an
        -- unchanged message from re-flashing.
        alert_gateway("auto-reload held by " .. holder, {
            id = "hs-reload-held",
            color = "notice",
            seconds = 4,
        })
    end
end

function reloadConfig(files)
    local doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if not doReload then
        return
    end

    if not hammerspoonReloadCoalesce then
        reloadUnlessHeld()
        return
    end

    -- Restarting one timer is what coalesces; a timer per event would only
    -- delay each reload rather than merge them, which is why timerifyFn in
    -- core/helpers.lua is not what this wants. The handle is global because a
    -- file-local one is collected and never fires - see core/power-watcher.lua.
    if hammerspoonReloadPendingTimer then
        hammerspoonReloadPendingTimer:stop()
    end
    hammerspoonReloadPendingTimer =
        hs.timer.doAfter(hammerspoonReloadCoalesceSeconds, function()
            hammerspoonReloadPendingTimer = nil
            -- Checked here rather than when the timer was set, so a hold
            -- released during the wait still gets its reload.
            reloadUnlessHeld()
        end)
end
--- Bound to hs.reload directly, not to any of the above: a hold suppresses the
--- automatic path only, and asking for a reload by hand always gets one.
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
