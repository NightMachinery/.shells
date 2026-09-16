function display_off()
    brishz_eval_hs("display-off")
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
    -- brishz_eval_hs(string.format("echo hs cli result: %s", res))
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
--- row, which without this would reload again while the previous reload is
--- still running. Waiting a moment for the burst to end and reloading once is
--- both calmer and faster.
hammerspoonReloadCoalesceSeconds = hammerspoonReloadCoalesceSeconds or 0.2

--- `x = x or true' cannot express a switch that defaults to on: `false or true'
--- is true, so the knob could never be turned off. Hence the long form, here
--- and for every other boolean below.
if hammerspoonReloadCoalesce == nil then
    hammerspoonReloadCoalesce = true
end

--- ** Holds
--- While anyone holds `service:hs-reload', nothing here reloads by itself. It
--- is a hold per holder rather than one flag because several agents edit this
--- repo at once: whoever finishes first must not re-enable reloading under
--- someone still typing. That shape - many holders, all wanting the same
--- outcome - is a *suppression registry* rather than a lock, and it is the
--- shared mode of the general hold mechanism. See =scripts/docs/holds.md=.
---
--- This asks night_hold rather than reading the hold files itself, and that is
--- the whole point: a hold now ends when its holder *dies*, and only night_hold
--- can tell whether a pid is still alive. A check written here would keep
--- auto-reload suppressed by a crashed agent's leftover file until something
--- else happened to clear it, which is exactly the failure this is supposed to
--- rule out.
---
--- It costs one hs.task, ~7.5 ms and off the main thread; see taskWithPath in
--- core/helpers.lua, which also repairs the PATH so ~/go/bin is reachable.
---
--- **It fails open.** A missing or broken binary reloads rather than refusing
--- to, because a hold that ends early is a much smaller problem than one that
--- never ends. You can always reload by hand - `hs-reload', or Hyper+Cmd+R.

--- The resource night_hold keeps this under, and the binary that answers for
--- it. An absolute path because hs.task needs one, and because Hammerspoon's
--- PATH is the bare launchd one.
hammerspoonReloadResource = hammerspoonReloadResource or "service:hs-reload"
hammerspoonNightHoldBin = hammerspoonNightHoldBin
    or (os.getenv("HOME") .. "/go/bin/night_hold")

--- Off by default: a band on every suppressed save is a lot of banding. Turn it
--- on while you are working on the holds themselves, or if you keep forgetting
--- that one is up.
if hammerspoonReloadHeldAlert == nil then
    hammerspoonReloadHeldAlert = false
end

--- Who is holding the reloader, or nil. Global so that "why did my save not do
--- anything" is one command away:
---   hs -c 'return hammerspoonReloadHeldBy()'
---
--- Synchronous, unlike the check on the save path: this one exists to be typed
--- at, and an answer that arrives in a callback is no answer at all.
function hammerspoonReloadHeldBy()
    local out, ok = hs.execute(
        hammerspoonNightHoldBin .. " holders " .. hammerspoonReloadResource .. " 2>/dev/null")
    if not ok or not out then
        return nil
    end
    -- One holder id per line; the first is enough to answer "who".
    return out:match("^[^\n]+")
end

--- Reloads unless someone holds the reloader. Asynchronous, so a save never
--- blocks Hammerspoon's main thread waiting on a subprocess.
local function reloadUnlessHeld()
    local task = taskWithPath(hammerspoonNightHoldBin, function(exitCode, stdOut, _)
        -- Anything other than a clean run with a holder on stdout means
        -- reload: no holders, a missing binary, a broken one. Fail open.
        local holder = nil
        if exitCode == 0 and stdOut then
            holder = stdOut:match("^[^\n]+")
        end
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
    end, {"holders", hammerspoonReloadResource})

    if not task or not task:start() then
        hs.reload()
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
brishz_eval_hs("bell-lm-eternalhappiness")
--- @end
