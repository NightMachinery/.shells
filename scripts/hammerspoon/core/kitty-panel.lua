--- * kitty panel (hyper+z in panel mode), over kitty remote control
--
-- The panel is kitty's own business: this file only asks the running kitty,
-- through `kitten @', to create, show and hide it. Every step is an hs.task,
-- so a press never blocks the main thread, and nothing needs a shell or
-- BrishGarden.
--
-- It used to be the zsh functions kitty-panel-ensure / -show / -hide, run in
-- the garden. The work never needed the garden or zsh, but running it there
-- made hyper+z dead for the 15 hours the garden was down after the tmux
-- server crashed on 2026-09-29.
--
-- The socket comes from the app's pid: kitty.conf puts {kitty_pid} in
-- `listen_on', so ~/.local/state/kitty-<pid>.sock belongs to the running
-- kitty, and a socket left behind by a crashed one is never picked. The zsh
-- side has the same knowledge in [agfi:kitty-socket-get], which the shell
-- still uses; this file does not need its KITTY_LISTEN_ON or pgrep fallbacks.
--
-- Used by kittyPanelToggle and kittyFocusWatcher in
-- core/window-media-bindings.lua.

local kittyBundleID = "net.kovidgoyal.kitty"
-- The Go binary itself: /opt/homebrew/bin/kitten is a bash wrapper around it,
-- which costs about 5 ms a call for nothing.
local kittenBin = "/Applications/kitty.app/Contents/MacOS/kitten"
local kittyPanelClass = "kitty-panel"

-- true: on every show, move tabs that scripts opened in normal kitty windows
-- into the panel. Otherwise they are folded in only when the panel is
-- created, which is every kitty launch (kitty cannot start as a panel, so its
-- startup session always opens in a normal window first). This was the zsh
-- variable $kitty_panel_fold_strays; set it here or from the console.
kitty_panel_fold_strays = kitty_panel_fold_strays or false

-- The screen the panel shows on: a core/screens.lua spec, resolved at every
-- show, or false to leave the panel wherever kitty put it. The default,
-- "working", is the focused window's screen (see screens_working_policy).
if kitty_panel_screens == nil then kitty_panel_screens = "working" end

-- Running tasks, sockets and pending timers are pinned with hsPin and
-- hsAfter (core/helpers.lua), so they are neither collected before their
-- callbacks fire nor kept forever afterwards.

local function kittyPanelFail(msg)
    print("kittyPanel: " .. msg)
    alert_gateway("kitty panel: " .. msg, { id = "kitty-panel", color = "warn" })
end

-- Path of the running kitty's remote-control socket, or nil.
local function kittySocketPath()
    local app = getApp(kittyBundleID)
    local pid = app and app:pid()
    if not pid then return nil end

    local path = os.getenv("HOME") .. "/.local/state/kitty-" .. pid .. ".sock"
    local attrs = hs.fs.attributes(path)
    if attrs and attrs.mode == "socket" then return path end
    return nil
end

-- The same as a `--to' address for kitten, or nil.
local function kittySocket()
    local path = kittySocketPath()
    return path and ("unix:" .. path)
end

-- `--match' argument for a kitty window or tab id. %d, not `..': an id that
-- hs.json hands back as a float must not become "id:2.0".
local function kittyMatchID(id)
    return string.format("id:%d", id)
end

-- Runs `bin argv...' as an hs.task and calls cb(ok, stdout, stderr) exactly
-- once. A task still running after `timeout' seconds (default 10) is
-- terminated, so a wedged kitty cannot pile up tasks.
--
-- The stdout must stay small. hs.task (Hammerspoon 1.1.1) collects it only
-- once the process has exited, so a child that writes more than the 64 KiB
-- pipe buffer blocks forever and its callback never fires. Measured
-- 2026-09-30: 1 KB came back, 200 KB never did, with or without a streaming
-- callback. That is why `ls' goes through jq (see kittyPanelList).
local function kittyTask(bin, argv, cb, timeout)
    timeout = timeout or 10
    local taskKey, timer

    local function finish(ok, out, err)
        if not hsUnpin(taskKey) then return end
        hsCancel(timer)
        cb(ok, out, err)
    end

    local task = hs.task.new(bin, function(code, out, err)
        finish(code == 0, out or "", ((err or ""):gsub("%s+$", "")))
    end, argv)
    if not task then return cb(false, "", "could not start " .. bin) end

    taskKey = hsPin(task)
    if not task:start() then
        hsUnpin(taskKey)
        return cb(false, "", "could not start " .. bin)
    end

    timer = hsAfter(timeout, function()
        local t = hsPinned(taskKey)
        if t and t:isRunning() then t:terminate() end
        finish(false, "", bin .. " timed out after " .. timeout .. " s")
    end)
end

-- Runs `kitten @ --to <sock> <args...>' and calls cb(ok, stdout, stderr).
-- Only for commands with small output; never `ls' (see kittyTask).
local function kitten(sock, args, cb)
    local argv = { "@", "--to", sock }
    for _, a in ipairs(args) do argv[#argv + 1] = a end
    kittyTask(kittenBin, argv, cb)
end

-- jq, which ships with macOS from 15 on and is a brew package before that.
local function kittyJq()
    for _, path in ipairs({ "/usr/bin/jq", "/opt/homebrew/bin/jq", "/usr/local/bin/jq" }) do
        if hs.fs.attributes(path, "mode") then return path end
    end
    return nil
end

-- What one `ls' says about the panel, as a small JSON object:
--   win      some window in the panel OS window, for `--match id:'
--   active   the panel's active tab's active window, the one to focus
--   firstTab the panel's first tab, where stray tabs are moved to
--   strays   the tabs of every other (normal) OS window, in order
--   freshTab the tab holding window $fresh (-1: none asked for)
-- Keys whose value would be null are left out, so they read as nil in Lua.
local kittyPanelJq = [[
[.[] | select(.wm_class == $class)] as $p
| {
    win: ([$p[] | .tabs[] | .windows[] | .id][0]),
    active: ([$p[] | .tabs[] | select(.is_active) | .windows[] | select(.is_active) | .id][0]),
    firstTab: ([$p[] | .tabs[0].id][0]),
    strays: [.[] | select(.wm_class != $class) | .tabs[].id],
    freshTab: ([.[] | .tabs[] | select(any(.windows[]; .id == $fresh)) | .id][0])
  }
| with_entries(select(.value != null))
]]

-- `ls | jq' for kittyPanelList. The pipeline runs in the background so the
-- TERM that kittyTask's timeout sends is handled at once; the trap then kills
-- both halves, which are this shell's own children. Without it the timeout
-- would kill only the shell and leave kitten and jq behind.
local kittyPanelListSh = [[
trap '/usr/bin/pkill -P $$; exit 143' TERM
"$1" @ --to "$2" ls | "$3" -c --arg class "$4" --argjson fresh "$5" "$6" &
wait "$!"
]]

-- cb(state) with the fields above, or cb(nil, err).
--
-- `kitten @ ls' is far too big to read here: 657 KB for 13 tabs, 578 KB of
-- it `foreground_processes', which no option leaves out. hs.task cannot
-- return that much at all, and hs.json.decode takes 83 ms over it on the
-- main thread. jq cuts it down to about 50 bytes in the child, and the whole
-- pipeline takes 40 to 60 ms.
local function kittyPanelList(sock, cb, freshWin)
    local jq = kittyJq()
    if not jq then return cb(nil, "jq not found (brew install jq)") end

    kittyTask("/bin/sh",
              { "-c", kittyPanelListSh,
                "sh", kittenBin, sock, jq, kittyPanelClass, string.format("%d", freshWin or -1), kittyPanelJq },
              function(ok, out, err)
                  -- A failed `ls' feeds jq nothing, and jq then succeeds
                  -- silently: no output is the failure signal.
                  if not ok or out == "" then
                      return cb(nil, "ls: " .. (err ~= "" and err or "no output"))
                  end
                  local okj, st = pcall(hs.json.decode, out)
                  if not okj or type(st) ~= "table" then
                      return cb(nil, "could not parse the panel state: " .. out)
                  end
                  st.strays = st.strays or {}
                  cb(st)
              end)
end

-- Moves the stray tabs into the panel, in order; an OS window left without
-- tabs closes itself. The shell tab a new panel was born with is only
-- scaffolding once real tabs have arrived, so it is closed then.
-- cb(sock, state, err) with the state as it is afterwards.
local function kittyPanelFold(sock, st, freshTab, cb)
    if #st.strays == 0 then return cb(sock, st) end
    if not st.firstTab then return cb(nil, nil, "no panel tab to move tabs into") end

    local moved, i = 0, 0
    local nextTab

    local function finish()
        if moved == 0 then return cb(sock, st) end

        local function relist()
            kittyPanelList(sock, function(st2, err)
                if not st2 then return cb(nil, nil, err) end
                cb(sock, st2)
            end)
        end

        if freshTab then
            kitten(sock, { "close-tab", "--match", kittyMatchID(freshTab) }, function() relist() end)
        else
            relist()
        end
    end

    nextTab = function()
        i = i + 1
        local t = st.strays[i]
        if not t then return finish() end

        kitten(sock, { "detach-tab", "--match", kittyMatchID(t), "--target-tab", kittyMatchID(st.firstTab) },
               function(ok, _, err)
                   if ok then
                       moved = moved + 1
                   else
                       print("kittyPanel: could not move tab " .. t .. " into the panel: " .. err)
                   end
                   nextTab()
               end)
    end

    nextTab()
end

--- ** Which screen the panel is on
--
-- kitty names a panel's screen by `output-name', which on macOS is the
-- screen's localized name, the same string as hs.screen:name() (`kitten
-- panel --output-name list' prints them). Two identical monitors share a
-- name; which of them kitty picks then is untested.
--
-- Every show sends the panel to the wanted screen, even when it should be
-- there already. kitty answers a move to the screen the panel is on with a
-- fresh layout (in kitty 0.48.2, resize_os_window.py applies the config
-- without comparing it to the stored one), so this costs one
-- remote-control call (1 to 4 ms) and needs
-- no record of where the panel is. Such a record went stale: it only knew
-- the moves made here, while hyper+shift+; or a display change can move the
-- panel too.

-- The output name the panel should be on now, and its hs.screen, or nil to
-- leave the panel alone.
local function kittyPanelWantedOutput()
    if not (kitty_panel_screens and Screens) then return nil end
    local s = Screens.target(kitty_panel_screens)[1]
    return s and s:name(), s
end

-- kitty answers ok to an output-name it does not know, and leaves the panel
-- where it is (screen_for_name in kitty's cocoa_window.m falls back to the
-- screen under the window's centre). The state kitten lists the names kitty
-- knows, so that case is printed rather than silent. A kitty running since
-- before a display change can hold an outdated name: kitty updates its
-- monitor list in place and keeps the old name (_glfwPollMonitorsNS).
local function kittyPanelCheckOutput(label, st, output)
    if not (output and type(st) == "table" and type(st.monitors) == "table") then return end
    for _, name in ipairs(st.monitors) do
        if name == output then return end
    end
    print(string.format("kittyPanel: %s: kitty knows no screen named %q (it knows: %s); the fit below moves the panel",
                        label, output, table.concat(st.monitors, ", ")))
end

-- The frame kitty gives an `edge=center' panel (_glfwPlatformSetLayerShellConfig
-- in kitty's cocoa_window.m): the screen's whole width, and from the bottom
-- of the menu bar to the screen's bottom edge, over the Dock. In
-- Hammerspoon's top-left coordinates that runs from frame().y (the usable
-- frame) to the bottom of fullFrame().
local function kittyPanelFrameOn(screen)
    local full, usable = screen:fullFrame(), screen:frame()
    return hs.geometry.rect(full.x, usable.y, full.w, full.y + full.h - usable.y)
end

-- Shown on the laptop after living on the monitor, the panel came out
-- 1920x1056 (the monitor's size) at the laptop's origin, spilling onto the
-- monitor: measured 2026-10-02 with kitty 0.48.2. kitty's layout takes the
-- size from the screen it picks, so it either laid the panel out against
-- outdated screen data or was never asked to (the old record said the panel
-- was on the laptop already); which, is unknown. Every show now asks for a
-- fresh layout (above), and after it, a panel that is not where kitty's own
-- layout puts it (kittyPanelFrameOn, give or take kPanelFitSlack for
-- kitty's rounding) is set to it over Accessibility. Whether kitty's
-- borderless panel takes a new size this way is unmeasured, so the line
-- printed says where the panel ended up.
--
-- The check reads the panel's bounds from CoreGraphics' window list
-- (Screens.windowStack), and asks kitty nothing unless the panel is off:
-- reading them over Accessibility instead took 10 to 85 ms right after a
-- show (measured 2026-10-03), while kitty was busy drawing. The panel is
-- kitty's one window above layer 0, and is listed only while shown, so this
-- runs after the show, and after the focus too, so it never holds them up.
local kPanelFitSlack = 2

local function kittyPanelFitNow(label, screen)
    local app = getApp(kittyBundleID)
    if not (app and screen) then return nil end
    local want = kittyPanelFrameOn(screen)
    local function near(f)
        return math.abs(f.x - want.x) <= kPanelFitSlack and math.abs(f.y - want.y) <= kPanelFitSlack
           and math.abs(f.w - want.w) <= kPanelFitSlack and math.abs(f.h - want.h) <= kPanelFitSlack
    end
    local pid = app:pid()
    for _, e in ipairs(Screens.windowStack()) do
        if e.pid == pid and e.layer ~= 0 and e.frame.w > 200 and e.frame.h > 200 then
            local f = e.frame
            if near(f) then return "ok" end
            local w = Screens.entryWindow(e)
            if not w then
                print("kittyPanel: " .. label .. ": could not fetch the panel window to fit it")
                return "moved"
            end
            pcall(function() w:setFrame(want, 0) end)
            local okg, g = pcall(function() return w:frame() end)
            g = okg and g or f
            -- %g, not %d: Accessibility frames can be fractional, and
            -- Lua 5.4's %d raises on 1056.5.
            print(string.format("kittyPanel: %s: %s the panel to %s: %gx%g@%g,%g -> %gx%g@%g,%g", label,
                                near(g) and "fitted" or "could not fit", screen:name() or "?",
                                f.w, f.h, f.x, f.y, g.w, g.h, g.x, g.y))
            return "moved"
        end
    end
    return false
end

-- kittyPanelFitNow with any error printed, not raised. "ok": the panel is
-- on screen where kitty's layout puts it; "moved": it was elsewhere and was
-- set there (or that was tried); false: it is not on screen; nil: no screen
-- to check, or an error.
local function kittyPanelFit(label, screen)
    local ok, res = pcall(kittyPanelFitNow, label, screen)
    if not ok then
        print("kittyPanel: " .. label .. ": fit: " .. tostring(res))
        return nil
    end
    return res
end

--- ** Which Space the panel joins
--
-- kitty marks the panel CanJoinAllSpaces but not FullScreenAuxiliary, so by
-- itself it never appears over a fullscreen app. To get it there anyway,
-- kitty's show (_glfwPlatformShowWindow in its cocoa_window.m, the same in
-- 0.48.2 and in master on 2026-10-03) adds the panel to one Space:
-- CGSGetActiveSpace(), the focused Space, which Hammerspoon reads as
-- hs.spaces.focusedSpace(). Measured 2026-10-03 with two displays:
--
-- - The panel comes up only when that Space is on the display the panel is
--   on. Shown onto the laptop while the monitor's Space was focused, it
--   stayed off screen; shown onto the monitor with the monitor's fullscreen
--   Space focused, it came up over the fullscreen app.
-- - Moving a shown panel to another display with output-name put it in
--   that display's desktop Space, behind the fullscreen Space the display
--   was showing: off screen.
-- - hs.spaces.moveWindowToSpace(panel, thatFullscreenSpace, true) answered
--   true and changed nothing.
--
-- kitty counts a panel sitting in a Space that is not showing as visible,
-- and answers ok to a `show' of a visible panel without doing anything
-- (toggle_os_window_visibility in its glfw.c). So once the panel was lost
-- this way, every hyper+z "showed" it and nothing appeared, until the user
-- went to that Space by hand.
--
-- Hence: every show hides first, which kitty skips for a hidden panel and
-- which makes a lost one showable again; after the show, a panel that is
-- not on screen is put right (kittyPanelAfterShow); and a move to another
-- screen focuses that screen's Space before the show (kittyPanelFocusSpaceOn).

-- The Space `screen' is showing and its type ("user" or "fullscreen"), or
-- nil. Both calls read the Dock's Space list, 8 to 17 ms each (measured
-- 2026-10-03), so only moves and repairs ask, never a plain show.
local function kittySpaceOn(screen)
    local ok, sid = pcall(hs.spaces.activeSpaceOnScreen, screen)
    if not (ok and sid) then return nil end
    local okt, kind = pcall(hs.spaces.spaceType, sid)
    return sid, okt and kind or nil
end

-- kittyFocusWatcher (core/window-media-bindings.lua) hides the panel when
-- another app comes to the front. While a show is under way it must not:
-- the hide before the show makes kitty hand focus back to the app it came
-- from, and focusing a Space activates that Space's app, and either
-- watcher hide could reach kitty after the show and hide the panel again.
-- The watcher skips activations before this time.
kittyPanelShowingUntil = 0
local kShowGuardSeconds, kShowGuardTailSeconds = 5, 0.3

local kSpaceFocusWaitSeconds, kSpaceFocusPollSeconds = 0.5, 0.02
-- How long after a show, or a Space move, the window list is read again.
local kRecheckSeconds = 0.15

-- Makes the Space `screen' shows the focused one, by focusing the front
-- window there, then cb(ok, note) once hs.spaces.focusedSpace() agrees, or
-- once the wait runs out. The show goes ahead either way.
local function kittyPanelFocusSpaceOn(screen, cb)
    local sid = kittySpaceOn(screen)
    if not sid then return cb(false, "no Space found for " .. tostring(screen:name())) end
    if hs.spaces.focusedSpace() == sid then return cb(true) end
    local okw, w = pcall(Screens.frontWindowOn, screen)
    if not (okw and w) then return cb(false, "no window on " .. tostring(screen:name()) .. " to focus") end
    pcall(function() w:focus() end)
    local deadline = hs.timer.secondsSinceEpoch() + kSpaceFocusWaitSeconds
    local function poll()
        local now = hs.spaces.focusedSpace()
        if now == sid then return cb(true) end
        if hs.timer.secondsSinceEpoch() > deadline then
            return cb(false, string.format("the focused Space is still %s, not %d", tostring(now), sid))
        end
        hsAfter(kSpaceFocusPollSeconds, poll)
    end
    poll()
end

-- A panel that is not on screen after its show. A desktop Space takes it
-- with moveWindowToSpace; a fullscreen Space does not (see above), so there
-- the show is redone with that Space focused first. `st.platformId' is the
-- panel's window number, from the state kitten; `ls' has no such field
-- here, so after a slow show only the redo is possible.
local function kittyPanelRepairSpace(label, screen, st)
    local sid, kind = kittySpaceOn(screen)
    local id = st and st.platformId
    local okh, have = pcall(function() return id and hs.spaces.windowSpaces(id) end)
    local where = string.format("it is in Space %s, %s shows %s Space %s, the focused Space is %s",
                                (okh and type(have) == "table") and table.concat(have, ",") or "?",
                                screen:name() or "?", tostring(kind), tostring(sid), tostring(hs.spaces.focusedSpace()))
    if sid and kind == "user" and id then
        local okm, res, err = pcall(hs.spaces.moveWindowToSpace, id, sid)
        hsAfter(kRecheckSeconds, function()
            local found = kittyPanelFit(label, screen)
            print(string.format("kittyPanel: %s: the panel was not on screen (%s); %s", label, where,
                                found and "moved it into that Space"
                                      or ("could not move it there: " .. tostring(okm and (err or res) or res))))
        end)
        return
    end
    print(string.format("kittyPanel: %s: the panel was not on screen (%s); showing it again with that Space focused",
                        label, where))
    kittyPanelShow(label .. " (again)", { screen = screen, focusSpace = true, again = true })
end

-- After a show: fit the panel, or put it right if it is not on screen. The
-- window list is read again after a moment before anything is repaired, so
-- a panel that is merely late is not shown twice. A panel the fit had to
-- move is checked again too: kitty sometimes lays it out at the new
-- screen's size but on the old screen (see the fit above), and moving it
-- across over Accessibility put it in the new screen's desktop Space,
-- behind the fullscreen Space that screen was showing (measured
-- 2026-10-03). `again' marks the second show, which only reports.
local function kittyPanelAfterShow(label, screen, st, again)
    local first = kittyPanelFit(label, screen)
    if first == nil or first == "ok" then return end
    hsAfter(kRecheckSeconds, function()
        if kittyPanelFit(label, screen) ~= false then return end
        if again then
            return print(string.format("kittyPanel: %s: the panel is still not on screen (the focused Space is %s)",
                                       label, tostring(hs.spaces.focusedSpace())))
        end
        local ok, err = pcall(kittyPanelRepairSpace, label, screen, st)
        if not ok then print("kittyPanel: " .. label .. ": repair: " .. tostring(err)) end
    end)
end

-- `edge=center' covers the display; the window level comes from
-- `macos_ns_window_layer' in kitty.conf (above every window in the space,
-- below Spotlight and Handy). Nothing here may focus anything: focusing a
-- hidden panel activates kitty on the desktop space before `show' has joined
-- the current one. That is kittyPanelShow's job, after the show.
local function kittyPanelCreate(sock, cb)
    local output = kittyPanelWantedOutput()
    local argv = { "launch", "--type=os-panel",
                   "--os-panel", "edge=center",
                   "--os-panel", "layer=top",
                   "--os-panel", "focus-policy=on-demand" }
    if output then
        table.insert(argv, "--os-panel")
        table.insert(argv, "output-name=" .. output)
    end
    for _, a in ipairs({ "--os-window-class", kittyPanelClass, "--dont-take-focus" }) do
        table.insert(argv, a)
    end

    kitten(sock, argv,
           function(ok, out, err)
               if not ok then return cb(nil, nil, "could not create the panel: " .. err) end

               local fresh = tonumber((out:gsub("%s+", "")))
               if not fresh then return cb(nil, nil, "launch printed no window id: " .. out) end

               -- A new panel counts as shown for kitty, so a first `show'
               -- would be a no-op while macOS has put it on the desktop space
               -- only. Hidden once, the next `show' orders it onto whatever
               -- space is current.
               kitten(sock, { "resize-os-window", "--match", kittyMatchID(fresh), "--action=hide" }, function()
                   kittyPanelList(sock, function(st, err2)
                       if not st then return cb(nil, nil, err2) end
                       if not st.win then return cb(nil, nil, "the new panel is missing from ls") end
                       kittyPanelFold(sock, st, st.freshTab, cb)
                   end, fresh)
               end)
           end)
end

-- Starts kitty minimized (the panel is what shows it) and waits up to 20 s
-- for its socket to answer. cb(sock, err).
--
-- A kitty that is already running but has no socket gets 5 s: it may have
-- just been started by something else. After that the socket is taken to be
-- lost, the failure that is invisible without being told: `listen_on' is
-- startup-only, and an unlinked socket path cannot be re-linked, so kitty
-- keeps the bound inode while every client gets ENOENT. Reloading kitty's
-- config will not help; only a restart will.
local function kittyLaunch(cb)
    local app = getApp(kittyBundleID)
    local wait, gaveUp

    if app then
        wait = 5
        gaveUp = "kitty is running (pid " .. tostring(app:pid()) ..
                 ") but has no socket in ~/.local/state; restart kitty"
    else
        wait = 20
        gaveUp = "kitty did not come up within 20 seconds"

        local openerKey
        local opener = hs.task.new("/usr/bin/open", function() hsUnpin(openerKey) end,
                                   { "-b", kittyBundleID, "--args", "--start-as", "minimized" })
        openerKey = opener and hsPin(opener)
        if not (opener and opener:start()) then
            if openerKey then hsUnpin(openerKey) end
            return cb(nil, "could not run open -b " .. kittyBundleID)
        end
    end

    local deadline = hs.timer.secondsSinceEpoch() + wait
    local attempt

    local function retry()
        if hs.timer.secondsSinceEpoch() > deadline then return cb(nil, gaveUp) end
        hsAfter(0.25, attempt)
    end

    attempt = function()
        local sock = kittySocket()
        if not sock then return retry() end
        kittyPanelList(sock, function(st)
            if st then cb(sock) else retry() end
        end)
    end

    hsAfter(0.25, attempt)
end

-- kitty running, with a panel OS window holding the tabs.
-- cb(sock, state, err).
local function kittyPanelEnsure(cb)
    local function withSocket(sock)
        kittyPanelList(sock, function(st, err)
            if not st then return cb(nil, nil, err) end
            if not st.win then return kittyPanelCreate(sock, cb) end
            if kitty_panel_fold_strays then return kittyPanelFold(sock, st, nil, cb) end
            cb(sock, st)
        end)
    end

    local sock = kittySocket()
    if sock then return withSocket(sock) end

    kittyLaunch(function(sock2, err)
        if not sock2 then return cb(nil, nil, err) end
        withSocket(sock2)
    end)
end

--- ** Slow path: every case, through kitten

-- Shows the panel, creating it (and launching kitty) first if need be, and
-- focuses its active window. Show first, then focus: focusing a hidden panel
-- activates kitty on the desktop space instead. done(err) at the end.
local function kittyPanelShowSlow(done)
    kittyPanelEnsure(function(sock, st, err)
        if not sock then return done(err) end

        -- `after' (the fit) runs once the panel is up and focused.
        local function show(after)
            local function finish(err)
                done(err)
                if after then hsAfter(0, after) end
            end
            kitten(sock, { "resize-os-window", "--match", kittyMatchID(st.win), "--action=show" }, function(ok, _, err2)
                if not ok then return done("show: " .. err2) end
                if not st.active then return finish() end

                kitten(sock, { "focus-window", "--match", kittyMatchID(st.active) }, function(ok2, _, err3)
                    finish((not ok2) and ("focus: " .. err3) or nil)
                end)
            end)
        end

        -- A failed move is only printed, not banded, and the panel is still
        -- shown: the fit after the show then sets it to the wanted screen
        -- over Accessibility, while kitty keeps its old output-name until a
        -- later show's move gets through.
        local output, outScreen = kittyPanelWantedOutput()
        local function fit() kittyPanelAfterShow("kittyPanelShowSlow", outScreen, st) end
        if not output then return show() end
        kitten(sock, { "resize-os-window", "--match", kittyMatchID(st.win), "--action=os-panel",
                       "--incremental", "output-name=" .. output }, function(ok, _, err2)
            if not ok then print("kittyPanel: move to " .. output .. ": " .. err2) end
            show(fit)
        end)
    end)
end

local function kittyPanelHideSlow(label)
    local sock = kittySocket()
    if not sock then return end

    kittyPanelList(sock, function(st, err)
        if not st then return kittyPanelFail(label .. ": " .. err) end
        if not st.win then return end

        kitten(sock, { "resize-os-window", "--match", kittyMatchID(st.win), "--action=hide" }, function(ok, _, err2)
            if not ok then kittyPanelFail(label .. ": hide: " .. err2) end
        end)
    end)
end

--- ** Fast path: kitty's remote-control protocol over hs.socket
--
-- Through kitten a show took about 120 ms: three calls, each a process
-- spawn, and kitten is a 51 MB Go binary that takes 16 ms just to start,
-- plus sh and jq for `ls'. Spoken straight to kitty's socket a call takes 1
-- to 4 ms (measured 2026-09-30), and no process is started at all. The
-- protocol is one `ESC P @kitty-cmd <JSON> ESC \' each way; the payload
-- fields are listed in rc_protocol.html inside the kitty bundle.
--
-- The state does not come from `ls' at all. `ls' reports every matched
-- window's foreground processes, and a single hidden tab (see
-- docs/kitty-tab-hide.md) made even a filtered two-window `ls' 635 KB,
-- which took kitty 40 ms to produce and hs.json 100 ms to decode on every
-- press. Instead the RC `kitten' command runs
-- configFiles/kitty/kitty_panel_state.py inside kitty, which reads only ids
-- from kitty's tab managers and answers in about 100 bytes and 2 ms.
-- Anything unusual (no panel yet, no kitty, stray tabs to fold, any error)
-- goes to the slow path above, which uses only kitty's documented `ls'
-- output, so it keeps working even if a kitty update breaks the kitten.

-- The in-kitty state kitten. `nightdir' comes from init.lua.
local kittyPanelStateKitten = (nightdir or (os.getenv("HOME") .. "/scripts")) ..
                              "/configFiles/kitty/kitty_panel_state.py"

-- The protocol version to send: kitty refuses a client newer than itself,
-- so this is the installed kitty's own version. Cached per kitty process,
-- because reading it from the bundle takes 7 ms, and it can only change
-- across a kitty restart.
local kittyRCVersionOf = { pid = nil, version = nil }

local function kittyRCVersion()
    local app = getApp(kittyBundleID)
    local pid = app and app:pid()
    if pid and kittyRCVersionOf.pid == pid then return kittyRCVersionOf.version end

    local info = hs.application.infoForBundleID(kittyBundleID)
    local parts = {}
    for n in tostring(info and info.CFBundleShortVersionString or ""):gmatch("%d+") do
        parts[#parts + 1] = tonumber(n)
    end
    local version = (#parts >= 3) and { parts[1], parts[2], parts[3] } or { 0, 48, 2 }
    kittyRCVersionOf = { pid = pid, version = version }
    return version
end

-- One remote-control call: cb(true, data) or cb(false, err), exactly once.
-- `data' is the reply's `data' field (a JSON string for ls), possibly nil.
-- `trace', when given, is called with "conn" once connected and "reply" when
-- the raw reply is in, for the timing line.
local function kittyRC(path, cmd, payload, cb, trace)
    local frame = "\27P@kitty-cmd" ..
                  hs.json.encode({ cmd = cmd, version = kittyRCVersion(), payload = payload }) ..
                  "\27\\"
    local sockKey, timer

    local function finish(ok, data)
        local sock = hsUnpin(sockKey)
        if not sock then return end
        hsCancel(timer)
        pcall(function() sock:disconnect() end)
        cb(ok, data)
    end

    local sock = hs.socket.new(function(raw)
        if trace then trace(string.format("reply(%d B)", #raw)) end
        local body = raw:match("^\27P@kitty%-cmd(.*)\27\\$")
        local ok, reply = pcall(hs.json.decode, body or "")
        if not ok or type(reply) ~= "table" then return finish(false, cmd .. ": unreadable reply") end
        if not reply.ok then return finish(false, cmd .. ": " .. tostring(reply.error or "failed")) end
        finish(true, reply.data)
    end)
    if not sock then return cb(false, "could not create a socket") end
    sockKey = hsPin(sock)

    -- hs.socket reports a failed connect only in its own log, so a call that
    -- never answers is caught here.
    timer = hsAfter(2, function() finish(false, cmd .. ": no reply within 2 s") end)

    sock:connect(path, function()
        local s = hsPinned(sockKey)
        if not s then return end
        if trace then trace("conn") end
        s:write(frame)
        s:read("\27\\")
    end)
end

-- cb(state) with the fields of kitty_panel_state.py (win, active, firstTab,
-- strays), or cb(nil, why); why is "no panel" when kitty answered but has
-- none. `match = "all"' makes sure kitty has a window to run the kitten
-- over even when none of its windows has focus.
local function kittyPanelFastState(path, cb, trace)
    kittyRC(path, "kitten", { kitten = kittyPanelStateKitten, match = "all" }, function(ok, data)
        if not ok then return cb(nil, data) end
        local okj, st = pcall(hs.json.decode, data or "")
        if not okj or type(st) ~= "table" then return cb(nil, "kitten: unreadable state: " .. tostring(data)) end
        st.strays = st.strays or {}
        if not st.win then return cb(nil, "no panel") end
        cb(st)
    end, trace)
end

--- ** Show and hide

-- ", N ms since the key press" when a press is recent (kittyHandler in
-- core/window-media-bindings.lua records kittyPressAt), else "". It shows how
-- long the main thread was busy before this file even started.
function kittySincePress()
    if not kittyPressAt then return "" end
    local ms = (hs.timer.absoluteTime() - kittyPressAt) / 1e6
    if ms > 2000 then return "" end
    return string.format(", %.1f ms since the key press", ms)
end

-- One show at a time: a second press while the first is still creating the
-- panel would otherwise create a second one. The watchdog frees the key if a
-- step never calls back (a launch alone may take 20 s).
local kittyPanelShowBusy = nil

-- Shows the panel and focuses its active window. opts, all optional:
--   screen      the screen to show it on, instead of kitty_panel_screens
--   focusSpace  focus that screen's Space before the show
--               (kittyPanelFocusSpaceOn); moves ask for it
--   again       this is the redo after a show left the panel off screen
--   done        fn(ok, note) once shown, as Screens.moveHandlers expects;
--               failures then go to it rather than to a band
function kittyPanelShow(label, opts)
    label = label or "kittyPanelShow"
    opts = opts or {}
    if kittyPanelShowBusy then
        print("kittyPanel: " .. label .. ": a show is already in progress")
        if opts.done then opts.done(false, ": a show of the panel is already in progress") end
        return
    end

    local token = {}
    kittyPanelShowBusy = token
    kittyPanelShowingUntil = hs.timer.secondsSinceEpoch() + kShowGuardSeconds
    local t0 = hs.timer.absoluteTime()
    local route = "fast"
    local steps = {}

    -- Milliseconds since t0, for the step list in the console line.
    local function mark(name)
        steps[#steps + 1] = string.format("%s %.1f", name, (hs.timer.absoluteTime() - t0) / 1e6)
    end
    local watchdog = hsAfter(30, function()
        if kittyPanelShowBusy == token then
            kittyPanelShowBusy = nil
            kittyPanelShowingUntil = 0
            if opts.done then opts.done(false, ": the show did not finish within 30 s") end
            kittyPanelFail(label .. ": show did not finish within 30 s")
        end
    end)

    local function done(err)
        if kittyPanelShowBusy ~= token then return end
        kittyPanelShowBusy = nil
        -- The tail lets the activations this show caused arrive first.
        kittyPanelShowingUntil = hs.timer.secondsSinceEpoch() + kShowGuardTailSeconds
        hsCancel(watchdog)
        if err then
            if opts.done then
                print("kittyPanel: " .. label .. ": " .. err)
                return opts.done(false, ": " .. err)
            end
            return kittyPanelFail(label .. ": " .. err)
        end
        print(string.format("kittyPanel: %s: shown in %.1f ms (%s%s)%s", label,
                            (hs.timer.absoluteTime() - t0) / 1e6, route,
                            #steps > 0 and (": " .. table.concat(steps, ", ")) or "",
                            kittySincePress()))
        if opts.done then opts.done(true) end
    end

    -- The slow path only knows kitty_panel_screens, so a show for one
    -- given screen (a move) fails instead.
    local function slow(why)
        if opts.screen then return done("the panel needs the slow path (" .. (why or "stray tabs to fold") .. ")") end
        if why and why ~= "no panel" then
            print("kittyPanel: " .. label .. ": fast show failed (" .. why .. "); using kitten")
        end
        route = "kitten"
        kittyPanelShowSlow(done)
    end

    local path = kittySocketPath()
    if not path then return slow() end

    kittyPanelFastState(path, function(st, why)
        mark("state")
        if not st then return slow(why) end
        if kitty_panel_fold_strays and #st.strays > 0 then return slow() end

        local output, outScreen
        if opts.screen then
            output, outScreen = opts.screen:name(), opts.screen
        else
            output, outScreen = kittyPanelWantedOutput()
        end

        -- `after' (the fit and the Space check) runs once the panel is up
        -- and focused, so it is not in the timing line.
        local function show(after)
            local function finish()
                done()
                if after then hsAfter(0, after) end
            end
            kittyRC(path, "resize-os-window", { match = kittyMatchID(st.win), action = "show" }, function(ok, err)
                mark("show")
                if not ok then return slow(err) end
                if not st.active then return finish() end
                kittyRC(path, "focus-window", { match = kittyMatchID(st.active) }, function(ok2, err2)
                    mark("focus")
                    if not ok2 then return slow(err2) end
                    finish()
                end)
            end)
        end
        local function after() kittyPanelAfterShow(label, outScreen, st, opts.again) end

        -- The payload `kitten @ resize-os-window --action=os-panel
        -- --incremental output-name=X' sends (captured on a fake socket).
        -- A failed move is only printed; the fit still runs (see
        -- kittyPanelShowSlow).
        local function moveAndShow()
            if not output then return show(after) end
            kittyPanelCheckOutput(label, st, output)
            kittyRC(path, "resize-os-window", { match = kittyMatchID(st.win), action = "os-panel", incremental = true,
                                                os_panel = { "output-name=" .. output } }, function(ok, err)
                mark("move")
                if not ok then print("kittyPanel: " .. label .. ": move to " .. output .. ": " .. tostring(err)) end
                show(after)
            end)
        end

        -- Hide first (see "Which Space the panel joins"). A failed hide is
        -- only printed.
        kittyRC(path, "resize-os-window", { match = kittyMatchID(st.win), action = "hide" }, function(okh, errh)
            mark("hide")
            if not okh then print("kittyPanel: " .. label .. ": hide before the show: " .. tostring(errh)) end
            if not (opts.focusSpace and outScreen) then return moveAndShow() end
            kittyPanelFocusSpaceOn(outScreen, function(okf, note)
                mark("space")
                if not okf then print("kittyPanel: " .. label .. ": " .. tostring(note) .. "; showing anyway") end
                moveAndShow()
            end)
        end)
    end, mark)
end

-- Hides the panel. No kitty, no panel: nothing to do. A stray normal window
-- is left alone here; the next show folds its tabs in if configured to.
function kittyPanelHide(label)
    label = label or "kittyPanelHide"
    local path = kittySocketPath()
    if not path then return end

    local t0 = hs.timer.absoluteTime()
    kittyPanelFastState(path, function(st, why)
        local tState = (hs.timer.absoluteTime() - t0) / 1e6
        if st then
            return kittyRC(path, "resize-os-window", { match = kittyMatchID(st.win), action = "hide" }, function(ok, err)
                if not ok then
                    print("kittyPanel: " .. label .. ": fast hide failed (" .. err .. "); using kitten")
                    return kittyPanelHideSlow(label)
                end
                print(string.format("kittyPanel: %s: hidden in %.1f ms (state %.1f)%s", label,
                                    (hs.timer.absoluteTime() - t0) / 1e6, tState, kittySincePress()))
            end)
        end
        if why == "no panel" then return end
        print("kittyPanel: " .. label .. ": fast hide failed (" .. tostring(why) .. "); using kitten")
        kittyPanelHideSlow(label)
    end)
end

--- ** Moving the shown panel (hyper+shift+;)
--
-- Screens.moveWindowNext moves windows over Accessibility, which would move
-- the panel behind kitty's back: kitty keeps the panel's output-name and
-- lays it out on that screen again at its next re-layout (a display, DPI or
-- font-size change), so the panel would jump back. So the panel is moved
-- here, through kitty. It is shown again rather than moved while shown: a
-- shown panel moved onto a screen that shows a fullscreen Space lands
-- behind that Space (see "Which Space the panel joins"), so the show
-- focuses the new screen's Space first. done(ok, note) as
-- Screens.moveHandlers expects.
function kittyPanelMoveTo(screen, label, done)
    if not (kittySocketPath() and screen and screen:name()) then
        return done(false, ": kitty has no remote-control socket")
    end
    kittyPanelShow(label, { screen = screen, focusSpace = true, done = done })
end

if Screens and Screens.moveHandlers then
    Screens.moveHandlers.kittyPanel = function(w, to, done)
        local ok, panel = pcall(function()
            local app = w:application()
            return app ~= nil and app:bundleID() == kittyBundleID and not w:isStandard()
        end)
        if not (ok and panel) then return false end
        kittyPanelMoveTo(to, "moveWindowNext", done)
        return true
    end
end

-- For the console: prints the socket, what the fast path sees, and what the
-- slow path's full `ls' says about the panel, with timings, without changing
-- anything on screen.
function kittyPanelInspect()
    local path = kittySocketPath()
    print("kittyPanelInspect: socket " .. tostring(path))
    if not path then return end

    local t0 = hs.timer.absoluteTime()
    kittyPanelFastState(path, function(st, why)
        local ms = (hs.timer.absoluteTime() - t0) / 1e6
        if st then
            print(string.format("kittyPanelInspect: fast: win=%s active=%s firstTab=%s strays=%d output=%q monitors=%s (%.1f ms)",
                                tostring(st.win), tostring(st.active), tostring(st.firstTab), #st.strays,
                                tostring(st.output), type(st.monitors) == "table" and table.concat(st.monitors, ", ") or "?",
                                ms))
        else
            print(string.format("kittyPanelInspect: fast: %s (%.1f ms)", tostring(why), ms))
        end

        local t1 = hs.timer.absoluteTime()
        kittyPanelList("unix:" .. path, function(st, err)
            local ms2 = (hs.timer.absoluteTime() - t1) / 1e6
            if not st then return print(string.format("kittyPanelInspect: slow: %s (%.0f ms)", err, ms2)) end
            print(string.format("kittyPanelInspect: slow: win=%s active=%s firstTab=%s strays=%d (%.0f ms)",
                                tostring(st.win), tostring(st.active), tostring(st.firstTab), #st.strays, ms2))
        end)
    end)
end
