--- * Core helpers

function nop()
    alert_gateway("repeating", { id = "nop" })
end
---
function sanitizeLocationTable(location)
    local sanitized = {}
    for key, value in pairs(location) do
        -- Exclude keys that start with '__' (like '__luaSkinType')
        if type(key) == "string" and not key:match("^__") then
            sanitized[key] = value
        end
    end
    return sanitized
end

--- Prints *and* returns the JSON. The shell reads it through
--- [agfi:h-hammerspoon-eval], which now passes `-q' to hs.ipc - and `-q'
--- suppresses print but not the command's return value, so a function that only
--- printed would come back empty. The print stays for use from the console.
function printLocation()
    local location = hs.location.get()
    if location then
        -- Sanitize the location table to remove non-serializable fields
        local sanitizedLocation = sanitizeLocationTable(location)

        -- Encode the sanitized table as a JSON string with pretty printing
        local success, jsonOrError = pcall(hs.json.encode, sanitizedLocation, true)

        if success then
            print(jsonOrError)
            return jsonOrError
        else
            -- If encoding fails, print the error message
            print("Error encoding location data to JSON:", jsonOrError)
            return
        end
    else
        print("No location data available.")
        return
    end
end

function active_app_re_p(pattern, case_mode)
    local activeApp = hs.application.frontmostApplication()
    local activeAppName = activeApp:name()

    if case_mode == nil then
        case_mode = "smart"
    end

    local compiledPattern
    if case_mode == "smart" then
        if pattern:match("%u") then
            -- If the pattern contains uppercase letters, use case-sensitive matching
            compiledPattern = rex.new(pattern)
        else
            -- If the pattern contains only lowercase letters, use case-insensitive matching
            compiledPattern = rex.new(pattern, rex.flags().CASELESS)
        end
    elseif case_mode == "sensitive" then
        -- Use case-sensitive matching
        compiledPattern = rex.new(pattern)
    elseif case_mode == "insensitive" then
        -- Use case-insensitive matching
        compiledPattern = rex.new(pattern, rex.flags().CASELESS)
    else
        error("Invalid case_mode. Valid values are 'smart', 'sensitive', or 'insensitive'.")
    end

    return compiledPattern:match(activeAppName) ~= nil
end

function copyToClipboard(text)
    hs.pasteboard.setContents(text)
end

function doEscape()
    hs.eventtap.keyStroke({}, "escape")
end

function doCopy()
    hs.eventtap.keyStroke({"cmd"}, "c")
end

function doPaste()
    hyper_exit()

    hs.eventtap.keyStroke({"cmd"}, "v")
end
---
function timerifyFn(params)
    -- We need to create a new timer for each call/press and make sure it
    -- doesn't get garbage-collected:
    local enabled_p = params.enabled_p
    if enabled_p == nil then
        enabled_p = true
    end
    local fn = params.fn
    local delay = params.delay or 0

    if enabled_p then
        return function()
            local timer
            timer = hs.timer.doAfter(delay, function()
                                         timer = nil
                                         fn()
            end)
        end
    else
        return fn
    end
end
---
function tableShallowCopy(orig)
    local copy

    local orig_type = type(orig)
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in pairs(orig) do

            copy[orig_key] = orig_value

        end
    else
        -- Raise error
        error("tableShallowCopy: Can't copy a " .. orig_type)
    end

    return copy
end

--- * Running zsh in the brish garden
--
-- Hammerspoon runs Lua on the main thread, which is also its UI and event
-- thread. Anything blocking here freezes hotkeys, window management and every
-- keystroke for the duration. Compare core/redis.lua, which documents a
-- previous version that could freeze the machine for up to 50 minutes.
--
-- Use hs.task for garden calls so a slow request cannot block the main
-- thread. Both string and argv forms now run the streaming Go client.
-- taskWithPath also supports standalone tasks that need Homebrew or locally
-- installed Go binaries under launchd's minimal PATH.
local EXTRA_PATHS = "/opt/homebrew/bin:/usr/local/bin:" .. os.getenv("HOME") .. "/go/bin"

function taskWithPath(bin, callback, args)
    local task = hs.task.new(bin, callback, args)
    if not task then return nil end

    -- Repair PATH rather than replacing the environment wholesale: brishz needs
    -- HOME, and setEnvironment replaces the table entirely.
    local env = task:environment() or {}
    env.PATH = EXTRA_PATHS .. ":" .. (env.PATH or "/usr/bin:/bin:/usr/sbin:/sbin")
    task:setEnvironment(env)

    return task
end

--- Collect output without pipe limits or hs.task's UTF-8 chunk decoding.
--- Streaming callbacks in hs.task decode each read separately, losing a chunk
--- that splits a Unicode character. Redirect the child straight into private
--- temporary files instead. The shell execs the binary, so PID and signals
--- still belong to the client. Completion reads the files as Lua byte strings.
--- Returns task, discard; call discard when creation/start fails or on timeout.
function taskCollectWithPath(bin, callback, args)
    local paths = setmetatable({}, {__gc = function(files)
        for _, path in pairs(files) do os.remove(path) end
    end})
    local finished = false
    local function discard()
        if finished then return end
        finished = true
        for i, path in pairs(paths) do os.remove(path); paths[i] = nil end
    end
    for i = 1, 2 do
        -- os.tmpname uses mkstemp (0600) on this POSIX host.
        local ok, path = pcall(os.tmpname)
        if not ok then discard(); return nil, discard end
        paths[i] = path
    end
    local function read(path)
        local file, err = io.open(path, "rb")
        if not file then return nil, err end
        local data, readErr = file:read("*a")
        file:close()
        return data, readErr
    end
    local argv = {"-c", 'out=$1; err=$2; shift 2; exec "$@" >"$out" 2>"$err"',
                  "task-capture", paths[1], paths[2], bin}
    for _, arg in ipairs(args or {}) do argv[#argv + 1] = arg end
    local task = taskWithPath("/bin/sh", function(code, out, err)
        if finished then return end
        local capturedOut, outErr = read(paths[1])
        local capturedErr, errErr = read(paths[2])
        discard()
        if not capturedOut or not capturedErr then
            code = -1
            err = (err or "") .. " capture failed: " .. tostring(outErr or errErr)
        end
        callback(code, (capturedOut or "") .. (out or ""),
                 (capturedErr or "") .. (err or ""))
    end, argv)
    if not task then discard() end
    return task, discard
end

--- ** Keeping callback objects alive, without leaking them
--
-- An hs.timer, hs.task or hs.socket that only a local refers to is
-- collected, and its callback then never fires. The opposite mistake is
-- quieter: Hammerspoon keeps each such callback in the Lua registry until
-- its object is collected, so a callback that can still reach its own
-- object (as an upvalue, or through a table it captures) keeps both alive
-- forever. Measured 2026-09-30 with 3000 doAfter timers, 200 tasks and 200
-- sockets: every self-referencing one survived a full collection, and every
-- plain one was collected.
--
-- So an object in flight is pinned in a table under a key, a fresh empty
-- table, and its callbacks capture the key, never the object. Unpinning it
-- leaves nothing that reaches it.

hsLiveObjects = {}

--- Pins `obj' and returns its key.
function hsPin(obj)
    local key = {}
    hsLiveObjects[key] = obj
    return key
end

--- The object pinned under `key', or nil once it is unpinned.
function hsPinned(key)
    return hsLiveObjects[key]
end

--- Unpins and returns the object under `key' (nil if it was already).
function hsUnpin(key)
    if key == nil then return nil end
    local obj = hsLiveObjects[key]
    hsLiveObjects[key] = nil
    return obj
end

--- hs.timer.doAfter(seconds, fn), pinned until it fires or is cancelled.
--- Returns a key for hsCancel.
function hsAfter(seconds, fn)
    local key = {}
    hsLiveObjects[key] = hs.timer.doAfter(seconds, function()
        hsLiveObjects[key] = nil
        fn()
    end)
    return key
end

--- Stops a timer from hsAfter. Harmless when it has fired already.
function hsCancel(key)
    local t = key and hsUnpin(key)
    if t then t:stop() end
end

--- ** BrishGarden calls that say when it is down
--
-- On 2026-09-29 a tmux crash took BrishGarden down for 15 hours, and every
-- hotkey that goes through it failed with nothing but a console line. The
-- helpers below make such a failure visible. They do not run the command
-- anywhere else: the hotkeys that matter have their own garden-free code
-- (the kitty panel is one), and the rest wait for the garden to be fixed.
-- The terms:
--
--   * "Not sent": a possible connection/setup error (2, 3, 6, 7 or 26)
--     accompanied by a failed TCP probe. Go also returns remote exit codes,
--     so an exit code alone cannot identify a connection failure. This is a
--     best-effort outage check, not proof if the garden dies after execution.
--     A client killed by a signal reports 128 plus the signal number.
--   * The "garden-down band": a warn band (id garden-down) naming the
--     hotkey that did not run, or the call that failed. While the liveness
--     probe (gardenLivenessCheck, every 60 s) finds the garden down, it
--     keeps a band up saying so; it says again when the garden is back.
--
-- Every client call is killed, with its whole process tree, if it is still
-- running after `opts.timeout' seconds (default 30), so a wedged garden
-- cannot pile tasks up. Capturing streamed output in files avoids full pipes.
-- With a streaming garden, killing the Go client cancels the remote command.
--
-- `opts', for all four helpers:
--   quiet     true: no console line for ordinary failures.
--   timeout   seconds before a client call is killed (default 30).
--   onFail    called as onFail(code, notSent) once when the call failed,
--             after the band, for a caller with a native way to do the job
--             or state to take back. notSent means a possible connection
--             error with a failed probe, subject to the race described above.

local gardenBrishzgo = os.getenv("BRISHZGO_BIN")
if not gardenBrishzgo or gardenBrishzgo == "" then
    gardenBrishzgo = os.getenv("HOME") .. "/go/bin/brishzgo"
end

function gardenClientEnvironment(noquote)
    local env = {brishz_async = "", brishz_copy = "", brishz_c = "",
                 brishz_in = "", brishz_noquote = noquote and "y" or ""}
    if garden_port_override then
        env.GARDEN_PORT = tostring(garden_port_override)
        env.bshEndpoint = "http://127.0.0.1:" .. tostring(garden_port_override)
    end
    return env
end

-- The garden's port, as the clients compute it (${GARDEN_PORT:-7230}).
-- garden_port_override points the helpers, their clients and the probe at
-- another port; the tests use it to play a dead garden without touching the
-- real one.
local function gardenPort()
    return tonumber(garden_port_override or os.getenv("GARDEN_PORT") or "") or 7230
end

local gardenNotSentCodes = { [2] = true, [3] = true, [6] = true, [7] = true, [26] = true }

--- cb(true) when something accepts a TCP connection on the garden's port
--- within a second, cb(false) otherwise. hs.socket reports only a connect
--- that succeeds, so a refusal is the absence of one.
function gardenProbe(cb)
    local timer
    local sockKey = hsPin(hs.socket.new())
    local function finish(up)
        local sock = hsUnpin(sockKey)
        if not sock then return end
        hsCancel(timer)
        pcall(function() sock:disconnect() end)
        cb(up)
    end
    timer = hsAfter(1, function() finish(false) end)
    hsPinned(sockKey):connect("127.0.0.1", gardenPort(), function() finish(true) end)
end

--- cb(true) for a possible connection/setup failure and a failed TCP probe;
--- cb(false) otherwise. See the outage race described above.
function gardenNotSentP(bin, code, cb)
    if not gardenNotSentCodes[code] then return cb(false) end
    -- A remote command can return any of these statuses too.
    return gardenProbe(function(up) cb(not up) end)
end

-- Whether the garden answered the last call or probe: true, false, or nil
-- before the first.
gardenUp = nil

local function gardenBand(msg, seconds)
    alert_gateway(msg, { id = "garden-down", color = "warn", seconds = seconds })
end

-- Terminates `rootPid' and everything below it. hs.task:terminate() alone
-- would leave any child processes behind. Reads `ps' synchronously; only a
-- call has already timed out.
local function gardenKillTree(rootPid, label)
    if type(rootPid) ~= "number" or rootPid <= 1 then return end

    local children = {}
    local ps = io.popen("/bin/ps -Ao pid=,ppid=")
    if ps then
        for line in ps:lines() do
            local pid, ppid = line:match("^%s*(%d+)%s+(%d+)")
            if pid then
                pid, ppid = tonumber(pid), tonumber(ppid)
                children[ppid] = children[ppid] or {}
                table.insert(children[ppid], pid)
            end
        end
        ps:close()
    end

    local pids, queue = {}, { rootPid }
    while #queue > 0 do
        local pid = table.remove(queue, 1)
        pids[#pids + 1] = pid
        for _, c in ipairs(children[pid] or {}) do queue[#queue + 1] = c end
    end

    local list = table.concat(pids, " ")
    print(label .. ": timed out; terminating pids " .. list)
    os.execute("/bin/kill -TERM " .. list .. " 2>/dev/null")
end

--- Runs `bin args' as an hs.task with the extra PATH, plus `env' on top of
--- its environment, and calls cb(code, stdout, stderr) exactly once.
--- `timeout' (seconds, or nil for none) kills the process tree of a task
--- still running then, and reports code -2. A task that cannot start
--- reports -1, and one killed by a signal 128 plus the signal. Returns
--- whether it started.
function gardenTask(bin, args, cb, timeout, env, label)
    label = label or bin
    local taskKey, timer, discardCapture

    local function finish(code, out, err)
        if taskKey then
            if not hsUnpin(taskKey) then return end
        end
        hsCancel(timer)
        if discardCapture then discardCapture() end
        cb(code, out or "", err or "")
    end

    local task
    task, discardCapture = taskCollectWithPath(bin, function(code, out, err)
        local t = hsPinned(taskKey)
        if t and t:terminationReason() == "interrupt" then
            err = (err or "") .. " (killed by signal " .. tostring(code) .. ")"
            code = 128 + code
        end
        finish(code, out, err)
    end, args)
    if not task then
        finish(-1, "", "could not create a task for " .. bin)
        return false
    end
    if env then
        local e = task:environment() or {}
        for k, v in pairs(env) do e[k] = v end
        task:setEnvironment(e)
    end
    taskKey = hsPin(task)
    if not task:start() then
        finish(-1, "", "could not start " .. bin)
        return false
    end

    if timeout then
        timer = hsAfter(timeout, function()
            local t = hsPinned(taskKey)
            if t and t:isRunning() then gardenKillTree(t:pid(), label) end
            finish(-2, "", "timed out after " .. timeout .. " s")
        end)
    end
    return true
end

-- One garden call: `bin args', then the policy above on failure.
-- onResult(code, stdout, stderr) exactly once.
local function gardenCall(args, label, opts, onResult, noquote)
    opts = opts or {}
    local bin = gardenBrishzgo
    local env = gardenClientEnvironment(noquote)
    local clientArgs = {"--"}
    for _, arg in ipairs(args) do clientArgs[#clientArgs + 1] = arg end

    gardenTask(bin, clientArgs, function(code, out, err)
        if code == 0 then
            gardenUp = true
            return onResult(0, out, err)
        end

        gardenNotSentP(bin, code, function(notSent)
            if notSent then
                gardenUp = false
                print(label .. ": BrishGarden down; not run")
                gardenBand("BrishGarden down: " .. label .. " did not run; run ivy", 30)
            else
                if not opts.quiet then
                    print(label .. ": " .. bin .. " exited " .. tostring(code) .. ": " .. err)
                end
                -- Remote nonzero statuses belong to the command. Local start
                -- and timeout failures still warrant a visible band.
                if code < 0 then
                    gardenBand("BrishGarden call failed: " .. label .. " (exit " .. tostring(code) .. ")")
                end
            end

            if opts.onFail then
                local ok, e = pcall(opts.onFail, code, notSent)
                if not ok then print(label .. ": onFail raised: " .. tostring(e)) end
            end
            onResult(code, out, err)
        end)
    end, opts.timeout or 30, env, label)
end

--- Runs `cmd' in the garden without blocking, and says so when it fails.
--- `label' prefixes the console lines and bands, so you can tell which
--- caller's call failed; `opts' is described above.
---
--- Reporting the failure is the thing brishz_eval_bg cannot do at all: it
--- launches a detached worker, so nobody is left to notice a remote failure.
---
--- Lives here rather than in lua/pipe.lua because hs.task is Hammerspoon's;
--- pipe.lua is plain Lua over posix and stays that way.
function brishz_eval_hs(cmd, label, opts)
    label = label or "brishz_eval_hs"
    gardenCall({ cmd }, label, opts, function() end, true)
end

--- The argument-list form: brishz_eval_q_hs({"some-hook", value, other}). Each
--- element is quoted by the client, so a value may contain anything at all and
--- still arrive as one word rather than as code. Use it whenever a value is
--- interpolated. String commands still go through brishz_eval_hs, with
--- brishz_noquote=y to preserve shell code and named-session state.
--- Both forms now report the remote command's exit status.
function brishz_eval_q_hs(argv, label, opts)
    label = label or "brishz_eval_q_hs"
    gardenCall(argv, label, opts, function() end)
end

--- Like brishz_eval_hs, but hands stdout to a callback instead of discarding
--- it. The only async way to read a value out of the garden: brishz_eval
--- returns one, but it blocks the main thread for the whole round trip, and a
--- DDC read alone is ~340ms -- long enough to stall an eventtap callback, which
--- macOS answers by disabling the tap.
---
--- The callback gets the trimmed stdout, or nil if the call failed; it is
--- called exactly once on every path, timeout included. It is also how a
--- caller can serialise garden work: keep one call in flight and send the
--- next from the callback. See hyperBrightnessStep in
--- core/window-media-bindings.lua, where doing that is the difference between
--- ten brightness steps landing and one. Captured output has no pipe limit.
function brishz_eval_out_hs(cmd, callback, label, opts)
    label = label or "brishz_eval_out_hs"
    gardenCall({ cmd }, label, opts, function(code, out)
        if code ~= 0 then return callback(nil) end
        callback((tostring(out or "")):gsub("^%s+", ""):gsub("%s+$", ""))
    end, true)
end

--- The argument-list form of brishz_eval_out_hs. Each value arrives as one
--- word; the callback gets trimmed stdout, or nil on call/command failure.
function brishz_eval_q_out_hs(argv, callback, label, opts)
    label = label or "brishz_eval_q_out_hs"
    gardenCall(argv, label, opts, function(code, out)
        if code ~= 0 then return callback(nil) end
        callback((tostring(out or "")):gsub("^%s+", ""):gsub("%s+$", ""))
    end)
end

--- The liveness probe: one TCP connect every 60 s, so a dead garden is
--- noticed within a minute rather than at the next failed hotkey (the
--- 2026-09-29 outage went unnoticed for 15 hours). While the garden is down
--- every probe re-issues the band with a lifetime longer than the interval,
--- so it stays on screen until the garden is back; an unchanged message on
--- the same id only extends the band. Its own state, not gardenUp, decides
--- when to say "back": a call that succeeded in between must not swallow it.
--- Global, so the timer is not collected.
local gardenProbeUp = nil

function gardenLivenessCheck()
    gardenProbe(function(up)
        local was = gardenProbeUp
        gardenProbeUp = up
        gardenUp = up
        if not up then
            gardenBand("BrishGarden is down: hotkeys that need it do nothing; run ivy", 90)
        elseif was == false then
            alert_gateway("BrishGarden is back", { id = "garden-down" })
        end
    end)
end

gardenLivenessTimer = hs.timer.doEvery(60, gardenLivenessCheck)
hsAfter(5, gardenLivenessCheck)

--- * _
function has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end
