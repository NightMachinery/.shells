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

function printLocation()
    local location = hs.location.get()
    if location then
        -- Sanitize the location table to remove non-serializable fields
        local sanitizedLocation = sanitizeLocationTable(location)

        -- Encode the sanitized table as a JSON string with pretty printing
        local success, jsonOrError = pcall(hs.json.encode, sanitizedLocation, true)

        if success then
            print(jsonOrError)
        else
            -- If encoding fails, print the error message
            print("Error encoding location data to JSON:", jsonOrError)
        end
    else
        print("No location data available.")
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
-- So watchers must NOT use brishzeval2bg. Its name is misleading: the trailing
-- `&' backgrounds the command inside the garden, but Hammerspoon still
-- synchronously waits for the round-trip, because brishzeval2 goes through
-- pipe_simple (lua/pipe.lua), which does blocking posix.read loops and then
-- posix.wait. Measured on this machine with hs.timer.absoluteTime:
--
--   brishzeval2bg("true")                            780.6 ms cold, 51.9 ms warm
--   hs.task.new("brishz2.dash", nil, {"true"}):start()          1.5 ms
--
-- hs.task is genuinely asynchronous (NSTask, callback on completion), and is
-- already the idiom elsewhere in this config: core/hyper-mode.lua,
-- core/mouse.lua, and core/choosers.lua all use it, the last one to run
-- brishz2.dash exactly like this.

-- hs.task does NOT inherit an interactive PATH. It gets the bare launchd one,
-- /usr/bin:/bin:/usr/sbin:/sbin, and brishz.dash shells out to jq, which lives
-- in /opt/homebrew/bin. Without this the task exits 22 with
-- "brishz.dash: 41: jq: not found" and, because a nil callback discards both
-- streams, fails completely silently. Same class of bug as the PATH in
-- launchers/audio-guard/com.user.audio-guard.plist.
local BREW_PATHS = "/opt/homebrew/bin:/usr/local/bin"

function taskWithPath(bin, callback, args)
    local task = hs.task.new(bin, callback, args)
    if not task then return nil end

    -- Repair PATH rather than replacing the environment wholesale: brishz needs
    -- HOME, and setEnvironment replaces the table entirely.
    local env = task:environment() or {}
    env.PATH = BREW_PATHS .. ":" .. (env.PATH or "/usr/bin:/bin:/usr/sbin:/sbin")
    task:setEnvironment(env)

    return task
end

-- Runs `cmd' in the garden without blocking. `label' only prefixes the failure
-- log, so you can tell which watcher's call failed.
--
-- A nil callback would discard the exit code and both streams, which is how the
-- missing-jq failure above stayed invisible. Log failures instead.
function runInGarden(cmd, label)
    label = label or "runInGarden"

    local task = taskWithPath("/usr/local/bin/brishz2.dash", function(exitCode, _, stdErr)
        if exitCode ~= 0 then
            print(label .. ": brishz2.dash exited " .. tostring(exitCode) ..
                      ": " .. tostring(stdErr))
        end
    end, {cmd})

    if task then task:start() end
end

--- * _
function has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end
