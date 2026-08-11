---- * Redis
local redis = require("redis")

local maxRetries = 30
local retryDelay = 10 -- seconds

local redisClient = nil

-- Read the shared secret. See docs/redis-hardening.md; the zsh side is
-- h-redis-auth-ensure, which is also what creates this file.
local function redisAuthGet()
    local f = io.open(os.getenv("HOME") .. "/.redis-auth", "r")
    if not f then return nil end

    local secret = f:read("*l")
    f:close()

    if secret == nil or secret == "" then return nil end
    return secret
end

function connectToRedis()
    local client = redis.connect('127.0.0.1', 6379)

    -- Authenticating is a separate step from connecting, and easy to skip by
    -- accident: redis rejects the *commands*, not the connection, so
    -- redis.connect succeeds against a password-protected server and only the
    -- first :set fails, with NOAUTH. Without this, every hyper-key press would
    -- fail a write, null the client and schedule a reconnect.
    local secret = redisAuthGet()
    if secret then
        -- pcall because AUTH against a server that has *no* password
        -- configured is an error, and raising it here would turn a working
        -- unprotected redis into a connection that retries forever. If the
        -- secret is genuinely wrong, the commands fail later and the existing
        -- reconnect path handles it.
        local ok, err = pcall(function()
            client:auth(secret)
        end)
        if not ok then
            print("redis: auth failed (continuing unauthenticated): " .. tostring(err))
        end
    end

    return client
end

-- Connect without ever blocking Hammerspoon's Lua thread.
--
-- The previous version looped up to 300 times calling hs.timer.usleep(10s)
-- between attempts. usleep blocks the single Lua thread that also services
-- every event tap and hotkey, so a Redis that was down or slow at startup
-- could freeze Hammerspoon -- and therefore every keystroke on the machine --
-- for up to 50 minutes, with no visible cause. The comment in hyper-mode.lua
-- already says as much: "I try to avoid hs.timer.usleep, because it basically
-- hangs Hammerspoon."
--
-- Retrying on a timer keeps the thread free between attempts.
local function scheduleRedisConnect(attempt)
    attempt = attempt or 1

    local ok, result = pcall(connectToRedis)
    if ok then
        redisClient = result
        return
    end

    if attempt >= maxRetries then
        print("redis: giving up after " .. attempt .. " attempts: " .. tostring(result))
        return
    end

    print("redis: attempt " .. attempt .. " failed (" .. tostring(result) ..
              "); retrying in " .. retryDelay .. "s")
    hs.timer.doAfter(retryDelay, function()
        scheduleRedisConnect(attempt + 1)
    end)
end

scheduleRedisConnect()

--- * Redis-backed mode state
redisModalityUpdateP = false
-- redisModalityUpdateP = true

-- Guard on the flag, not on the function's own name.
--
-- These previously read `if redisActivateMode then`, which tests whether the
-- function itself exists -- always true -- so the writes ran on every hyper
-- press regardless of redisModalityUpdateP being false. The nil check on
-- redisClient matters too: without it, a failed connection turns every mode
-- change into a Lua error.
local function redisSetMode(mode, value)
    if not redisModalityUpdateP then return end
    if not redisClient then return end

    local ok, err = pcall(function()
        redisClient:set(mode, value)
    end)
    if not ok then
        print("redis: set(" .. tostring(mode) .. ") failed: " .. tostring(err))
        redisClient = nil          -- drop a dead connection rather than retrying it per keypress
        scheduleRedisConnect()
    end
end

function redisActivateMode(mode)
    redisSetMode(mode, true)
end

function redisDeactivateMode(mode)
    redisSetMode(mode, false)
end
