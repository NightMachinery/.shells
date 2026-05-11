---- * Redis
local redis = require("redis")
local maxRetries = 300
local retryDelay = 10 -- seconds

function connectToRedis()
    return redis.connect('127.0.0.1', 6379)
end

function connectToRedisWithRetries(maxRetries, retryDelay)
    local redisClient
    local success, retVal
    for attempt = 1, maxRetries do
        success, retVal = pcall(connectToRedis)

        if success then
            return true, retVal -- retVal here is the redisClient on success
        else
            print("connectToRedisWithRetries: Attempt " .. attempt .. " failed: " .. retVal) -- retVal is the error message on failure

            print("connectToRedisWithRetries: Retrying in " .. retryDelay .. " seconds...")
            hs.timer.usleep(retryDelay * 1000000) -- usleep takes microseconds

        end
    end
    return false, nil
end

local success, redisClient = connectToRedisWithRetries(maxRetries, retryDelay)
if success then
    -- print("Connected to Redis successfully!")
    -- You can now use redisClient to interact with Redis
else
    print("Failed to connect to Redis after " .. maxRetries .. " attempts.")
end

--- * Redis-backed mode state
redisModalityUpdateP = false
-- redisModalityUpdateP = true

-- @works but not needed for now
function redisActivateMode(mode)
    if redisActivateMode then
        redisClient:set(mode, true)
    end
end
function redisDeactivateMode(mode)
    if redisActivateMode then
        redisClient:set(mode, false)
    end
end
