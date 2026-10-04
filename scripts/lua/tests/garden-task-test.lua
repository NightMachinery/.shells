-- Run from the scripts root with Lua 5.4. All garden/UI calls are mocked.
local tasks, timers, bands = {}, {}, {}
local createFails, startFails = false, false
hs = {
    task = {new = function(bin, callback, args)
        if createFails then return nil end
        local task = {bin = bin, callback = callback, args = args, running = false,
                      env = {PATH = "/usr/bin", brishz_async = "ambient"}}
        function task:environment() return self.env end
        function task:setEnvironment(env) self.env = env; return self end
        function task:start() self.running = not startFails; return self.running end
        function task:isRunning() return self.running end
        function task:pid() return 2147483000 end
        function task:terminationReason() return self.reason or "exit" end
        tasks[#tasks + 1] = task
        return task
    end},
    timer = {
        doAfter = function(seconds, callback)
            local timer = {seconds = seconds, callback = callback}
            function timer:stop() self.stopped = true end
            timers[#timers + 1] = timer
            return timer
        end,
        doEvery = function() return {} end,
    },
}
alert_gateway = function(msg) bands[#bands + 1] = msg end
dofile("hammerspoon/core/helpers.lua")
local probeUp = true
gardenProbe = function(cb) cb(probeUp) end
local function countPins()
    local n = 0
    for _ in pairs(hsLiveObjects) do n = n + 1 end
    return n
end
local baseline = countPins()
local function complete(task, code, out, err)
    assert(task.bin == "/bin/sh")
    for i, text in ipairs({out or "", err or ""}) do
        local file = assert(io.open(task.args[i + 3], "wb"))
        file:write(text); file:close()
    end
    local paths = {task.args[4], task.args[5]}
    task.running = false
    task.callback(code, "", "")
    for _, path in ipairs(paths) do assert(not io.open(path, "rb")) end
    assert(countPins() == baseline, "task/timer leak")
end
local calls = 0
local bigOut = string.rep("€🎤\0", 40000)
local bigErr = string.rep("é", 80000)
assert(gardenTask("/bin/printf", {"%s", "literal ' quote"}, function(code, out, err)
    calls = calls + 1
    assert(code == 17 and out == bigOut and err == bigErr)
end, 30))
local task = tasks[#tasks]
assert(task.args[6] == "/bin/printf" and task.args[8] == "literal ' quote")
assert(task.env.PATH:find("/go/bin", 1, true))
complete(task, 17, bigOut, bigErr)
task.callback(17, "late", "late")
assert(calls == 1)

local argv = {"--help", "a b", "'\"$;"}
local outValue
brishz_eval_q_out_hs(argv, function(out) outValue = out end)
task = tasks[#tasks]
assert(task.args[6]:match("/brishzgo$"))
assert(task.args[7] == "--" and task.args[8] == "--help")
assert(task.args[9] == "a b" and task.args[10] == argv[3] and #argv == 3)
assert(task.env.brishz_async == "" and task.env.brishz_copy == "")
assert(task.env.brishz_c == "" and task.env.brishz_in == "" and task.env.brishz_noquote == "")
complete(task, 0, "  output \n", "")
assert(outValue == "output")

brishz_eval_out_hs("cd /tmp; print state", function(out) outValue = out end)
task = tasks[#tasks]
assert(task.args[8] == "cd /tmp; print state" and task.env.brishz_noquote == "y")
complete(task, 0, " state ", "")
assert(outValue == "state")

local failure
local opts = {quiet = true, onFail = function(code, notSent) failure = {code, notSent} end}
brishz_eval_out_hs("return 7", function(out) outValue = out end, "remote failure", opts)
complete(tasks[#tasks], 7, "", "expected")
assert(outValue == nil and failure[1] == 7 and not failure[2] and #bands == 0)
probeUp = false
garden_port_override = 7231
brishz_eval_q_hs({"printf", "sentinel"}, "closed garden", opts)
task = tasks[#tasks]
assert(task.env.GARDEN_PORT == "7231" and task.env.bshEndpoint == "http://127.0.0.1:7231")
complete(task, 7)
assert(failure[2] and #bands == 1)
garden_port_override = nil

for _, fail in ipairs({"create", "start"}) do
    createFails, startFails = fail == "create", fail == "start"
    calls = 0
    assert(not gardenTask("/absent", {}, function(code)
        calls = calls + 1; assert(code == -1)
    end, 30))
    assert(calls == 1 and countPins() == baseline)
    if fail == "start" then
        for i = 4, 5 do assert(not io.open(tasks[#tasks].args[i], "rb")) end
    end
end
createFails, startFails = false, false
calls = 0
gardenTask("/bin/sleep", {"5"}, function(code) calls = calls + 1; assert(code == -2) end, 0.1)
task = tasks[#tasks]
local execute, popen = os.execute, io.popen
io.popen = function(cmd)
    assert(cmd == "/bin/ps -Ao pid=,ppid=")
    return {lines = function()
        local lines, i = {"2147483000 1", "2147483001 2147483000"}, 0
        return function() i = i + 1; return lines[i] end
    end, close = function() end}
end
os.execute = function(cmd)
    assert(cmd == "/bin/kill -TERM 2147483000 2147483001 2>/dev/null")
    return true
end
timers[#timers].callback()
os.execute, io.popen = execute, popen
task.callback(15, "", "")
assert(calls == 1 and countPins() == baseline)
for i = 4, 5 do assert(not io.open(task.args[i], "rb")) end

calls = 0
gardenTask("/bin/sleep", {}, function(code) calls = calls + 1; assert(code == 143) end, 30)
task = tasks[#tasks]; task.reason = "interrupt"
complete(task, 15)
assert(calls == 1)

-- Collection after a reload/discard removes the capture files too.
local captured = taskCollectWithPath("/bin/true", function() end, {})
local paths = {captured.args[4], captured.args[5]}
captured.callback = nil
tasks[#tasks] = nil
captured = nil
collectgarbage("collect"); collectgarbage("collect")
for _, path in ipairs(paths) do assert(not io.open(path, "rb")) end
-- Transcription uses the same collector without touching UI, logs or clipboard.
ModalMode = {onScreenChange = function() end}
Screens = {on = function() end}
hyper_bind_v2 = function() end
brishzgo_binary = "/test path/brishzgo"
dofile("hammerspoon/core/stt.lua")
local updates, copied, bell = 0, nil, nil
updateIndicator = function() updates = updates + 1 end
mkdir = function() end
hs.pasteboard = {setContents = function(text) copied = text end}
brishz_eval_hs = function(cmd) bell = cmd end
local open = io.open
io.open = function(path, mode)
    if path:match("/logs/hs/stt/wav_files.txt$") then
        return {write = function() end, close = function() end}
    end
    return open(path, mode)
end
local input = "/tmp/audio with ' quote.wav"
processRecording(input, "en", "with-test")
task = tasks[#tasks]
assert(task.args[6] == brishzgo_binary and task.args[7] == "--")
assert(task.args[11] == "with-test" and task.args[13] == input)
assert(task.env.brishz_async == "" and task.env.brishz_noquote == "")
complete(task, 0, "  € transcript \n", "")
assert(copied == "€ transcript " and bell == "bell-transcription-ready" and updates == 2)
startFails = true
processRecording(input, "en", "with-test")
task = tasks[#tasks]
for i = 4, 5 do assert(not io.open(task.args[i], "rb")) end
assert(updates == 4)
io.open = open
print("Hammerspoon garden capture and STT tests passed")
