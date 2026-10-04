local posix = require("posix")

brishzgo_binary = os.getenv("BRISHZGO_BIN") or (os.getenv("HOME") .. "/go/bin/brishzgo")
-- Compatibility name for callers outside this module.
brishzq_binary = brishzgo_binary
---
---
-- froked from https://stackoverflow.com/a/16515126/1410221
--
-- Simple popen3() implementation
--
function popen3(path, ...)
    local r1, w1 = posix.pipe()
    local r2, w2 = posix.pipe()
    local r3, w3 = posix.pipe()

    -- All six ends. This used to test `w1 or r2 or r3', which passed as long
    -- as any one pipe had been made.
    local function closeAll()
        for _, fd in pairs({ r1, w1, r2, w2, r3, w3 }) do posix.close(fd) end
    end
    if not (r1 and w1 and r2 and w2 and r3 and w3) then
        closeAll()
        error("pipe() failed")
    end

    local pid, err = posix.fork()
    if pid == nil then
        closeAll()
        error("fork() failed: " .. tostring(err))
    end
    if pid == 0 then
        -- The child. Nothing here may raise: an error would unwind into the
        -- caller's Lua inside a forked copy of the whole process (Hammerspoon,
        -- when this runs there), which would then carry on as a second copy.
        -- So a failed exec says why on stderr and exits 127, as a shell does.
        posix.close(w1)
        posix.close(r2)
        posix.close(r3)
        posix.dup2(r1, posix.fileno(io.stdin))
        posix.dup2(w2, posix.fileno(io.stdout))
        posix.dup2(w3, posix.fileno(io.stderr))
        posix.close(r1)
        posix.close(w2)
        posix.close(w3)

        local _, execErr = posix.execp(path, table.unpack({...}))
        posix.write(2, "popen3: cannot run " .. tostring(path) .. ": " .. tostring(execErr) .. "\n")
        posix._exit(127)
    end

    posix.close(r1)
    posix.close(w2)
    posix.close(w3)

    return pid, w1, r2, r3
end

--
-- Pipe input into cmd + optional arguments and wait for completion
-- and then return status code, stdout and stderr from cmd.
--
function pipe_simple(input, cmd, ...)
    input = input or ""
    local pid, w, r, e = popen3(cmd, ...)
    local fds = {
        [w] = {events = {OUT = true}},
        [r] = {events = {IN = true}},
        [e] = {events = {IN = true}},
    }
    local stdout, stderr, offset = {}, {}, 1
    local function close(fd)
        if fds[fd] then posix.close(fd); fds[fd] = nil end
    end
    -- A child may close stdin early. Handle EPIPE without terminating Lua.
    local oldSigpipe = posix.signal(posix.SIGPIPE, posix.SIG_IGN)
    local ok, failure = pcall(function()
        for fd in pairs(fds) do
            local flags = assert(posix.fcntl(fd, posix.F_GETFL, 0))
            assert(posix.fcntl(fd, posix.F_SETFL, flags | posix.O_NONBLOCK))
        end
        if #input == 0 then close(w) end
        while next(fds) do
            local count, err, errno = posix.poll(fds, -1)
            if not count then
                if errno ~= posix.EINTR then error("poll: " .. tostring(err)) end
            else
                for fd, state in pairs(fds) do
                    local events = state.revents or {}
                    if fd == w then
                        if events.ERR or events.HUP then
                            close(w)
                        elseif events.OUT then
                            local n, writeErr, writeErrno = posix.write(w, input:sub(offset, offset + 65535))
                            if n then
                                offset = offset + n
                                if offset > #input then close(w) end
                            elseif writeErrno == posix.EPIPE then
                                close(w)
                            elseif writeErrno ~= posix.EAGAIN and writeErrno ~= posix.EINTR then
                                error("write: " .. tostring(writeErr))
                            end
                        end
                    elseif events.IN or events.HUP or events.ERR then
                        local buf, readErr, readErrno = posix.read(fd, 65536)
                        if buf then
                            if #buf == 0 then
                                close(fd)
                            else
                                local target = fd == r and stdout or stderr
                                target[#target + 1] = buf
                            end
                        elseif readErrno ~= posix.EAGAIN and readErrno ~= posix.EINTR then
                            error("read: " .. tostring(readErr))
                        end
                    end
                    if events.NVAL then error("poll: invalid file descriptor") end
                end
            end
        end
    end)
    for fd in pairs(fds) do close(fd) end
    posix.signal(posix.SIGPIPE, oldSigpipe)
    if not ok then posix.kill(pid, posix.SIGTERM) end
    local waitPid, cause, status
    repeat
        waitPid, cause, status = posix.wait(pid)
    until waitPid or status ~= posix.EINTR
    if not ok then error(failure) end
    assert(waitPid, "wait: " .. tostring(cause))
    if cause == "killed" then status = 128 + status end
    return status, table.concat(stdout), table.concat(stderr)
end

--- example
-- local my_in = "hi\n"
-- local my_cmd = "cat"
-- local my_args = {} -- no arguments
-- local my_status, my_out, my_err = pipe_simple(my_in, my_cmd, table.unpack(my_args))

-- print("s: " .. my_status .. "\nout:\n" .. my_out .. "\nerr:\n" .. my_err)
---
function exec_raw(cmd)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()
  return (s)
end
function exec(cmd)
  return trim1(exec_raw(cmd))
end
function trim1(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

--- * Brish
--- Running a command in the garden. Every one of these execs a client binary
--- directly with an argument list -- there is no shell in the middle, so there
--- is nothing to quote and nothing that can be mis-quoted into code.
---
--- brishzgo handles both forms: `_q' takes quoted argv, while the string
--- form uses brishz_noquote=y so named sessions retain their shell state.
--- Both report the command's status and stream its output. `_bg' uses the
--- Go client's detached worker, without waiting for the HTTP reply.
---
--- Explicit evalFile/outFile options keep the legacy client, since those
--- flags describe a file-based transport that the Go client does not expose.
--- This file stays plain Lua; Hammerspoon uses hs.task in core/helpers.lua.

local kBrishzqLegacy = "/usr/local/bin/brishzq.zsh"

local function brishzArgv(quoted, cmd, opts, async)
    opts = opts or {}
    local legacy = opts.evalFile or opts.outFile
    local vars = {"brishz_async=", "brishz_copy=", "brishz_c=",
                  "brishz_in=", "brishz_noquote="}
    if not quoted then vars[#vars + 1] = "brishz_noquote=y" end
    if opts.session then vars[#vars + 1] = "brishz_session=" .. opts.session end
    if opts.evalFile then vars[#vars + 1] = "brishz_eval_file_p=y" end
    if opts.outFile then vars[#vars + 1] = "brishz_out_file_p=y" end
    if opts.stdin ~= nil then
        vars[#vars + 1] = "brishz_in=" .. (legacy and opts.stdin or "MAGIC_READ_STDIN")
    end
    if async and not legacy then vars[#vars + 1] = "brishz_async=y" end

    local argv = {"/usr/bin/env"}
    for _, v in ipairs(vars) do argv[#argv + 1] = v end
    argv[#argv + 1] = legacy and kBrishzqLegacy or brishzgo_binary
    -- Leading -- avoids interpreting a literal command name as local help.
    if not legacy then argv[#argv + 1] = "--" end
    if quoted then
        for _, word in ipairs(cmd) do argv[#argv + 1] = tostring(word) end
    else
        argv[#argv + 1] = cmd
    end
    return argv, legacy
end

--- Fire and forget. Forks twice: the middle child exits at once and is reaped
--- here, so the grandchild is reparented and cannot come back as a zombie. The
--- caller waits only for that first fork, which is why this costs about 3ms
--- against 55 for waiting on the garden.
---
--- Nothing can be reported back, by construction -- not even a failure to
--- start. Use brishz_eval when the answer matters.
local function spawnDetached(argv)
    local path = argv[1]
    local args = {table.unpack(argv, 2)}

    local pid = posix.fork()
    assert(pid ~= nil, "fork() failed")
    if pid == 0 then
        local pid2 = posix.fork()
        if pid2 == 0 then
            local devnull = posix.open("/dev/null", posix.O_WRONLY)
            if devnull then
                posix.dup2(devnull, posix.fileno(io.stdout))
                posix.dup2(devnull, posix.fileno(io.stderr))
            end
            posix.execp(path, table.unpack(args))
            posix._exit(1)
        end
        posix._exit(0)
    end
    posix.wait(pid)
end

local function brishzRun(quoted, cmd, opts)
    local argv = brishzArgv(quoted, cmd, opts)
    local status, out, err = pipe_simple((opts and opts.stdin) or "", table.unpack(argv))
    return trim1(out or ""), err or "", status
end

--- Runs a command line in the garden and waits. Returns its output, its stderr
--- and its exit status -- all three, so that a failure is distinguishable from
--- empty output.
---
--- The status is the command's own for both the string and argv forms.
function brishz_eval(cmd, opts)
    return brishzRun(false, cmd, opts)
end

--- The same, with the command as a list: brishz_eval_q({"ecn", whatever}). Each
--- element is quoted for you, so `whatever' is data no matter what is in it.
--- Returns the command's own exit status, not the client's.
function brishz_eval_q(argv, opts)
    return brishzRun(true, argv, opts)
end

--- Does not wait for anything: not for the command, and not for the HTTP
--- round-trip either. Backgrounding the command *inside* the garden would not
--- achieve that, since the reply is still waited for.
local function brishzBackground(quoted, cmd, opts)
    local argv, legacy = brishzArgv(quoted, cmd, opts, true)
    if legacy then
        spawnDetached(argv)
    else
        -- This waits only for local launch. The detached worker owns stdin
        -- and its garden connection after the client parent exits.
        return pipe_simple((opts and opts.stdin) or "", table.unpack(argv))
    end
end

function brishz_eval_bg(cmd, opts)
    return brishzBackground(false, cmd, opts)
end

--- Argument-list form of brishz_eval_bg. Costs nothing extra, since nothing is
--- waited for; use it for anything with a value in it.
function brishz_eval_q_bg(argv, opts)
    return brishzBackground(true, argv, opts)
end

--- A shell that keeps its state between calls -- variables, cwd, anything --
--- because every call lands in the same named garden session. That is the only
--- thing this adds; being one session is also its hazard, since a command that
--- hangs there blocks every later call to it.
function brishz_eval_bsh(cmd, opts)
    opts = opts or {}
    opts.session = opts.session or "bsh"
    return brishz_eval(cmd, opts)
end
---
function mkdir(path)
    local status, stdout, stderr = pipe_simple("", "mkdir", "-p", path)
    return status == 0
end
---
