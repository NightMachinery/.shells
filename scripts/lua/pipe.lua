local posix = require("posix")

brishzq_binary = "/usr/local/bin/brishzq.zsh"
---
--- shell_quote used to live here and quoted with Lua's %q, which is Lua
--- quoting, not shell quoting -- it escapes for a Lua source file, not for a
--- shell word. Every caller that interpolated a value with it was one quote
--- away from a command that silently did nothing, or worse. Nothing quotes for
--- a shell any more: the brishz_eval_q functions below pass an argument list
--- and let the garden's own client do it.
---
-- froked from https://stackoverflow.com/a/16515126/1410221
--
-- Simple popen3() implementation
--
function popen3(path, ...)
    local r1, w1 = posix.pipe()
    local r2, w2 = posix.pipe()
    local r3, w3 = posix.pipe()

    assert((w1 ~= nil or r2 ~= nil or r3 ~= nil), "pipe() failed")

    local pid, err = posix.fork()
    assert(pid ~= nil, "fork() failed")
    if pid == 0 then
        posix.close(w1)
        posix.close(r2)
        posix.dup2(r1, posix.fileno(io.stdin))
        posix.dup2(w2, posix.fileno(io.stdout))
        posix.dup2(w3, posix.fileno(io.stderr))
        posix.close(r1)
        posix.close(w2)
        posix.close(w3)

        local ret, err = posix.execp(path, table.unpack({...}))
        assert(ret ~= nil, "execp() failed")

        posix._exit(1)
        return
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
    --
    -- Launch child process
    --
    local pid, w, r, e = popen3(cmd, table.unpack({...}))
    assert(pid ~= nil, "filter() unable to popen3()")

    --
    -- Write to popen3's stdin, important to close it as some (most?) proccess
    -- block until the stdin pipe is closed
    --
    posix.write(w, input)
    posix.close(w)

    local bufsize = 4096
    --
    -- Read popen3's stdout via Posix file handle
    --
    local stdout = {}
    local i = 1
    while true do
        buf = posix.read(r, bufsize)
        if buf == nil or #buf == 0 then break end
        stdout[i] = buf
        i = i + 1
    end

    --
    -- Read popen3's stderr via Posix file handle
    --
    local stderr = {}
    local i = 1
    while true do
        buf = posix.read(e, bufsize)
        if buf == nil or #buf == 0 then break end
        stderr[i] = buf
        i = i + 1
    end

    --
    -- Clean-up child (no zombies) and get return status
    --
    local wait_pid, wait_cause, wait_status = posix.wait(pid)

    return wait_status, table.concat(stdout), table.concat(stderr)
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
--- Two clients, because they answer two different needs:
---
---   brishz2.dash  43 lines of dash. Takes one string, a command line for the
---                 garden to evaluate. ~55ms. Enough when the command is a
---                 constant.
---   brishzq.zsh   the real client. Takes an argument list and quotes each
---                 element itself, so a value may contain quotes, semicolons,
---                 newlines or anything else without becoming code. Also
---                 returns the command's own exit code, forwards its stderr,
---                 and can pick a session. ~85ms: zsh startup plus quoting.
---
--- Hence the `_q' in the names: pay for quoting when a value is involved, not
--- when the command is a constant. Read the names as
--- brishz_eval[_q][_bg]: `_q' takes an argv table, `_bg' does not wait.
---
--- In the `_bg' forms the quoting is free, because nothing waits for the reply
--- at all. Prefer `_q' there whenever a value is interpolated.

local kBrishzDash = "/usr/local/bin/brishz2.dash"
local kBrishzq = "/usr/local/bin/brishzq.zsh"

--- Brish is configured through environment variables, and `env' sets them for
--- the child alone. posix.setenv would change Hammerspoon's own environment and
--- leak into every later call.
---
--- opts: session, evalFile (send the command as a file, for binary-unsafe
--- payloads), outFile (receive the output as a file), stdin.
local function brishzArgv(quoted, cmd, opts)
    opts = opts or {}
    -- Only the full client understands these, so asking for one sends a string
    -- command there too, in its no-quoting mode.
    local useZsh = quoted or opts.session or opts.evalFile or opts.outFile

    local vars = {}
    if useZsh and not quoted then
        -- Without this brishzq.zsh quotes the whole command line into a single
        -- word, and the garden looks for a command by that name.
        table.insert(vars, "brishz_noquote=y")
    end
    if opts.session then
        table.insert(vars, "brishz_session=" .. opts.session)
    end
    if opts.evalFile then
        table.insert(vars, "brishz_eval_file_p=y")
    end
    if opts.outFile then
        table.insert(vars, "brishz_out_file_p=y")
    end
    if opts.stdin then
        table.insert(vars, "brishz_in=" .. opts.stdin)
    end

    -- `env' is only worth an extra exec when there is something to set, and the
    -- common case -- a constant command line, no options -- sets nothing.
    local argv = {}
    if #vars > 0 then
        table.insert(argv, "/usr/bin/env")
        for _, v in ipairs(vars) do
            table.insert(argv, v)
        end
    end

    table.insert(argv, useZsh and kBrishzq or kBrishzDash)
    if quoted then
        for _, word in ipairs(cmd) do
            table.insert(argv, tostring(word))
        end
    else
        table.insert(argv, cmd)
    end

    return argv
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
    local status, out, err = pipe_simple("", table.unpack(argv))
    return trim1(out or ""), err or "", status
end

--- Runs a command line in the garden and waits. Returns its output, its stderr
--- and its exit status -- all three, because the old version returned only
--- stdout and a silent failure was indistinguishable from empty output.
---
--- The status here is the client's, so it reports that the call failed but not
--- what the command itself returned. brishz_eval_q gives the real code.
function brishz_eval(cmd, opts)
    return brishzRun(false, cmd, opts)
end

--- The same, with the command as a list: brishz_eval_q({"ecn", whatever}). Each
--- element is quoted for you, so `whatever' is data no matter what is in it.
--- Returns the command's own exit status, not the client's.
function brishz_eval_q(argv, opts)
    return brishzRun(true, argv, opts)
end

--- Does not wait, for anything: not for the command, and not for the HTTP
--- round-trip either. The `bg' in the old brishzevalbg only backgrounded the
--- command *inside* the garden, so Hammerspoon still blocked for the reply --
--- which made it slower than a plain synchronous call, not faster.
function brishz_eval_bg(cmd, opts)
    spawnDetached(brishzArgv(false, cmd, opts))
end

--- Argument-list form of brishz_eval_bg. Costs nothing extra, since nothing is
--- waited for; use it for anything with a value in it.
function brishz_eval_q_bg(argv, opts)
    spawnDetached(brishzArgv(true, argv, opts))
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
