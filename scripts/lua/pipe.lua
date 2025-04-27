local posix = require("posix")

brishzq_binary = "/usr/local/bin/brishzq.zsh"
---
function shell_quote(str)
    -- Quote a string using Lua's %q format specifier
    -- Returns a quoted string suitable for reuse as a Lua string
    -- Example: hello"world -> "hello\"world"
    return string.format("%q", str)
end
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

function brishz(cmd)
  -- @duplicateCode/e0d4c801e9f8b4200d78468857610ae1 (mpv_shared.lua)
  cmdq = "brishz_eval_file_p=y /usr/local/bin/brishzq.zsh " .. cmd
  return exec(cmdq)
end

function brishzeval(cmd)
  local cmdq = ("/usr/local/bin/brishz.dash %q"):format(cmd)
  return exec(cmdq)
end
function brishzeval2Old(cmd)
  -- can not handle newlines well. %q quotes them wrongly. It also quotes \ wrongly, I think.
  local cmdq = ("brishz_quote=y /usr/local/bin/brishz.dash %q"):format(cmd)
  print("cmdq: " .. cmdq)
  return exec(cmdq)
end
function brishzeval2bg(cmd)
  brishzeval2("{ " .. cmd .. " } &>/dev/null &")
end
function brishzevalbg(cmd)
  brishzeval("{ " .. cmd .. " } &>/dev/null &")
end

function brishzeval2(cmd)
  local my_in = cmd
  local my_cmd = "/usr/local/bin/bsh.dash" -- susceptible to getting stuck as bsh.dash uses a single session
  local my_args = {} -- no arguments
  local my_status, my_out, my_err = pipe_simple(my_in, my_cmd, table.unpack(my_args))

  return my_out .. my_err
end

function brishzeval2_out(cmd)
  local my_in = cmd
  local my_cmd = "/usr/local/bin/bsh.dash" -- susceptible to getting stuck as bsh.dash uses a single session
  local my_args = {} -- no arguments
  local my_status, my_out, my_err = pipe_simple(my_in, my_cmd, table.unpack(my_args))

  return my_out
end
---
function mkdir(path)
    local status, stdout, stderr = pipe_simple("", "mkdir", "-p", path)
    return status == 0
end
---
