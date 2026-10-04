-- Run from scripts root: lua lua/tests/pipe-test.lua
-- A bounded harness should enforce a timeout, since deadlock is the regression.
dofile('lua/pipe.lua')
local input = string.rep('input\0\255\n', 65536)
local program = [[
import sys
sys.stdout.buffer.write(b'o'*262144);sys.stdout.buffer.flush()
sys.stderr.buffer.write(b'e'*262144);sys.stderr.buffer.flush()
sys.stdout.buffer.write(sys.stdin.buffer.read());sys.stdout.buffer.flush()
sys.exit(17)
]]
local status, out, err = pipe_simple(input, '/usr/bin/env', 'python3', '-c', program)
assert(status == 17 and out == string.rep('o', 262144) .. input and err == string.rep('e', 262144))
-- A child that refuses input must not kill Lua with SIGPIPE or leak a zombie.
status, out, err = pipe_simple(input, '/bin/sh', '-c', 'printf early; exit 3')
assert(status == 3 and out == 'early' and err == '')
status = pipe_simple('', '/bin/sh', '-c', 'kill -TERM $$')
assert(status == 143)
status, out, err = pipe_simple('', '/nonexistent/synthetic-command')
assert(status == 127 and out == '' and err:match('cannot run'))
-- Environment and argv boundary, without using a garden.
local saved = pipe_simple
local seenInput, seenArgv
function pipe_simple(data, ...)
    seenInput, seenArgv = data, {...}
    return 17, ' output\n', 'error'
end
local text, errorText, code = brishz_eval_q({'--help', "it's data", ''}, {stdin=input, session='demo'})
assert(text == 'output' and errorText == 'error' and code == 17 and seenInput == input)
assert(table.concat(seenArgv, '\n'):find('brishz_in=MAGIC_READ_STDIN', 1, true))
assert(seenArgv[#seenArgv-3] == '--' and seenArgv[#seenArgv-2] == '--help' and seenArgv[#seenArgv] == '')
brishz_eval_bsh('typeset -g example=1')
local args = table.concat(seenArgv, '\n')
assert(args:find('brishz_noquote=y', 1, true) and args:find('brishz_session=bsh', 1, true))
brishz_eval_q_bg({'cat'}, {stdin=input})
assert(seenInput == input and table.concat(seenArgv, '\n'):find('brishz_async=y', 1, true))
brishz_eval_q({'cat'}, {outFile=true})
assert(table.concat(seenArgv, '\n'):find('/usr/local/bin/brishzq.zsh', 1, true))
pipe_simple = saved
print('PASS: concurrent binary pipes, early exit, signals, exec failure and Go argv/options')
