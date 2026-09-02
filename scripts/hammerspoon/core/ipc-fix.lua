--- * hs.ipc print recursion fix
--- `hs -c` wedges whenever anything prints to the console while the command is
--- being handled, and the usual trigger is nothing more exotic than a lazy
--- extension load: `-- Loading extension: pasteboard` is a console print like
--- any other.
---
--- Why, from the installed hs/ipc.lua (1.1.1):
---
---   * lines 65-87: hs.ipc replaces the global `print` with `printReplacement`,
---     which prints to the console and then mirrors the text to every
---     registered CLI instance. It guards against re-entering itself with
---     `module.print_inside(id)`, and when the guard trips it reports that with
---     `log.w("... already recursing, refusing request.")`.
---   * hs.logger formats through `hs.printf`, and `hs.printf` is
---     `print(string.format(...))` (_coresetup.lua:26) - a *global* `print`
---     lookup, so the warning re-enters `printReplacement` while the guard
---     counter is still raised, trips the guard, and warns again. Unbounded:
---     one measured run produced 39,984 lines in four seconds before the
---     client's receive timeout, and Hammerspoon has been seen to restart.
---   * line 373: a client's default console mode is the string "none", which is
---     truthy, so the mirroring branch runs for every `hs -c`, not only `hs -C`.
---     Only `-q` (line 384) skips it.
---
--- Upstream issue: https://github.com/Hammerspoon/hammerspoon/issues/3872
---
--- The fix is a correct guard rather than no guard: a depth counter, and a
--- nested print goes straight to the console and stops there. Nested output is
--- still visible, it just never reaches the mirror, so it cannot recurse - and
--- the warning that was the fuel is never emitted at all.
---
--- We deliberately do NOT change the "none" default to a falsy value, tempting
--- as it is. That is upstream's bug, and flipping it here would also change
--- what `hs -i` shows in this config; the guard fixes the hang without
--- touching what anyone asked to see.

--- ** rawPrint: the console print, without the global
--- _coresetup.lua:217-228 keeps the real Lua print as `hs.rawprint` (stdout)
--- and defines the *console* print as a local built on `hs._logmessage`, which
--- is not exported. So rebuild it here, same tostring-per-argument and same tab
--- join, and never route it through the global `print` - that is the thing we
--- are trying not to re-enter.
local logmessage = hs._logmessage

local function rawPrint(...)
    local vals = table.pack(...)
    for k = 1, vals.n do
        vals[k] = tostring(vals[k])
    end
    logmessage(table.concat(vals, "\t") .. "\n")
end

--- ** Is this really ipc's print?
--- Patching the wrong function would be worse than not patching: capturing the
--- plain console print as `ipcPrint` would leave the mirror unreachable and
--- every `hs -c` silent. _coresetup's console print closes over `logmessage`,
--- so an upvalue by that name identifies it; ipc's replacement closes over
--- `originalPrint` instead.
local function isConsolePrint(fn)
    if type(fn) ~= "function" then
        return false
    end
    if type(debug) ~= "table" or type(debug.getupvalue) ~= "function" then
        return false
    end

    local i = 1
    while true do
        local name = debug.getupvalue(fn, i)
        if not name then
            return false
        end
        if name == "logmessage" then
            return true
        end
        i = i + 1
    end
end

--- ** Apply, or say why not
--- A global so the CLI can check it: `hs -q -c 'return tostring(ipcFixApplied)'`.
ipcFixApplied = false

local function skip(why)
    rawPrint("ipc-fix: not applied: " .. why)
end

--- package.loaded rather than `hs.ipc`: reading a field off `hs` goes through
--- the lazy extension loader, which prints "-- Loading extension: ipc" - a
--- console print, from the module whose console prints we are here to fix.
local ipcModule = package.loaded["hs.ipc"]

if type(ipcModule) ~= "table" then
    skip("hs.ipc is not loaded")
elseif type(ipcModule.print_inside) ~= "function"
    or type(ipcModule.print_enter) ~= "function"
    or type(ipcModule.__registeredCLIInstances) ~= "table" then
    skip("hs.ipc does not look like the version this patches")
elseif type(logmessage) ~= "function" then
    skip("hs._logmessage is missing, so there is no console print to fall back to")
elseif isConsolePrint(print) then
    skip("hs.ipc has not replaced the global print")
else
    local ipcPrint = print

    --- Depth rather than a boolean: a print from inside the mirror path is the
    --- case we are handling, and it can nest more than once.
    local depth = 0

    print = function(...) -- luacheck: ignore
        if depth > 0 then
            --- Inside the mirror already. The console still gets the line; the
            --- mirror does not, which is precisely what makes recursion
            --- impossible instead of merely reported.
            rawPrint(...)
            return
        end

        depth = depth + 1
        --- pcall so a raising sendMessage - a client that died mid-command,
        --- say - cannot leave the counter stuck above zero and silence every
        --- later mirror for the rest of this config's life.
        local ok, err = pcall(ipcPrint, ...)
        depth = depth - 1

        if not ok then
            rawPrint("ipc-fix: print failed: " .. tostring(err))
        end
    end

    ipcFixApplied = true
end
