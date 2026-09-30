qview_mode = ModalMode.createAppFocusMode{
    name = "qview",
    appName = "qView",
    bundleID = "com.interversehq.qView",
    auto_trigger_p = false,
    overlay = {
        text = "qView",
        position = "left-top",
        overlayMargin = 0,
    },
}

ModalMode.installGlobals(qview_mode, "qview")

-- The file qView shows, read when the key is pressed. The keys below used to
-- leave that to zsh (qview-path-get, inside the garden call), so a key
-- pressed just before the next image could tag, or trash, the one after it.
-- Asked of qView's IPC server over hs.socket instead: one JSON request per
-- line, one compact JSON reply per line (src/qvipcserver.cpp in the qView
-- fork), on the socket h-qview-open starts it with.
-- @duplicateCode/ce70dc7e3ffc07c958b26ca897aaa904: qview-path-get in
-- zshlang/auto-load/others/pictures/qview.zsh: the socket path and the
-- request.
local qviewSocketPath = (function()
    local f = io.popen("/usr/bin/id -u")
    local uid = f and f:read("*l")
    if f then f:close() end
    return "/tmp/qview-" .. tostring(uid) .. ".sock"
end)()

-- cb(path), or cb(nil, why), exactly once.
function qviewPathGet(cb)
    local timer
    local sock = hs.socket.new(function(raw)
        local ok, reply = pcall(hs.json.decode, raw or "")
        if ok and type(reply) == "table" and reply.ok and type(reply.path) == "string" and reply.path ~= "" then
            return cb(reply.path)
        end
        cb(nil, "unreadable reply")
    end)
    if not sock then return cb(nil, "no socket") end

    local key = hsPin(sock)
    local cbOnce = cb
    cb = function(...)
        local s = hsUnpin(key)
        if not s then return end
        hsCancel(timer)
        pcall(function() s:disconnect() end)
        cbOnce(...)
    end
    -- hs.socket reports a failed connect only in its own log.
    timer = hsAfter(1, function() cb(nil, "qView is not answering on " .. qviewSocketPath) end)
    sock:connect(qviewSocketPath, function()
        local s = hsPinned(key)
        if not s then return end
        s:write('{"method":"currentFilePath"}\n')
        s:read("\n")
    end)
end

-- Runs `cmd <the current file>' in the garden, through hs-reval-alert as the
-- zsh keys did, with the path captured now.
local function qviewOnFile(cmd)
    qviewPathGet(function(path, why)
        if not path then
            alert_gateway("qView: no current file (" .. tostring(why) .. ")", { color = "warn" })
            return
        end
        brishz_eval_q_hs({ "hs-reval-alert", cmd, path }, "qview " .. cmd)
    end)
end

qview_bind_v2{
    mods = {"shift"},
    key = "escape",
    auto_trigger_p = false,
    pressedfn = qview_exit,
}

qview_bind_v2{
    -- mods={},
    key="g",
    pressedfn=function()
        qviewOnFile("green")
    end,
}

qview_bind_v2{
    -- mods={},
    key="b",
    pressedfn=function()
        qviewOnFile("blue")
    end,
}

qview_bind_v2{
    -- mods={},
    key="r",
    pressedfn=function()
        qviewOnFile("red")
    end,
}

qview_bind_v2{
    -- mods={},
    key="n",
    pressedfn=function()
        qviewOnFile("navy")
    end,
}

qview_bind_v2{
    -- mods={},
    key="m",
    pressedfn=function()
        qviewOnFile("lightsalmon")
    end,
}

qview_bind_v2{
    -- mods={},
    key="x",
    pressedfn=function()
        qviewOnFile("gray")
    end,
}

qview_bind_v2{
    -- mods={"shift"},
    key="d",
    pressedfn=function()
        qviewOnFile("qview-trs")
    end,
}

qview_bind_v2{
    -- mods={"shift"},
    key="u",
    pressedfn=function()
        brishz_eval_hs('awaysh-fast hs-reval-alert qview-restore-last')
    end,
}

qview_bind_v3{
    -- mods={},
    key={"SPC", "c", "c"},
    pressedfn=function()
        qviewOnFile("dup")
    end,
}
