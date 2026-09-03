--- * FIM: fill-in-the-middle completion at the cursor
--- The Hammerspoon twin of the zsh widget in
--- `zshlang/interactive/auto-load/FIM.zsh': hyper+shift+right asks a code model
--- what belongs between the text before the cursor and the text after it, and
--- offers the one line it answers with. Same request, same providers, same
--- status vocabulary -- see `./docs/fim.md'.
---
--- The whole point of doing this in Hammerspoon rather than per-app is that
--- there is no per-app hook worth writing: a Telegram draft, a browser textarea
--- and a native Cocoa field all want the same completion and none of them share
--- an extension point. What they do not share is a way to *read* the text, so
--- there are two capture paths:
---
---   ax    - hs.axuielement on the focused element, reading AXValue and
---           AXSelectedTextRange. Exact, invisible to the user, and the only
---           one that can tell later whether the cursor moved. Native Cocoa
---           fields and some Qt apps (sioyek) support it.
---   keys  - shift+cmd+up / cmd+c / shift+cmd+down / cmd+c / left, through the
---           clipboard. Ugly, but it is the only thing that works in Purple
---           Telegram, whose Qt draft box does not appear in the Accessibility
---           tree at all, and in kitty, which exposes an empty text area.
---
--- The run is a small state machine, and every step of it is a timer or a
--- callback: nothing here may block, because Hammerspoon's Lua thread is also
--- the event thread and a blocked one freezes every keystroke on the machine.
---
---   requesting  the task is running. Escape cancels it; any other key
---               *detaches* -- the key goes through to the app you are typing
---               in, and the completion lands on the clipboard instead.
---   ghost       the completion is on screen and nothing has been inserted.
---               Any key inserts it (and is itself delivered afterwards),
---               Escape discards it, a cmd/ctrl chord or a change of app
---               copies it to the clipboard and gets out of the way.
---   draining    the paste has been sent and has not been seen to land yet.
---               The key that accepted the ghost, and anything typed behind
---               it, is held here and posted in order once it has.
---
--- The clipboard is the fallback destination for every case where we could not
--- paste. A completion is never silently lost.
---
--- The keyDown eventtap exists only between the request and the end of the run.
--- A permanently installed tap on this machine would see every keystroke of
--- every app, which is both a privacy cost and a latency one; this way it is
--- installed for the second or two a completion is actually pending.

--- ** Configuration

--- hyper+shift+right -> the default provider. This used to be a mouse-movement
--- binding; see the note in core/mouse.lua.
fimHotkeyDefault = fimHotkeyDefault or { mods = {"shift"}, key = "right" }

--- hyper+ctrl+right -> deepseek: slower (~1.4s) and better, for when the
--- output matters more than the wait.
fimHotkeyDeepseek = fimHotkeyDeepseek or { mods = {"ctrl"}, key = "right" }

fimDefaultProvider = fimDefaultProvider or "codestral"

--- Context caps. The prefix keeps its LAST N characters and the suffix its
--- FIRST N: the model only ever needs the text nearest the cursor, and this is
--- also the privacy limit -- it bounds how much of whatever you happen to have
--- focused can leave the machine. Cut on code-point boundaries, never
--- mid-character, or the provider gets invalid UTF-8.
fimPrefixMaxChars = fimPrefixMaxChars or 4000
fimSuffixMaxChars = fimSuffixMaxChars or 1000

--- How long a ghost waits for a decision. When it runs out the completion goes
--- to the clipboard rather than being thrown away.
fimGhostSeconds = fimGhostSeconds or 45

--- Lifetime of the final status band.
fimStatusSeconds = fimStatusSeconds or 2.5

--- How long to wait for the pasteboard to change after a synthetic cmd+c
--- before concluding that the selection was empty. This is the one number in
--- the keystroke path that is a guess about someone else's app.
fimCopyTimeoutSeconds = fimCopyTimeoutSeconds or 0.35

--- How long to hold the key you accepted with while waiting for the paste to
--- appear. Past this the key goes out anyway: a cursor one character off is a
--- far smaller harm than a keyboard that stopped responding.
fimPasteConfirmSeconds = fimPasteConfirmSeconds or 0.4

--- The blind version of the same wait, for every path that cannot watch the
--- text: the keystroke capture, and any focused element that does not answer
--- AXNumberOfCharacters (Emacs and sioyek hand back an AXWindow, which does
--- not).
fimPasteSettleSeconds = fimPasteSettleSeconds or 0.06

--- A cached-element AX read costs 0.02-0.15ms when the app is healthy, so the
--- first read slower than this is not noise, it is an app that has begun to
--- struggle. Stop watching and take the blind wait: these reads are
--- synchronous on the thread that carries every keystroke on the machine, and
--- polling a hanging app would freeze the keyboard outright.
fimAxSlowReadSeconds = fimAxSlowReadSeconds or 0.005

--- Per-element AX timeout for the confirmation reads. Without one a wedged app
--- blocks for the system default, which is multiple seconds. Never set this to
--- 0.0: that is not "no timeout", it is "reset to the system default".
fimAxTimeoutSeconds = fimAxTimeoutSeconds or 0.05

fimAlertId = fimAlertId or "fim"

--- Force the keystroke path even where AX would work. For debugging the half
--- of the code that only Telegram and kitty normally exercise:
---   hs -c 'fimForceKeystrokePath = true'
fimForceKeystrokePath = fimForceKeystrokePath or false

--- The zsh widget's symbols, so the two halves of this feature read the same:
--- `❄' says the line is ours, then the state.
fimSymLead = fimSymLead or "❄"
fimSymWait = fimSymWait or "⋯"
fimSymOk = fimSymOk or "✓"
fimSymNone = fimSymNone or "∅"
fimSymErr = fimSymErr or "✗"
fimSymCopied = fimSymCopied or "📋"

local kBrishzq = "/usr/local/bin/brishzq.zsh"
local kPollSeconds = 0.02
local kPasteRestoreSeconds = 0.3
local kRequestBandSeconds = 30
local kMaxErrorChars = 200
local kPasteConfirmPollSeconds = 0.015

--- Our own injected events carry this in the event source's user-data field.
--- CGEventTapPostEvent puts a replacement event back into the stream *after*
--- the tap that produced it, so the drain should never see our cmd+v -- but a
--- doubled paste is an expensive way to discover otherwise.
local kInjectedMarker = 0x1F117
local kUserDataProperty = hs.eventtap.event.properties.eventSourceUserData

--- ** State
--- One table, global, so that a reload can find the previous run's tap and
--- timers and stop them. A dangling eventtap survives `hs.reload()' -- it is
--- held by the objc runtime, not by the Lua chunk -- and an orphaned one would
--- silently eat the next keystroke the user typed.
local previousState = fimState

fimState = fimState or {
    runId = 0,
    state = "idle",
}

--- ** Small helpers

local function fimBand(text, color, seconds)
    return alert_gateway(text, {
        id = fimAlertId,
        color = color or "default",
        seconds = seconds or fimStatusSeconds,
        -- The band belongs next to what you are typing, not on the other
        -- monitor, and a fullscreen wash for a 0.3s completion would be absurd.
        screens = "active",
        flashSeconds = 0,
        -- Plain, emphatically: completions are code, and `*' and `_' in code
        -- must not be eaten as markup.
        markup = "plain",
    })
end

local function fimHead(sym)
    return fimSymLead .. " FIM " .. sym
end

local function elapsedString(startedAt)
    if not startedAt then return "" end
    return string.format(" %.1fs", hs.timer.secondsSinceEpoch() - startedAt)
end

--- AXSelectedTextRange.location counts UTF-16 code units, not bytes and not
--- code points, so slicing a Lua byte string at it needs a walk. Anything
--- outside the BMP -- an emoji, most notably -- costs two units for one code
--- point. Returns a byte count, so that s:sub(1, n) is the prefix.
---
--- Invalid UTF-8 falls back to treating the offset as bytes: wrong, but a wrong
--- completion beats an error dialog, and every field this reads is valid UTF-8
--- in practice.
local function utf16OffsetToByte(s, units)
    if units <= 0 then return 0 end

    local ok, result = pcall(function()
        local seen = 0
        for pos, cp in utf8.codes(s) do
            if seen >= units then
                return pos - 1
            end
            seen = seen + ((cp > 0xFFFF) and 2 or 1)
        end
        return #s
    end)

    if ok then return result end
    return math.min(units, #s)
end

--- The same unit, counted rather than located: AXNumberOfCharacters is an
--- NSString length, so it is UTF-16 code units too, and an emoji moves it by
--- two. This is how much the count must grow by for a paste to have landed.
local function utf16Length(s)
    local ok, result = pcall(function()
        local count = 0
        for _, cp in utf8.codes(s) do
            count = count + ((cp > 0xFFFF) and 2 or 1)
        end
        return count
    end)
    if ok then return result end
    return #s
end

--- Exposed for testing from `hs -c'; nothing else calls it by this name.
fimUtf16Length = utf16Length

--- Trim any dangling UTF-8 continuation bytes from the front of a byte slice.
--- Only reachable through the invalid-input fallbacks below.
local function trimLeadingContinuation(s)
    while #s > 0 do
        local b = s:byte(1)
        if b >= 0x80 and b < 0xC0 then
            s = s:sub(2)
        else
            break
        end
    end
    return s
end

local function trimTrailingPartial(s)
    -- Walk back over continuation bytes to the lead byte and drop the whole
    -- sequence if it is incomplete.
    local i = #s
    local trailing = 0
    while i > 0 do
        local b = s:byte(i)
        if b >= 0x80 and b < 0xC0 then
            trailing = trailing + 1
            i = i - 1
        else
            local need = 0
            if b >= 0xF0 then need = 3
            elseif b >= 0xE0 then need = 2
            elseif b >= 0xC0 then need = 1 end
            if need > trailing then
                return s:sub(1, i - 1)
            end
            return s
        end
    end
    return s
end

--- The last `n' characters, cut on a code-point boundary.
local function lastChars(s, n)
    if n <= 0 then return "" end
    local len = utf8.len(s)
    if len and len <= n then return s end
    if len then
        return s:sub(utf8.offset(s, -n))
    end
    return trimLeadingContinuation(s:sub(-n))
end

--- The first `n' characters, cut on a code-point boundary.
local function firstChars(s, n)
    if n <= 0 then return "" end
    local len = utf8.len(s)
    if len and len <= n then return s end
    if len then
        return s:sub(1, utf8.offset(s, n + 1) - 1)
    end
    return trimTrailingPartial(s:sub(1, n))
end

local function oneLine(s, maxChars)
    s = tostring(s or "")
    s = s:gsub("%s+", " "):gsub("^%s+", ""):gsub("%s+$", "")
    if #s > maxChars then
        s = firstChars(s, maxChars) .. "…"
    end
    return s
end

local function frontmostPid()
    local app = hs.application.frontmostApplication()
    return app and app:pid() or nil
end

local function setPasteboard(text)
    if text == nil then
        hs.pasteboard.clearContents()
    else
        hs.pasteboard.setContents(text)
    end
end

--- ** Teardown
--- Everything that can outlive a run goes through here. `fimCancel' is the
--- public face of it and is the first thing a new hotkey press calls, so that a
--- second press supersedes rather than races.

local function stopTap(st)
    if st.tap then
        st.tap:stop()
        st.tap = nil
    end
end

local function stopTimers(st)
    for _, key in ipairs({ "pollTimer", "ghostTimer", "confirmTimer", "drainTimer" }) do
        if st[key] then
            st[key]:stop()
            st[key] = nil
        end
    end
end

local function teardown(st, keepBand)
    stopTap(st)
    stopTimers(st)
    if st.task then
        -- terminate, not interrupt: we no longer care what it was going to say.
        pcall(function() st.task:terminate() end)
        st.task = nil
    end
    st.state = "idle"
    st.completion = nil
    st.detached = false
    st.drainQueue = nil
    if not keepBand then
        alert_gateway_dismiss(fimAlertId)
    end
end

function fimCancel()
    -- Bumping the run id is what makes an in-flight task callback a no-op: the
    -- callback closes over the id it was started with and compares.
    fimState.runId = (fimState.runId or 0) + 1
    teardown(fimState, false)
    return true
end

-- Reload hygiene: the previous chunk's tap and timers are still alive, held by
-- the objc runtime rather than by any Lua reference we are about to replace.
if previousState then
    pcall(function() teardown(previousState, false) end)
    if previousState ~= fimState then
        pcall(function() teardown(fimState, false) end)
    end
end

--- ** Capture
--- Two paths, both ending in `callback(prefix, suffix, pathName)' or
--- `callback(nil)'.

local function axGet(element, attribute)
    if not element then return nil end
    local ok, value = pcall(function() return element:attributeValue(attribute) end)
    if ok then return value end
    return nil
end

local function axFocusedElement()
    local app = hs.application.frontmostApplication()
    if not app then return nil end
    local ok, axApp = pcall(hs.axuielement.applicationElement, app)
    if not ok or not axApp then return nil end
    return axGet(axApp, "AXFocusedUIElement")
end

--- The location half of AXSelectedTextRange, or nil. Read again at accept time
--- to find out whether the cursor moved while the ghost was up.
local function axCursorLocation(element)
    local range = axGet(element, "AXSelectedTextRange")
    if type(range) == "table" and type(range.location) == "number" then
        return range.location, (type(range.length) == "number" and range.length or 0)
    end
    return nil
end

local function captureViaAx(st)
    local element = axFocusedElement()
    if not element then return nil end

    local value = axGet(element, "AXValue")
    if type(value) ~= "string" then return nil end

    local location, length = axCursorLocation(element)
    if not location then return nil end

    local cut = utf16OffsetToByte(value, location)
    local afterCut = utf16OffsetToByte(value, location + length)

    st.axElement = element
    st.axLocation = location

    return value:sub(1, cut), value:sub(afterCut + 1)
end

--- Wait for the pasteboard's change count to move. A synthetic cmd+c into an
--- empty selection changes nothing at all, so a timeout *is* the answer
--- "nothing was selected" rather than a failure -- there is no event to wait
--- for and no way to ask the app.
local function waitForPasteboard(st, baseCount, callback)
    local deadline = hs.timer.secondsSinceEpoch() + fimCopyTimeoutSeconds
    local timer
    timer = hs.timer.doEvery(kPollSeconds, function()
        if hs.pasteboard.changeCount() ~= baseCount then
            timer:stop()
            if st.pollTimer == timer then st.pollTimer = nil end
            callback(hs.pasteboard.getContents())
        elseif hs.timer.secondsSinceEpoch() >= deadline then
            timer:stop()
            if st.pollTimer == timer then st.pollTimer = nil end
            callback(nil)
        end
    end)
    st.pollTimer = timer
end

--- shift+cmd+up selects cursor..start and a copy gives us the prefix; a right
--- arrow collapses that back to the cursor; shift+cmd+down then selects
--- cursor..end for the suffix; a left arrow collapses back again. Each collapse
--- is skipped when its copy timed out, since an arrow key on an empty selection
--- is an ordinary cursor move and would walk the cursor one character away.
---
--- Both collapses are load-bearing, and finding that out cost a test run: the
--- selection anchor does NOT survive the two extensions. See the comment on the
--- right arrow below for what goes wrong without it.
---
--- Every keystroke passes delay 0: hs.eventtap.keyStroke defaults to 200ms, and
--- five of those is a whole second of the user watching their document flash.
local function captureViaKeystrokes(st, runId, callback)
    local saved = hs.pasteboard.getContents()

    local function finish(prefix, suffix)
        -- Give the clipboard back before anything else happens; the request
        -- that follows may well want to put a completion on it.
        setPasteboard(saved)
        callback(prefix, suffix)
    end

    local function alive()
        return st.runId == runId
    end

    local base1 = hs.pasteboard.changeCount()
    hs.eventtap.keyStroke({"shift", "cmd"}, "up", 0)
    hs.eventtap.keyStroke({"cmd"}, "c", 0)

    waitForPasteboard(st, base1, function(prefix)
        if not alive() then return end

        -- Collapse back to the cursor before extending the other way.
        --
        -- The anchor does NOT stay put across the two extensions, which is the
        -- obvious thing to assume and is wrong. Measured in TextEdit:
        -- shift+cmd+up leaves the selection's origin at the *top*, so a
        -- following shift+cmd+down grows [0, end) and hands back the whole
        -- document as the "suffix", with the cursor finally collapsing to 0.
        -- Right arrow collapses a selection to its right edge, which is
        -- exactly the cursor we started from.
        --
        -- Only when something was actually selected: on an empty selection,
        -- right arrow is an ordinary cursor move and would walk us one
        -- character forward. A nil here is precisely that -- the copy timed
        -- out because there was nothing to copy.
        if prefix ~= nil then
            hs.eventtap.keyStroke({}, "right", 0)
        end

        local base2 = hs.pasteboard.changeCount()
        hs.eventtap.keyStroke({"shift", "cmd"}, "down", 0)
        hs.eventtap.keyStroke({"cmd"}, "c", 0)

        waitForPasteboard(st, base2, function(suffix)
            if not alive() then
                setPasteboard(saved)
                return
            end
            -- Mirrored: collapse to the left edge, back where we found the
            -- cursor. Skipped on an empty selection for the same reason.
            if suffix ~= nil then
                hs.eventtap.keyStroke({}, "left", 0)
            end
            finish(prefix or "", suffix or "")
        end)
    end)
end

--- ** The eventtap

local function keyCodeOf(event)
    return event:getKeyCode()
end

local function isEscape(event)
    return keyCodeOf(event) == hs.keycodes.map.escape
end

local function copyCompletionToClipboard(st, message, color)
    local completion = st.completion
    teardown(st, true)
    if completion then
        setPasteboard(completion)
    end
    fimBand(message, color or "notice")
end

local function markInjected(event)
    event:setProperty(kUserDataProperty, kInjectedMarker)
    return event
end

local function isInjected(event)
    return event:getProperty(kUserDataProperty) == kInjectedMarker
end

--- Everything the confirmation loop needs, resolved *before* cmd+v goes out.
---
--- Resolving AXFocusedUIElement is the expensive call -- around 1ms typically,
--- with a 53ms tail measured against System Settings -- while a read on an
--- element already in hand is 0.02-0.15ms. Doing the resolution once here is
--- what makes a 15ms poll cost about 1% of its own tick instead of dominating
--- it, and it is also the only correct place for the baseline: after the paste
--- the count has already moved.
---
--- AXNumberOfCharacters, not AXValue: the latter marshals the entire buffer
--- across the process boundary on every read, which for a terminal is a
--- screenful of text per poll.
local function pasteConfirmPlan(st, completion, selectionLength)
    if st.path ~= "ax" or not st.axElement then return nil end

    local element = st.axElement
    pcall(function() element:setTimeout(fimAxTimeoutSeconds) end)

    -- Once, here, and not on every tick of the loop below. isValid is itself an
    -- AX round trip -- it asks the app for the element's attribute names -- so
    -- per-poll it would roughly double the cost of a loop that is deliberately
    -- cheap, and it would learn nothing: an element that dies mid-paste answers
    -- nil to the count read, and a nil read already aborts.
    local valid, alive = pcall(function() return element:isValid() end)
    if not valid or not alive then return nil end

    local base = axGet(element, "AXNumberOfCharacters")
    if type(base) ~= "number" then return nil end

    -- A paste over a selection replaces it, so the net growth is the
    -- completion minus whatever was highlighted.
    local target = base + utf16Length(completion) - (selectionLength or 0)
    if target <= base then return nil end

    return {
        element = element,
        target = target,
        pollsLeft = math.max(1, math.ceil(fimPasteConfirmSeconds / kPasteConfirmPollSeconds)),
    }
end

--- Wait for the paste, then call `done'. With a plan, that means watching the
--- character count; without one, a fixed delay.
---
--- Every exit here is a *bounded* one. The reads are synchronous on
--- Hammerspoon's single Lua thread, which is also its event thread, so a loop
--- that kept trying against an unresponsive app would freeze every keystroke
--- on the machine -- strictly worse than the wrong cursor position this is
--- trying to avoid.
local function confirmPaste(st, plan, done)
    if not plan then
        st.confirmTimer = hs.timer.doAfter(fimPasteSettleSeconds, function()
            st.confirmTimer = nil
            done()
        end)
        return
    end

    local element = plan.element
    local deadline = hs.timer.secondsSinceEpoch() + fimPasteConfirmSeconds
    local timer
    timer = hs.timer.doEvery(kPasteConfirmPollSeconds, function()
        plan.pollsLeft = plan.pollsLeft - 1

        local startedAt = hs.timer.secondsSinceEpoch()
        local ok, count = pcall(function()
            return element:attributeValue("AXNumberOfCharacters")
        end)
        local spent = hs.timer.secondsSinceEpoch() - startedAt

        local stop
        if not ok or type(count) ~= "number" then
            -- A nil read is "the element is gone or the app will not answer",
            -- never "unchanged". Reading it as unchanged would hold the key
            -- back for the whole window for nothing.
            stop = true
        elseif count >= plan.target then
            stop = true
        elseif spent > fimAxSlowReadSeconds then
            stop = true
        elseif plan.pollsLeft <= 0 or startedAt >= deadline then
            stop = true
        end

        if stop then
            timer:stop()
            if st.confirmTimer == timer then st.confirmTimer = nil end
            done()
        end
    end)
    st.confirmTimer = timer
end

--- Post everything the drain held, in the order it was typed, and end the run.
--- The tap goes down first: these are real CGEventPosts and would otherwise
--- come straight back to us.
local function flushDrain(st, runId)
    if st.runId ~= runId or st.state ~= "draining" then return end

    local queue = st.drainQueue or {}
    st.drainQueue = nil
    teardown(st, true)

    for _, queued in ipairs(queue) do
        queued:post()
    end
end

--- Accept: put the completion in through the pasteboard, then let the key the
--- user actually pressed through behind it.
---
--- The first version of this returned all three events from the tap at once --
--- cmd+v down, cmd+v up, and a copy of your key. That is only correct in an app
--- that handles a paste synchronously. Qt, Electron and WebKit do not, so in
--- Telegram the key was consumed against the *pre-paste* buffer: an arrow
--- navigated the old text rather than the text just inserted.
---
--- So the key is swallowed here and posted once the paste has been seen to
--- land, and the tap stays up in `draining' meanwhile so that anything typed
--- during that window queues behind it instead of overtaking it.
---
--- One physical press still yields one keyDown: auto-repeat across an accept is
--- lost, and emulating it would mean inventing keystrokes the user did not make.
local function acceptGhost(st, event)
    local completion = st.completion

    -- The AX path is the only one that can check this, and it is worth
    -- checking: pasting a completion computed for a cursor that has since
    -- moved corrupts the line rather than completing it.
    local selectionLength = 0
    if st.path == "ax" and st.axElement then
        local location, length = axCursorLocation(st.axElement)
        if location ~= st.axLocation then
            copyCompletionToClipboard(st,
                fimHead(fimSymCopied) .. " cursor moved, copied to clipboard", "warn")
            return false
        end
        selectionLength = length or 0
    end

    local plan = pasteConfirmPlan(st, completion, selectionLength)

    local saved = hs.pasteboard.getContents()
    setPasteboard(completion)

    local elapsed = elapsedString(st.startedAt)
    local runId = st.runId

    -- Not a teardown: the tap is the whole mechanism here and has to stay up.
    -- Everything else that could still fire does go.
    stopTimers(st)
    if st.task then
        pcall(function() st.task:terminate() end)
        st.task = nil
    end
    st.state = "draining"
    st.completion = nil
    st.drainQueue = { event:copy() }

    fimBand(fimHead(fimSymOk) .. " inserted " .. tostring(utf8.len(completion) or #completion)
                .. " chars" .. elapsed, "free")

    -- Only restore if the completion is still there: a copy the user made in
    -- the meantime must not be clobbered by our bookkeeping.
    hs.timer.doAfter(kPasteRestoreSeconds, function()
        if hs.pasteboard.getContents() == completion then
            setPasteboard(saved)
        end
    end)

    confirmPaste(st, plan, function() flushDrain(st, runId) end)

    -- A backstop on the drain itself, not on the confirmation: whatever goes
    -- wrong in there, the keyboard comes back.
    st.drainTimer = hs.timer.doAfter(fimPasteConfirmSeconds + fimPasteSettleSeconds, function()
        st.drainTimer = nil
        flushDrain(st, runId)
    end)

    return true, {
        markInjected(hs.eventtap.event.newKeyEvent({"cmd"}, "v", true)),
        markInjected(hs.eventtap.event.newKeyEvent({"cmd"}, "v", false)),
    }
end

local function handleKeyDown(st, event)
    if st.state == "draining" then
        if isInjected(event) then return false end
        -- Held, not passed through: the point of the whole exercise is that
        -- these land after the key that accepted the ghost, in the order they
        -- were typed.
        local queue = st.drainQueue
        if queue then queue[#queue + 1] = event:copy() end
        return true
    end

    if st.state == "requesting" then
        if isEscape(event) then
            st.runId = (st.runId or 0) + 1
            teardown(st, true)
            fimBand(fimHead(fimSymErr) .. " cancelled", "warn")
            return true
        end

        -- You kept typing, so the completion no longer fits where it was going.
        -- Let the key through untouched and redirect the answer to the
        -- clipboard; cancelling instead would throw away a request you already
        -- paid for.
        st.detached = true
        stopTap(st)
        return false
    end

    if st.state == "ghost" then
        if isEscape(event) then
            teardown(st, true)
            fimBand(fimHead(fimSymNone) .. " discarded", "warn")
            return true
        end

        local flags = event:getFlags()
        if flags.cmd or flags.ctrl or frontmostPid() ~= st.pid then
            copyCompletionToClipboard(st, fimHead(fimSymCopied) .. " copied to clipboard")
            return false
        end

        return acceptGhost(st, event)
    end

    return false
end

local function startTap(st)
    stopTap(st)
    st.tap = hs.eventtap.new({ hs.eventtap.event.types.keyDown }, function(event)
        return handleKeyDown(st, event)
    end)
    st.tap:start()
end

--- ** The run

local function showGhost(st, completion)
    st.state = "ghost"
    st.completion = completion

    fimBand(fimHead(fimSymOk) .. elapsedString(st.startedAt)
                .. "  ·  any key inserts, Esc discards\n" .. completion,
            "notice", fimGhostSeconds)

    st.ghostTimer = hs.timer.doAfter(fimGhostSeconds, function()
        st.ghostTimer = nil
        copyCompletionToClipboard(st, fimHead(fimSymCopied) .. " copied to clipboard (timed out)")
    end)
end

local function request(st, runId, provider, prefix, suffix)
    st.startedAt = hs.timer.secondsSinceEpoch()
    -- Kept for debugging only: `hs -c' can then show what was actually
    -- captured, which is the only way to tell a bad completion apart from a
    -- bad capture -- and the keystroke path is easy to get subtly wrong.
    st.prefix = prefix
    st.suffix = suffix

    local task = taskWithPath(kBrishzq, function(exitCode, stdOut, stdErr)
        -- A superseded run's callback must do nothing at all: it would
        -- otherwise overwrite the band and the state of the run that replaced
        -- it.
        if st.runId ~= runId then return end

        local elapsed = elapsedString(st.startedAt)
        st.task = nil

        if exitCode ~= 0 then
            teardown(st, true)
            local message = oneLine(stdErr, kMaxErrorChars)
            if message == "" then
                message = "exit " .. tostring(exitCode)
            end
            fimBand(fimHead(fimSymErr) .. " " .. message, "crit")
            return
        end

        local completion = stdOut or ""
        if completion == "" then
            teardown(st, true)
            fimBand(fimHead(fimSymNone) .. " empty completion" .. elapsed, "warn")
            return
        end

        if st.detached or frontmostPid() ~= st.pid then
            st.completion = completion
            copyCompletionToClipboard(st,
                fimHead(fimSymCopied) .. " copied to clipboard (you kept typing)" .. elapsed)
            return
        end

        showGhost(st, completion)
    end, { "@opts", "provider", provider, "@", "fim-get", prefix, suffix })

    if not task then
        teardown(st, true)
        fimBand(fimHead(fimSymErr) .. " could not start brishzq.zsh", "crit")
        return
    end

    st.task = task
    st.state = "requesting"

    fimBand(fimHead(fimSymWait) .. " " .. provider .. " (" .. st.path .. ")",
            "default", kRequestBandSeconds)

    task:start()

    -- Only now, with every synthetic keystroke of the capture already sent, so
    -- the tap cannot see our own cmd+c and call it "the user kept typing".
    startTap(st)
end

--- The hotkey action. `provider' is a name fim-get knows: codestral, deepseek,
--- deepseek-flash.
function fimComplete(provider)
    provider = provider or fimDefaultProvider

    -- Secure Input means a password field somewhere has told the system that
    -- nobody may observe the keyboard. Reading the focused field or sending it
    -- keystrokes would be exactly the thing that flag exists to prevent, so
    -- this refuses before touching anything.
    if hs.eventtap.isSecureInputEnabled() then
        fimBand(fimHead(fimSymErr) .. " Secure Input is on", "crit")
        return false
    end

    fimCancel()
    hyper_exit()

    local st = fimState
    local runId = st.runId
    st.pid = frontmostPid()
    st.axElement = nil
    st.axLocation = nil
    st.detached = false

    if not st.pid then
        fimBand(fimHead(fimSymNone) .. " nothing to complete", "warn")
        return false
    end

    local function proceed(prefix, suffix, path)
        if st.runId ~= runId then return end

        if (prefix == "" or prefix == nil) and (suffix == "" or suffix == nil) then
            fimBand(fimHead(fimSymNone) .. " nothing to complete", "warn")
            return
        end

        st.path = path
        request(st, runId, provider,
                lastChars(prefix or "", fimPrefixMaxChars),
                firstChars(suffix or "", fimSuffixMaxChars))
    end

    if not fimForceKeystrokePath then
        local prefix, suffix = captureViaAx(st)
        if prefix ~= nil then
            proceed(prefix, suffix, "ax")
            return true
        end
    end

    st.state = "capturing"
    captureViaKeystrokes(st, runId, function(prefix, suffix)
        proceed(prefix, suffix, "keys")
    end)
    return true
end

--- ** Bindings

hyper_bind_v2{
    mods = fimHotkeyDefault.mods,
    key = fimHotkeyDefault.key,
    pressedfn = function() fimComplete(fimDefaultProvider) end,
}

hyper_bind_v2{
    mods = fimHotkeyDeepseek.mods,
    key = fimHotkeyDeepseek.key,
    pressedfn = function() fimComplete("deepseek") end,
}
--- @end
