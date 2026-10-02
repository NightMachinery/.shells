--- * Coalescing level stepper
--- A display level -- brightness, contrast -- stepped from a held key, without
--- letting the keypresses outrun the bus that carries them.
---
--- One DDC operation costs 200-380ms on this panel and a locked step is a read
--- plus a write, so ~600ms; key repeat is ~30ms. Firing one detached job per
--- press used to overlap them ten to one, and unserialised DDC does not lose
--- steps so much as invent them: ten concurrent decrements measured *one step
--- brighter* than they started. h-ddc-lock-do in system.zsh fixed the
--- corruption, but a lock alone turns a one-second key hold into twenty seconds
--- of queue. So the presses are coalesced here, at the source.
---
--- The rule is one garden call in flight at a time. Presses arriving during a
--- flight accumulate into `pending' and leave as a single larger delta, so the
--- total always matches what was pressed while the number of DDC round trips
--- stays proportional to time rather than to keystrokes. The call asks for the
--- new level in the same breath, which costs nothing extra -- the reply is what
--- the band displays, and it makes the optimistic level self-correcting after
--- every flush rather than drifting.
---
--- Which display a press steps is decided here, per press, and sent to zsh as
--- id:<n> selectors (see core/screens.lua). It used to be left to zsh's
--- default, `main', which is the menu-bar display: with the lid open that is
--- the laptop panel, so the brightness keys stepped the laptop while you
--- worked on the monitor, and the contrast keys did nothing at all, since the
--- laptop has no contrast. The target is the screen named by the instance's
--- `screens' knob, the active screen by default; a family that the target
--- cannot do (contrast on the laptop) falls back to `fallbackScreens'.
---
--- Each target has its own flight state. A reading from one panel must never
--- clamp, or be shown as, a step meant for another, and a reply that comes
--- back after focus moved belongs to the display it was sent to.
---
--- Written as a factory because brightness and contrast are the same problem on
--- the same bus under the same lock, and every measurement above was taken
--- against that bus rather than against either axis. Two copies would drift,
--- and the second copy would be the one without the comments.
---
--- Usage, from core/window-media-bindings.lua:
---   local b = levelStepperNew{
---       title = "Brightness", id = "hyper-brightness",
---       family = "brightness", knobPrefix = "hyper_brightness_",
---       label = "hyperBrightnessStep",
---   }
---   function hyperBrightnessStep(dir) b.step(dir) end
---
--- `title' heads the band, `id' is the alert id it updates in place, `family'
--- names the shell commands (<family>-inc, <family>-get), `knobPrefix' the
--- globals, and `label' is what a failed garden call prints as.
--- `internalOK' is false for a family the built-in panel cannot do.
---
--- `dir' is the direction, not a boolean: the shell side is <family>-dec and
--- <family>-inc, so that is what travels.

--- Defaults for the per-instance knobs. Seeded as globals at construction, so
--- they are discoverable from the console, and read back on every press, so a
--- console edit takes effect on the next keystroke rather than on the next
--- reload.
local kKnobDefaults = {
    step = 0.01,
    band_seconds = 1.5,
    bar_cells = 20,
    --- How long a level read from the panel is worth believing. The cache is
    --- only ever authoritative until something else writes, and three other
    --- things do: brightness-auto-loop on a 3s cycle, display-black-on-loop on
    --- a 5s one, and the monitor's own buttons, which we cannot see at all.
    --- Past this the band shows an ellipsis rather than a stale number -- the
    --- reply is ~600ms behind the press, and being briefly uninformative beats
    --- being briefly wrong. Matched to the fastest of those writers.
    trust_seconds = 3,
    --- A core/screens.lua spec: which display a press steps.
    screens = "active",
    --- Where a press goes when no target display can do this family.
    fallback_screens = "external",
}

function levelStepperNew(opts)
    local prefix = opts.knobPrefix

    for key, default in pairs(kKnobDefaults) do
        local name = prefix .. key
        if _G[name] == nil then _G[name] = default end
    end

    local function knob(key)
        local v = _G[prefix .. key]
        if v == nil then return kKnobDefaults[key] end
        return v
    end

    --- Flight state per target, keyed by the target's ids ("2", "2,3").
    ---   ids        CGDirectDisplayIDs, in band order
    ---   names      display names, same order
    ---   reading    the last reading per id, written only ever from a reply
    ---   readingAt  when that came off the panel, for the trust window
    ---   sentDelta  the delta of the call currently out
    ---   pending    the delta accumulated since it left
    ---   inFlight   true while a garden call is out
    --- The reading plus both deltas is where the panel is heading, which is
    --- what the band shows: a reply only ever confirms the delta *it* carried,
    --- so displaying the reading alone made the band jump back up mid-hold and
    --- then down again as the next flush landed.
    local states = {}

    --- The displays a press steps right now.
    local function resolveTargets()
        local recs = {}
        for _, screen in ipairs(Screens.target(knob("screens"))) do
            local r = Screens.record(screen)
            if r and (opts.internalOK ~= false or not r.internal) then
                recs[#recs + 1] = r
            end
        end
        if #recs == 0 then
            for _, screen in ipairs(Screens.target(knob("fallback_screens"))) do
                local r = Screens.record(screen)
                if r and (opts.internalOK ~= false or not r.internal) then
                    recs[#recs + 1] = r
                end
            end
        end
        return recs
    end

    local function stateFor(recs)
        local ids, names = {}, {}
        for _, r in ipairs(recs) do
            ids[#ids + 1] = tostring(r.cgid)
            names[#names + 1] = r.name
        end
        local key = table.concat(ids, ",")
        local st = states[key]
        if not st then
            st = { ids = ids, names = names, reading = nil, readingAt = 0,
                   sentDelta = 0, pending = 0, inFlight = false }
            states[key] = st
        end
        st.names = names
        return st
    end

    --- Nothing sent, nothing waiting: the panel is where the band says it is.
    local function settled(st)
        return st.sentDelta == 0 and st.pending == 0
    end

    local function outstanding(st)
        return st.sentDelta + st.pending
    end

    --- The reading, but only while it is still worth trusting, and only when
    --- it covers every display of the target. Every consumer has to ask the
    --- same question: a reading too old for the band to show is too old to
    --- suppress a write on the strength of. The clamp below used to test only
    --- that a reading existed, which at the top of the range made a stale 1.0
    --- authoritative enough to swallow the keypress and not authoritative
    --- enough to display -- so the band sat at the ellipsis and never
    --- recovered.
    local function trusted(st)
        if not st.reading then return nil end
        if (hs.timer.secondsSinceEpoch() - st.readingAt) > knob("trust_seconds") then
            return nil
        end
        local levels = {}
        for i, id in ipairs(st.ids) do
            local v = st.reading[id]
            if v == nil then return nil end
            levels[i] = v
        end
        if #levels == 0 then return nil end
        return levels
    end

    --- Where each display is heading, or nil when there is no reading worth
    --- trusting -- in which case the target is unknowable, however many presses
    --- are outstanding.
    local function targets(st)
        local levels = trusted(st)
        if not levels then return nil end

        local delta = outstanding(st)
        local out = {}
        for index, level in ipairs(levels) do
            out[index] = math.max(0, math.min(1, level + delta))
        end
        return out
    end

    local function bar(level)
        local cells = knob("bar_cells")
        local filled = math.max(0, math.min(cells, math.floor(level * cells + 0.5)))
        return string.rep("\u{25AE}", filled) .. string.rep("\u{25AF}", cells - filled)
    end

    --- An arrow while the panel has not caught up, and nothing once it has. It
    --- cannot be the ellipsis, which already means "no reading to trust", and it
    --- cannot be a region of the bar either: at 20 cells one cell is 5%, so the
    --- one to three steps typically outstanding would not move a single cell.
    local function cue(st)
        if settled(st) then return "" end
        return outstanding(st) < 0 and " \u{2193}" or " \u{2191}"
    end

    local function bandShow(st)
        local levels = targets(st)
        local arrow = cue(st)

        --- The display's name heads the band, so with two screens it is never
        --- a guess which one moved; with several rows each one is named.
        local title = opts.title
        if #st.names == 1 then title = title .. " \u{00B7} " .. st.names[1] end

        local text
        if not levels then
            --- Either nothing has come back yet, or what did is old enough that
            --- another writer may have moved the panel since. The cue still goes
            --- on, so a press is visibly doing something even when the level is
            --- not ours to report.
            text = title .. "\n" .. "\u{2026}" .. arrow
        else
            local rows = {}
            for i, level in ipairs(levels) do
                local name = (#levels > 1) and ("  " .. (st.names[i] or "")) or ""
                rows[#rows + 1] = string.format("%s  %d%%%s%s",
                    bar(level), math.floor(level * 100 + 0.5), arrow, name)
            end
            text = title .. "\n" .. table.concat(rows, "\n")
        end

        --- flashSeconds 0 deliberately: a fullscreen wash on every step of a key
        --- hold would be unusable. Same id throughout, so the band updates in
        --- place and its deadline is pushed out rather than a second one
        --- stacking up.
        alert_gateway(text, {
            id = opts.id,
            seconds = knob("band_seconds"),
            flashSeconds = 0,
            screens = "all",
            -- The whole point of this band is to be read while hyper is held:
            -- the level keys leave the mode entered on purpose, so the peek
            -- would otherwise fade the one band the keypress exists to show.
            peek = false,
        })
    end

    --- Replies come back as "<cgid> <level>" lines, one per display, so a
    --- display that failed to answer cannot shift the others' rows.
    local function parse(out)
        if not out or out == "" then return nil end

        local levels, any = {}, false
        for line in tostring(out):gmatch("[^\r\n]+") do
            local id, v = line:match("^%s*(%d+)%s+(%S+)%s*$")
            local n = tonumber(v)
            if id and n then
                levels[id] = math.max(0, math.min(1, n))
                any = true
            end
        end

        if not any then return nil end
        return levels
    end

    --- One command, one round trip: step every display, then report where each
    --- landed. The shell's own lock makes each step atomic against the
    --- blackout loop and brightness-auto. With no step to make -- at either
    --- end of the range, where the clamp takes the whole delta -- it is the
    --- read alone, because the band still has to be told where the panel is.
    local function command(st)
        local parts = {}
        if st.sentDelta ~= 0 then
            for _, id in ipairs(st.ids) do
                parts[#parts + 1] = string.format("%s-inc %.4f id:%s", opts.family, st.sentDelta, id)
            end
        end
        for _, id in ipairs(st.ids) do
            parts[#parts + 1] = string.format("ec \"%s $(%s-get id:%s 2>/dev/null)\"", id, opts.family, id)
        end
        return table.concat(parts, " ; ")
    end

    local flush

    --- `readIfIdle' asks for a bare reading when there is no delta to send and
    --- nothing the band would trust. Only a keypress passes it; the tail call
    --- below must not, or a read that keeps failing would spin.
    flush = function(st, readIfIdle)
        if st.inFlight then return end
        if st.pending == 0 then
            --- Nothing to write. Bail out if the band already has a level it
            --- would show, or if this was not a keypress; otherwise fall
            --- through to ask.
            if targets(st) or not readIfIdle then return end
        end

        st.sentDelta = st.pending
        st.pending = 0
        st.inFlight = true

        brishz_eval_out_hs(command(st), function(out)
            st.inFlight = false
            --- Whatever it carried is either in the reading below or lost with
            --- the call; either way it is no longer outstanding.
            st.sentDelta = 0

            local levels = parse(out)
            if levels then
                st.reading = levels
                st.readingAt = hs.timer.secondsSinceEpoch()
            else
                --- The call failed, so we cannot say whether its write landed.
                --- The old reading is no longer something to vouch for.
                st.reading = nil
            end

            bandShow(st)
            --- Whatever was pressed while that was out.
            flush(st)
        end, opts.label)
    end

    local function step(dir)
        local recs = resolveTargets()
        if #recs == 0 then
            alert_gateway(opts.title .. "\nno display here can do this", {
                id = opts.id, seconds = knob("band_seconds"), flashSeconds = 0,
                screens = "all", peek = false,
            })
            return
        end
        local st = stateFor(recs)

        local delta = (dir == "dec") and -knob("step") or knob("step")

        st.pending = st.pending + delta

        --- Hold the key past either end and the accumulator would otherwise run
        --- off to -0.5 while the panel sat at 0, so the first press back up
        --- would need forty more before anything moved. Clamped against the
        --- first display: with one panel that is exact, and with several it is
        --- the best a single scalar accumulator can do.
        local levels = trusted(st)
        if levels and levels[1] then
            local base = levels[1] + st.sentDelta
            st.pending = math.max(-base, math.min(1 - base, st.pending))
        end

        bandShow(st)
        flush(st, true)
    end

    return { step = step }
end
--- @end
