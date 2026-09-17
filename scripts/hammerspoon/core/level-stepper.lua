--- * Coalescing level stepper
--- A display level -- brightness, contrast -- stepped from a held key, without
--- letting the keypresses outrun the bus that carries them.
---
--- One DDC operation costs 200-380ms on this panel and a locked `chg' is a read
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

    --- The last reading off the panel, one entry per selected display, written
    --- only ever from a reply. nil before the first one.
    local reading = nil
    --- When that came off the panel, for the trust window.
    local readingAt = 0
    --- The delta of the call currently out, and the delta accumulated since it
    --- left. The reading plus both of those is where the panel is heading,
    --- which is what the band shows: a reply only ever confirms the delta *it*
    --- carried, so displaying the reading alone made the band jump back up
    --- mid-hold and then down again as the next flush landed.
    local sentDelta = 0
    local pending = 0
    --- True while a garden call is out. The whole serialisation is this flag.
    local inFlight = false

    --- Nothing sent, nothing waiting: the panel is where the band says it is.
    local function settled()
        return sentDelta == 0 and pending == 0
    end

    local function outstanding()
        return sentDelta + pending
    end

    --- The reading, but only while it is still worth trusting. Every consumer
    --- has to ask the same question: a reading too old for the band to show is
    --- too old to suppress a write on the strength of. The clamp below used to
    --- test only that a reading existed, which at the top of the range made a
    --- stale 1.0 authoritative enough to swallow the keypress and not
    --- authoritative enough to display -- so the band sat at the ellipsis and
    --- never recovered.
    local function trusted()
        if not reading or #reading == 0 then return nil end
        if (hs.timer.secondsSinceEpoch() - readingAt) > knob("trust_seconds") then
            return nil
        end
        return reading
    end

    --- Where each display is heading, or nil when there is no reading worth
    --- trusting -- in which case the target is unknowable, however many presses
    --- are outstanding.
    local function targets()
        local levels = trusted()
        if not levels then return nil end

        local delta = outstanding()
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
    local function cue()
        if settled() then return "" end
        return outstanding() < 0 and " \u{2193}" or " \u{2191}"
    end

    local function bandShow()
        local levels = targets()
        local arrow = cue()

        local text
        if not levels then
            --- Either nothing has come back yet, or what did is old enough that
            --- another writer may have moved the panel since. The cue still goes
            --- on, so a press is visibly doing something even when the level is
            --- not ours to report.
            text = opts.title .. "\n" .. "\u{2026}" .. arrow
        else
            local rows = {}
            for _, level in ipairs(levels) do
                rows[#rows + 1] = string.format("%s  %d%%%s",
                    bar(level), math.floor(level * 100 + 0.5), arrow)
            end
            text = opts.title .. "\n" .. table.concat(rows, "\n")
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

    local function parse(out)
        if not out or out == "" then return nil end

        local levels = {}
        for line in tostring(out):gmatch("[^\r\n]+") do
            local n = tonumber((line:gsub("%s", "")))
            if n then levels[#levels + 1] = math.max(0, math.min(1, n)) end
        end

        if #levels == 0 then return nil end
        return levels
    end

    local flush

    --- `readIfIdle' asks for a bare reading when there is no delta to send and
    --- nothing the band would trust. Only a keypress passes it; the tail call
    --- below must not, or a read that keeps failing would spin.
    flush = function(readIfIdle)
        if inFlight then return end
        if pending == 0 then
            --- Nothing to write. Bail out if the band already has a level it
            --- would show, or if this was not a keypress; otherwise fall
            --- through to ask.
            if targets() or not readIfIdle then return end
        end

        sentDelta = pending
        pending = 0
        inFlight = true

        --- One command, one round trip: step, then report where that landed.
        --- The shell's own lock makes the pair atomic against the blackout loop
        --- and brightness-auto. With no step to make -- at either end of the
        --- range, where the clamp takes the whole delta -- it is the read
        --- alone, because the band still has to be told where the panel is.
        local cmd = sentDelta ~= 0
            and string.format("%s-inc %.4f ; %s-get", opts.family, sentDelta, opts.family)
            or (opts.family .. "-get")
        brishz_eval_out_hs(cmd, function(out)
            inFlight = false
            --- Whatever it carried is either in the reading below or lost with
            --- the call; either way it is no longer outstanding.
            sentDelta = 0

            local levels = parse(out)
            if levels then
                reading = levels
                readingAt = hs.timer.secondsSinceEpoch()
            else
                --- The call failed, so we cannot say whether its write landed.
                --- The old reading is no longer something to vouch for.
                reading = nil
            end

            bandShow()
            --- Whatever was pressed while that was out.
            flush()
        end, opts.label)
    end

    local function step(dir)
        local delta = (dir == "dec") and -knob("step") or knob("step")

        pending = pending + delta

        --- Hold the key past either end and the accumulator would otherwise run
        --- off to -0.5 while the panel sat at 0, so the first press back up
        --- would need forty more before anything moved. Clamped against the
        --- first display: with one panel that is exact, and with several it is
        --- the best a single scalar accumulator can do.
        local levels = trusted()
        if levels and levels[1] then
            local base = levels[1] + sentDelta
            pending = math.max(-base, math.min(1 - base, pending))
        end

        bandShow()
        flush(true)
    end

    return { step = step }
end
--- @end
