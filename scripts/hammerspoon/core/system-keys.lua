-- * [[https://www.hammerspoon.org/docs/hs.eventtap.event.html#newSystemKeyEvent][Hammerspoon docs: hs.eventtap.event]]
-- ** DONE [[https://github.com/Hammerspoon/hammerspoon/issues/3350][`hs.eventtap.event.newSystemKeyEvent`: keeps pressing the keys indefintely · Issue #3350 · Hammerspoon/hammerspoon]]

function systemKey(key, repeatDelay)
    -- REPEAT_FASTER = 10 * 1000
    REPEAT_FASTER = 5

    hs.eventtap.event.newSystemKeyEvent(key, true):post()

    if not repeatDelay or repeatDelay <= 0 then
        repeatDelay = REPEAT_FASTER
    end
    hs.timer.usleep(repeatDelay)
    hs.eventtap.event.newSystemKeyEvent(key, false):post()
end

function mediaPlayPauseKey()
    systemKey("PLAY")
end

function mediaPreviousKey()
    systemKey("PREVIOUS")
end

function mediaNextKey()
    systemKey("NEXT")
end

function volumeIncKey()
    systemKey("SOUND_UP")
end

function volumeDecKey()
    systemKey("SOUND_DOWN")
end

function volumeMuteKey()
    systemKey("MUTE")
end

function brightnessIncKey()
    systemKey("BRIGHTNESS_UP")
end

function brightnessDecKey()
    systemKey("BRIGHTNESS_DOWN")
end
---
if false then
    hyper_bind_v1("p", function()
                      appName = application.frontmostApplication():name()
                      -- brishzeval("true " .. hs.pasteboard.getContents())
                      if appName == "Screen Sharing" then
                          -- Doesn't work, test with `sleep 2 ; hs -c 'hs.eventtap.keyStrokes("hi jungle")'`
                          -- hs.eventtap.keyStrokes(hs.pasteboard.getContents())
                      else
                          -- This was slow even with input-lang-get-darwin-fast, so the fault is probably with hammerspoon itself?

                          -- Warning:hs.keycode: key 'c' not found in active keymap; using ANSI-standard US keyboard layout as fallback, returning '8'
                          -- eventtap.keyStroke({"cmd"}, 'a')
                          -- doCopy()
                          eventtap.keyStroke({"cmd"}, 0)
                          eventtap.keyStroke({"cmd"}, 8)
                          local res
                          -- res = brishzeval2(("lang-toggle %q"):format(hs.pasteboard.getContents())) -- got into deadlocks since we hammerspoon in getting and setting the input lang now
                          if hs.keycodes.currentSourceID() == inputEnglish then
                              res = brishzeval2(("ecn %q | en2per"):format(hs.pasteboard.getContents()))
                              langSetPer()
                          else
                              res = brishzeval2(("ecn %q | per2en"):format(hs.pasteboard.getContents()))
                              langSetEn()
                          end
                          hs.eventtap.keyStrokes(tostring(res))
                      end
    end)
end
---
