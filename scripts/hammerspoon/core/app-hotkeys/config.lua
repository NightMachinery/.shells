--- * App hotkeys: settings
--- The knobs of main.lua, with their defaults. Loaded before it. Each is read
--- at every use, so setting one from the console
--- (hs -c 'hide_return_policy = "summoner"') takes effect at once, until the
--- next reload.

--- Where a hide returns to, for the app hotkeys' second press and kitty's
--- toggle (screenReturnTarget). An enum:
---   "screen"    the app used last on the screen being left. With Emacs and
---               Telegram on the laptop and Brave on the monitor, hyper+x,
---               hyper+/, hyper+l, hyper+l lands in Emacs.
---   "summoner"  the app you were in just before the one being hidden, on
---               whichever screen: the app that brought you here. The same
---               keys land in Brave.
hide_return_policy = "screen"

--- How an app hotkey brings its app forward.
---   false  straight to the window server. unhide() first, since the second
---          press of a hotkey hides its app; it is a no-op on a visible app.
---          Then _bringtofront(false), the call hs.application:activate()
---          itself ends with (SetFrontProcessWithOptions, front window only).
---   true   hs.application:activate() as it is, which before that asks the
---          target app over Accessibility for its focused window and makes
---          it main. That is 3 to 8 ms of round trips to an app that answers
---          promptly, and unbounded when it does not. Switch this on if an
---          app with several windows (on several spaces, say) ever comes
---          forward with the wrong one.
app_hotkey_activate_via_ax_p = false

--- Whether an activation this config causes moves focus off a floating
--- window, such as a Picture-in-Picture video, to the app's front normal
--- window ("Not landing on a floating window" in main.lua). false keeps whatever window the
--- app focuses.
app_focus_skip_floating_p = true

--- Apps whose floating window is the point, which that check leaves alone:
--- the kitty panel floats on purpose (core/kitty-panel.lua), and a hide can
--- return to it.
appFloatingIntended = { ["net.kovidgoyal.kitty"] = true }
