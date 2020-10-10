#!/usr/bin/env python3

import AppKit
bundle = "com.googlecode.iterm2"
if not AppKit.NSRunningApplication.runningApplicationsWithBundleIdentifier_(bundle):
    AppKit.NSWorkspace.sharedWorkspace().launchApplication_("iTerm")

import os
from brish import z, zp
# if os.environ.get("ITERM2_COOKIE", "") == "":
os.environ["ITERM2_COOKIE"] = z("""osascript -e 'tell application "iTerm2" to request cookie' """).outrs

import asyncio
import iterm2
from IPython import embed
import nest_asyncio

async def main(connection):
    # if z("isdbg"):
    #     nest_asyncio.apply()

    input_lang = 'U.S.' # @possibly use redis for this. This can make us override the saved setting with the touchbar toggle.
    app = await iterm2.async_get_app(connection)
    async with iterm2.FocusMonitor(connection) as monitor:
        while True:
            update = await monitor.async_get_next_update()
            window = app.current_terminal_window
            ##
            # zp("ecdbg {update.__dict__!s}")
            # if update.window_changed:
                # zp("ecdbg {update.window_changed.__dict__!s}")
                # zp("ecdbg {update.window_changed.event.__dict__!s}")
                # zp("ecdbg {window.__dict__!s}")
            ##
            if (update.active_session_changed or update.selected_tab_changed or update.window_changed) and window.current_tab:
                # embed(using='asyncio')
                if update.window_changed:
                    focus=update.window_changed.event.name
                    if focus == 'TERMINAL_WINDOW_BECAME_KEY':
                        input_lang = z('input-lang-get-darwin').outrs
                        z('input-lang-set en')
                    else:
                        z('input-lang-set {input_lang}')
                    zp('reval-ec redis-cli set iterm_focus {focus} 2>&1')
                zp('reval-ec redis-cli set iterm_active_session {window.current_tab.active_session_id} 2>&1')


iterm2.run_forever(main)
