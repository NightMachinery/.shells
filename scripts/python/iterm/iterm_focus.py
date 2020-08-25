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
    # nest_asyncio.apply()

    app = await iterm2.async_get_app(connection)
    async with iterm2.FocusMonitor(connection) as monitor:
        while True:
            update = await monitor.async_get_next_update()
            # print(update.__dict__)
            window = app.current_terminal_window
            if (update.active_session_changed or update.selected_tab_changed or update.window_changed) and window.current_tab:
                # embed(using='asyncio')
                zp('reval-ec redis-cli set iterm_active_session {window.current_tab.active_session_id} 2>&1')


iterm2.run_forever(main)
