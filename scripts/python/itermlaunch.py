#!/usr/bin/env python3
# Doesn't work, use itermlaunch.dash

import iterm2
import AppKit

AppKit.NSWorkspace.sharedWorkspace().launchApplication_("iTerm2")

async def main(connection):
    app = await iterm2.async_get_app(connection)
    await app.async_activate()
    # This will run 'vi' from bash. If you use a different shell, you'll need
    # to change it here. Running it through the shell sets up your $PATH so you
    # don't need to specify a full path to the command.
    await iterm2.Window.async_create(connection, command="ec hi")

iterm2.run_until_complete(main, True)

