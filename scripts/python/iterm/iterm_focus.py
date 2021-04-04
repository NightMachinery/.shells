#!/usr/bin/env python3

import AppKit
import signal
import asyncio
import traceback
import os
import re
from brish import z, zp

from IPython import embed
import nest_asyncio
def embed2():
    nest_asyncio.apply()
    embed(using='asyncio')

bundle = "com.googlecode.iterm2"
if not AppKit.NSRunningApplication.runningApplicationsWithBundleIdentifier_(bundle):
    AppKit.NSWorkspace.sharedWorkspace().launchApplication_("iTerm")

# if os.environ.get("ITERM2_COOKIE", "") == "":
os.environ["ITERM2_COOKIE"] = z("""osascript -e 'tell application "iTerm2" to request cookie' """).outrs
import iterm2

tab_actiavte_pat = re.compile(r'tab_activate (?P<index>\d+)')
async def tab_actiavte(connection, window, index=4):
    await window.tabs[index].async_activate()

async def handle_client_factory(connection, ):
    app = await iterm2.async_get_app(connection)
    window = app.current_terminal_window
    async def handle_client(reader, writer):
        request = None
        request = (await reader.read(255)).decode('utf8')
        m = tab_actiavte_pat.match(request)
        response = ""
        try:
            response = f"Unknown request: {request}\n"
            if m:
                i = int(m.group('index'))
                response = f"Activating tab {i}\n"
                await tab_actiavte(connection, window, i)
        except:
            response+="An exception occurred:\n"
            response+=traceback.format_exc()

        writer.write(response.encode('utf8'))
        # await writer.drain()
        writer.close()

    return handle_client

async def main(connection):
    # if z("isdbg"):
    #     nest_asyncio.apply()

    server = await asyncio.start_unix_server(await handle_client_factory(connection), path=f"{HOME}/tmp/.iterm_socket")
    await asyncio.gather(serve(server), iterm_focus_monitor(connection))


async def serve(server):
    async with server:
        print("Unix socket online")
        await server.serve_forever()

async def iterm_focus_monitor(connection):
    app = await iterm2.async_get_app(connection)

    # input_lang = 'U.S.'
    async with iterm2.FocusMonitor(connection) as monitor:
        print("Focus monitoring online")
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
            if (update.active_session_changed or update.selected_tab_changed or update.window_changed) and window and window.current_tab:
                # embed(using='asyncio')
                if update.window_changed:
                    focus=update.window_changed.event.name
                    if focus == 'TERMINAL_WINDOW_BECAME_KEY':
                        # input_lang = z('input-lang-get-darwin').outrs
                        # z('input-lang-set en')
                        z('input-lang-push en')
                    else:
                        # z('input-lang-set {input_lang}')
                        z('input-lang-pop')
                    zp('reval-ec redis-cli set iterm_focus {focus} 2>&1')
                zp('reval-ec redis-cli set iterm_active_session {window.current_tab.active_session_id} 2>&1')

HOME = os.environ["HOME"]
iterm2.run_forever(main)
