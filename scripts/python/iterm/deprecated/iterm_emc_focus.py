#!/usr/bin/env python3

# Usage: `pkill -SIGUSR1 -f iterm_emc_focus.py`
# Now integrated into our main iTerm Python server, but preserved here as an example, as the main server could not use signals (they broke brish), and so switched to unix sockets.

import iterm2
import signal
import asyncio

# from IPython import embed
# import nest_asyncio


async def tab_actiavte(connection, window, index=4):
    await window.tabs[index].async_activate()

async def main(connection):
    loop = asyncio.get_event_loop()
    app = await iterm2.async_get_app(connection)
    window = app.current_terminal_window

    # nest_asyncio.apply()
    # embed(using='asyncio')

    # 30    SIGUSR1      terminate process    User defined signal 1
    sig = signal.SIGUSR1
    loop.add_signal_handler(sig, lambda: asyncio.ensure_future(tab_actiavte(connection, window)))
    print(f"Listening for signal {sig} ...")

iterm2.run_forever(main)
