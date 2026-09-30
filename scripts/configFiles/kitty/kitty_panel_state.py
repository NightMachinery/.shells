"""kitty_panel_state: the hyper+z panel's state, as a tiny JSON object.

Run over remote control by hammerspoon/core/kitty-panel.lua:

    kitten @ kitten ~/scripts/configFiles/kitty/kitty_panel_state.py

It is a no_ui kitten, so handle_result runs inside kitty and its return
value is the command's output. That is the point of it: `kitten @ ls' also
reports every window's foreground processes, which can run to hundreds of
KB (one hidden tab here made a two-window `ls' 635 KB), and all of it has to
be produced by kitty and decoded in Hammerspoon on every key press. This
reads only ids from kitty's tab managers.

The fields, all optional:
  win       some window in the panel OS window (wm_class kitty-panel)
  active    the panel's active tab's active window, the one to focus
  firstTab  the panel's first tab, where stray tabs are moved to
  strays    the tabs of every other OS window, in order
"""

import json

from kittens.tui.handler import result_handler

PANEL_CLASS = "kitty-panel"


def main(args):
    pass


@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    state = {"strays": []}
    for tm in list(boss.os_window_map.values()):
        tabs = list(tm.tabs)
        if tm.wm_class == PANEL_CLASS and "win" not in state:
            for tab in tabs:
                windows = list(tab)
                if windows:
                    state["win"] = windows[0].id
                    break
            if tabs:
                state["firstTab"] = tabs[0].id
            active_tab = tm.active_tab
            active_window = active_tab.active_window if active_tab else None
            if active_window is not None:
                state["active"] = active_window.id
        elif tm.wm_class != PANEL_CLASS:
            state["strays"].extend(tab.id for tab in tabs)
    return json.dumps(state)
