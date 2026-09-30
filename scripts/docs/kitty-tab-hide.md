# Hiding kitty tabs

`kitty-tab-hide` takes a tab out of kitty's tab bar, and `kitty-tab-show` puts
it back. Whatever runs in the tab keeps running. Code: the "Hiding tabs"
section of `zshlang/auto-load/others/terminal emulators/kitty.zsh`.

```
kitty-tab-ls                         #: every tab: its match, shown/hidden, title
kitty-tab-hide title:htop            #: any kitty tab match expression
kitty-tab-show window_id:154         #: back into the main window, as the active tab
kitty-tab-toggle window_id:154       #: whichever of the two applies
kitty-tab-state window_id:154        #: sets REPLY to shown or hidden
kitty-tab-hide-fz [query]            #: pick shown tabs to hide
kitty-tab-show-fz [query]            #: pick one hidden tab to show
kitty-tab-toggle-fz [query]          #: pick one tab of either kind
```

The argument is a kitty tab match expression (`kitty @ detach-tab --help`
lists the fields), so hiding several tabs at once is `title:foo or title:bar`.
[agfi:tmux2kitty-hide] and friends are wrappers that take a moved job's name
instead.

## How it works

kitty can hide an OS window, but not a single tab. So hiding a tab is two
remote-control calls: `detach-tab` moves the tab into a new OS window of its
own, and `resize-os-window --action hide` hides that window. Measured: well
under a second, and neither focus nor the main window's active tab moves.

Showing moves the tab back with `detach-tab --target-tab`, into the OS window
of the main window's active tab. kitty makes an arriving tab the active one,
and closes the emptied hidden window by itself. Showing the hidden window in
place would be simpler, but a shown window lands on whichever space macOS
picks, which is what the hyper+z panel exists to avoid. Nothing here focuses
kitty either, because focusing a hidden panel activates kitty on the wrong
space. If the panel is down, the tab is there at the next hyper+z.

- **Naming a tab:** by its first window's id, `window_id:<id>`, which is what
  `kitty-tab-ls` prints. A tab id does not survive `detach-tab`, because kitty
  moves the windows into a new tab. A window id does.
- **Hidden or shown:** kitty reports no visibility. A hidden tab is one whose
  windows carry the user variable `kitty_tab_hidden`, set on hide and cleared
  on show, and that sits outside the main OS window. The main OS window is the
  panel (`wm_class` `kitty-panel`), or without one, the first OS window with an
  unmarked tab. The mark keeps a second ordinary kitty window from counting as
  hidden. The location check means a tab that something else put back counts
  as shown.
- **One hidden window per tab:** kitty gives a window that `detach-tab`
  creates no name to find it by later, so there is nothing to gather hidden
  tabs under.
- **The panel can bring them back:** it folds stray OS windows into itself
  whenever it is created (any hyper+z that finds no panel window, as after
  kitty's launch), and on every hyper+z when Hammerspoon's
  `kitty_panel_fold_strays` is on (`hammerspoon/core/kitty-panel.lua`). A
  folded tab is then an ordinary shown tab.
- Hidden tabs do not outlive kitty, any more than shown ones do.

Knob: `kitty_tab_fz_opts` (array), extra options for the pickers' fz.

## Untested

- Showing a tab when kitty has no main OS window at all, only hidden ones. It
  then shows the tab's own window instead.
- Hiding the only tab of the panel. The panel window may close, in which case
  the next hyper+z would create a new one and fold the tab straight back in.
