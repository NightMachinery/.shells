# tmux and tty-title

[agfi:tty-title] sets the terminal title so a kitty tab announces what it is
running: `🍼scripts` for Claude Code, `⚡scripts` for Codex, `🪐scripts` for
Antigravity, `🌳` for the `ivy` session. Inside tmux the kitty window and tab
titles never changed.

The escape was never lost. The panes had the right titles all along; only the
hop out of tmux was missing. From `tmux list-panes -a`, with `T=` for
`#{pane_title}` and `W=` for `#{window_name}`:

```
scripts-claudework3 0.0 T=[✳ tty-title tmux compatibility] W=[claude.exe]
scripts-codex1      0.0 T=[✳ Claude Code]                  W=[claude.exe]
BrishGarden         0.0 T=[mb2.local]                       W=[zsh]
```

`mb2.local` is the hostname, the default title tmux assigns a pane at
creation; the BrishGarden shell never set one.

## Why

Three causes, the last of them cosmetic.

tmux's `set-titles` is off by default and `~/.tmux.conf` set no title option
at all. With it off tmux never emits the client's `tsl`/`fsl`, so nothing a
pane writes can reach kitty:

```
$ tmux show-options -g | grep -iE 'title|rename'
set-titles off
set-titles-string "#S:#I:#W - \"#T\" #{session_alerts}"
allow-rename off
allow-set-title on
automatic-rename on
```

`allow-set-title on` is why the inbound OSC 2 was accepted into `pane_title`;
`set-titles off` is why it stopped there. `xterm-kitty`'s terminfo already
carries `tsl=\E]2;` and `fsl=^G`, so tmux had everything it needed to pass it
on.

Second, [agfi:title] in `zshlang/basic/auto-load/terminal emulators.zsh`
dispatches on `case "$TERM"`. `~/.tmux.conf` sets `default-terminal
'tmux-direct'`, so inside a pane `$TERM=tmux-direct`, which matches neither
the `xterm*` arm nor `screen*`. It fell into the catch-all and survived only
because `infocmp tmux-direct` happens to carry `hs`, `tsl=\E]0;` and `fsl=^G`.
Two consequences. It rested on a terminfo coincidence. And on any host where
tmux advertises `screen-256color` instead, `title` takes the `screen*` arm and
emits `\ek…\e\\`, the *window rename* sequence, which `allow-rename off`
discards outright -- silent, not merely invisible.

Third, the pane title was nearly invisible even where it landed. The default
`status-right` renders `"#{=21:pane_title}"`: active pane only, truncated to
21 characters, bottom-right corner.

## The fix in `~/.tmux.conf`

```
set -g set-titles on
set -g set-titles-string '#{?#{==:#{pane_title},#{host}},#S,#{pane_title}}'
```

Not tmux's default string, `#S:#I:#W - "#T" #{session_alerts}`. Two existing
consumers read the title back with start-anchored matches, and any prefix
destroys them. [agfi:kitty-tab-codex-p] in
`zshlang/auto-load/others/terminal emulators/kitty.zsh` tests
`((.title // "") | test("^⚡"))` against `kitty @ ls`.
[agfi:h-claude-code-session-of-kitty-window] in
`zshlang/auto-load/others/claude-session.zsh` strips leading non-alphanumerics
and then compares for *equality* against a session name.

Not bare `#T` either. A pane that never set a title keeps tmux's default, the
hostname, so every plain-shell session's kitty window would read `mb2.local`.
The conditional substitutes the session name in exactly that case. Measured,
both branches:

```
$ tmux display-message -p '#{host}'
mb2.local
$ tmux display-message -p '#{?#{==:#{pane_title},#{host}},#S,#{pane_title}}'
✳ tty-title tmux compatibility
$ tmux display-message -t 'BrishGarden:' -p '#{?#{==:#{pane_title},#{host}},#S,#{pane_title}}'
BrishGarden
```

This also makes [agfi:tmux-attach]'s pre-attach `tty-title "$session"` come
out right *while attached*, not only for the instant before.

## The fix in `terminal emulators.zsh`

`zshlang/basic/auto-load/terminal emulators.zsh` gains an [agfi:isTmux] branch
in `title`, ahead of the `case`, that emits OSC 2 with `printf`; and `tmux*`
is added to the `xterm*` arm.

`$TMUX` rather than `$TERM` because it is authoritative and survives
`default-terminal` being changed back to a `screen*` value, which is exactly
the latent failure above.

`tmux*` is still added to the arm because `$TMUX` does not travel over ssh.
`zshlang/basic/ssh.zsh` smuggles only `COLORFGBG COLORTERM TERM_PROGRAM
KITTY_WINDOW_ID`, but `TERM` is forwarded, so a shell on a remote host reached
by ssh *from* a pane has `TERM=tmux-direct` and no `$TMUX`. OSC 2 is still the
right thing to write there: the outer tmux is what parses it.

The `screen*` arm is left alone. It is genuinely ambiguous between GNU screen
and a tmux configured with a `screen*` `default-terminal`, and `\ek` is the
only thing real screen understands.

OSC 1, the icon name, is not sent. tmux's OSC dispatch handles 0 and 2 and
drops 1, and neither consumer reads an icon name.

## The fix in `setup/minimal_proxy/.zshenv`

The file carries a byte-identical fork of `title`/`tty-title`, marked
`@duplicateCode/fd706e6b8475e27ca5cf27951b1d8ddc`, and is rsync'd to servers,
so it gets the same two edits. It has no `isTmux`, so that copy inlines
`test -n "$TMUX"`.

## The fix in `claude.zsh`

Claude Code rewrites the title continuously with `✳ <summary>`, overwriting
the `🍼${PWD:t}` marker within seconds. Its binary honours
`CLAUDE_CODE_DISABLE_TERMINAL_TITLE`, so `zshlang/auto-load/others/claude.zsh`
gains a `claude_tty_title_keep_p` flag, read with `bool`, that sets it.

Off by default, because strategy 3 of
[agfi:h-claude-code-session-of-kitty-window] maps a kitty window to a session
*by* that `✳ <name>` title. Strategies 1, 2 and 4 -- foreground PID, tmux
client, registry -- are unaffected, and under tmux it is strategy 2 that
fires. So the flag is cheap inside tmux and costly outside. Codex and
Antigravity do not overwrite the title and need no equivalent.

## A separate bug: the title text was executed

`title` used `print -Pn "$1:q"`, and `setopt prompt_subst` is in force. The
`:q` is a no-op: it backslash-escapes, and then `print` without `-r` strips
exactly those backslashes again before prompt expansion runs. A title
containing `$(...)` is therefore *executed*:

```
$ x='a %n 50%% $(echo PWNED) b'
$ print -Pn "$x:q"; echo
a evar 50% PWNED b
$ print -Pn "$x"; echo
a evar 50% PWNED b
$ printf '%s' "$x"; echo
a %n 50%% $(echo PWNED) b
```

Reachable from [agfi:mpv] (media filename), [agfi:iloop] (`$PWD`) and the
fuzzy pickers. Every arm now uses `printf` and no arm uses prompt expansion.

## Rejected: `tmux select-pane -T`

Recorded so nobody re-litigates it. It does *not* steal focus:
`cmd-select-pane.c` returns before any activation code when `-T` is given,
whatever the man page's lead sentence suggests. It is still the wrong tool
here.

- It forks a `tmux` process on every call; OSC 2 is one `write(2)`.
- It expands its argument as a tmux *format*, a second injection surface on
  top of the prompt one above.
- It needs `$TMUX_PANE` to be correct, which breaks under `tmux run-shell` and
  in garden shells.
- It cannot work at all for the ssh-from-a-pane case, which the escape
  handles for free.

## Caveats

- Reverting is not symmetric. tmux never pushes or restores the terminal
  title; there is no `22;0t`/`23;0t` in the binary. Turning `set-titles` back
  off stops updates, it does not restore what kitty showed before. Detaching
  likewise leaves the last tmux-set title in place.
- Applies live. Unlike `terminal-overrides`/`terminal-features`, these are
  session options read at redraw time, so `tmux source-file ~/.tmux.conf`
  reaches already-attached clients with no reattach.
- Refresh is event-driven, not polled. tmux recomputes the title on any full
  redraw -- pane switch, window switch, attach, pane title change -- and a
  `strcmp` guard suppresses redundant escapes.
- Per client. The format is evaluated from each attached client, so clients
  on different sessions do not interfere. Clients lacking `tsl`/`fsl` are
  skipped, so the Termux client from `./tmux-termux-truecolor.md` is harmless
  here; it advertises the `title` feature anyway.
- `remain-on-exit on`. A dead pane keeps its last title and `#{pane_title}`
  carries no dead marker, so a kitty window can advertise a live-looking title
  for a session whose shell has exited. Prefixing `#{?pane_dead,[dead] ,}`
  would fix that and break `kitty-tab-codex-p`'s `^⚡` anchor; left out
  deliberately.
- `ivy` is a special case, handled below. Any other session that wants a fixed
  label rather than its active pane's title can do the same thing.

## Pinning one session's label: `ivy`

The global string follows the active pane, and `ivy`'s panes are mpv and
friends, so the tab came to read whatever was playing instead of the `🌳` that
[agfi:ivy] sets with `tty-title` just before attaching.

`set-titles-string` is a *session* option, so [agfi:ivy-tmux-title-set] pins
`ivy`'s label without touching any other session:

```
tmux set-option -t 'ivy' set-titles-string '🌳'
```

It runs right after `tmux new-session` in [agfi:ivy-self], so it covers every
later attach and not only the one `ivy` performs itself.
[agfi:ivy-tty-title-get] holds the label, still overridable with
`ivy_tty_title`; a `#` in it is doubled, because `#` opens a format
substitution in a tmux option.

The pane titles are left alone, so `status-right` still shows what is playing:

```
$ tmux list-panes -t ivy -F '#{pane_index} #{pane_title}'
0 Christmas Classics (2000)
1 tmp
2 tmp
$ kitty @ ls | jq -r '.[].tabs[].windows[]|select(.id==1)|.title'
🌳
```

One trap. `set-option` rejects the `=` exact-match prefix that `has-session`
accepts, so the session-exists check and the write cannot share a target
string:

```
$ tmux has-session -t '=ivy' && echo ok
ok
$ tmux set-option -t '=ivy' set-titles-string TEST
no such session: =ivy
```

The bare name is safe after the exact check, because tmux resolves an exact
session name before trying it as a prefix.

## Testing

```
tmux source-file ~/.tmux.conf
tmux show-options -g | grep set-titles
brishz-restart
```

`brishz-restart` because BrishGarden holds persistent zsh shells and will not
see zshlang edits otherwise.

From a pane:

```
tty_title_f=y tty-title 'hello 50% $(echo not-run)'
kitty @ ls | jq -r '.[].tabs[].windows[].title'
```

The kitty title must read `hello 50% $(echo not-run)`, literally.

Confirm the two forks of `title` stayed in sync. Three differences are
expected and only three: the `isTmux` call inlined as `test -n "$TMUX"`, the
`[agfi:isTmux]` link dropped, and the two doc paths made absolute. Anything
else means the forks have drifted:

```
diff <(sed -n '/^function title {/,/^function tty-title {/p' \
        ~/scripts/zshlang/basic/auto-load/terminal\ emulators.zsh) \
     <(sed -n '/^function title {/,/^function tty-title {/p' \
        ~/scripts/setup/minimal_proxy/.zshenv)
```

References:

- `./tmux-termux-truecolor.md`, for how tmux talks to the attached client
- `~/.tmux.conf`
- [tmux set-titles](https://man.openbsd.org/tmux#set-titles),
  [set-titles-string](https://man.openbsd.org/tmux#set-titles-string),
  [allow-set-title](https://man.openbsd.org/tmux#allow-set-title),
  [allow-rename](https://man.openbsd.org/tmux#allow-rename),
  [automatic-rename](https://man.openbsd.org/tmux#automatic-rename)
