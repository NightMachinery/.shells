# tmux, kitty and macOS permissions

A tmux server, and every process it will ever run in a pane, gets its macOS
privacy permissions (TCC: Automation, Accessibility, protected folders, and so
on) from whichever client happened to start the server. If that was a mosh or
ssh session, every pane answers to the remote-shell daemon. The fix is to have
kitty start the server. This page explains the mechanism, the tools, and how to
repair a server that already went wrong.

Code: `zshlang/auto-load/others/tmux-kitty.zsh`, plus
[agfi:darwin-responsible-get] in `zshlang/auto-load/others/macOS/macOS.zsh` and
the two hooks in [agfi:tmuxnew].

## The mechanism: the responsible process

TCC does not judge the process that sends an Apple Event or asks for
Accessibility. It judges that process's **responsible process**. That is an
ancestor fixed when the process is spawned, inherited through fork, exec and
daemonisation, and never reassigned. An app launched by Launch Services is its
own responsible process, and its children inherit it.

What follows for tmux, all of it measured:

- `tmux new -d` forks the server, which daemonises. Its parent becomes
  launchd, but it keeps the responsible process of the client that started
  it.
- A pane's processes are the server's children, so they inherit the
  *server's* responsible process. The client that created the session does
  not count, and neither does the one that attaches. A session created from a
  kitty window, inside a server started from mosh, still answers to the mosh
  side.
- mosh-server is launched over ssh and then daemonises, so everything in a
  mosh session answers to the ssh daemon. With Tailscale SSH that is
  `tailscaled`, identified by TCC through its versioned Homebrew Cellar path.
- kitty does not disclaim responsibility for its children. A shell in a kitty
  window, a `kitty @ launch` background process, and a tmux server started by
  either one all answer to kitty.

Because the server's parent is launchd, `ps` ancestry cannot show any of this.
Ask the kernel instead:

```
darwin-responsible-get <pid>     #: '<pid> <executable>'
darwin-responsible-get-fz [q]    #: the same for processes picked with ffps
tmux-server-doctor               #: says whom the running server answers to
```

`darwin-responsible-get` calls `responsibility_get_pid_responsible_for_pid`, an
undocumented libSystem function that needs no privileges. TCC's own decisions
are in the unified log:

```
log show --last 30m --style compact --predicate 'subsystem == "com.apple.TCC"'
```

## The incident this came from

- tmux segfaulted. The next `tmux new` came from a `tz` in a mosh session over
  Tailscale SSH, so the new server, and later BrishGarden in it, answered to
  `tailscaled`.
- osascript in the garden raised an Automation prompt in `tailscaled`'s name.
  Declining it made every `notif-os-dismiss-all` fail with `-1743`.
- **Misleading:** re-running the boot launchers from kitty changed nothing.
  They created their sessions inside the existing server, so they inherited its
  attribution.
- **Misleading:** allowing `tailscaled` under Automation only swapped the
  error for `-25211` ("osascript is not allowed assistive access"). The
  notification script also needs Accessibility, and TCC asks separately for
  each service. Granting the daemon permission one service at a time never
  converges, and each grant hands those rights to anything that arrives over
  Tailscale SSH.
- A server started from mosh also carries `SSH_CLIENT` and `SSH_CONNECTION` in
  its global environment, so every pane inherits them. Code that tests those
  variables thinks each pane is an ssh session.

Since 2026-09-30 hyper+d no longer goes through the garden: Hammerspoon runs
`notif-dismiss-v2.jxa` itself (`hammerspoon/core/app-hotkeys.lua`), so the
Automation and Accessibility grants that decide it are Hammerspoon's, whoever
started tmux. `notif-os-dismiss-all` from a shell still depends on them.

## Prevention: `tmux-server-ensure`

[agfi:tmuxnew] calls [agfi:tmux-server-ensure] before `tmux new`. `tmuxnew` is
where [agfi:tmuxnewsh2], [agfi:tmuxnewsh], [agfi:tmux-job-start],
[agfi:brishgarden-boot], `tz`, `tma` and the boot launchers all end up, so this
one hook covers all of them.

- **A server is running:** return at once. The test is one `connect(2)` to the
  socket that a bare `tmux` would use, through `zsh/net/socket`. It needs no
  fork and cannot start a server by accident. The socket file proves nothing,
  since it survives a crash.
- **No server, and this shell already answers to kitty** (the normal boot,
  kitty running `zsh -c ivy`): return, and let `tmuxnew` start it as before.
- **Otherwise:** ask kitty over its remote-control socket
  ([agfi:kitty-socket-get]) to run `tmux start-server` in the background,
  through `zsh -c` so the server's environment comes from `.zshenv`. Wait
  until the socket answers, then check the result with
  [agfi:tmux-server-kitty-p].
- **kitty unreachable, or the wait times out:** say why, and let `tmuxnew`
  start the server from where it is. A server with the wrong attribution beats
  having no tmux from the phone while the Mac's GUI is down.
  `tmux-server-doctor` reports it afterwards.

The kitty-started server gets `exit-empty off`, so that it stays up while it is
still empty. A side effect is that it no longer exits when its last session
closes; `tmux kill-server` still ends it.

`TMUX_TMPDIR` is forwarded. A socket that came from `$TMUX` is passed as `-S`.
Knob: `tmux_server_ensure_timeout` (seconds).

`ivy` runs [agfi:tmux-server-doctor] when it finds tmux already running and
the server is not kitty's, because that is the moment you are sure to be at
kitty and able to restart it.

Not covered:

- A bare `tmux` or `tmux new` typed in a mosh shell, and the `t.hv` alias.
  They bypass `tmuxnew`. A zsh `tmux` wrapper for interactive shells could
  catch them. A PATH shim would catch everything, but would cost an extra exec
  on each of the many tmux calls that tmux-z and agent-session make.
- `tmux-subagent-launch.py` calls `tmux new-session` itself. It runs inside
  tmux, where a server already exists.

## Moving one job out: `tmux2kitty`

```
tmux2kitty BrishGarden           #: any pane target: a session name, %28, ...
tmux2kitty-ls                    #: what was moved: name, shown/hidden, kitty window, pid
tmux2kitty-text BrishGarden      #: its whole scrollback, through kitty's socket
tmux2kitty-focus BrishGarden     #: switch kitty to it, showing it first if hidden
tmux2kitty-show BrishGarden      #: bring its hidden tab back, as the active tab
tmux2kitty-hide BrishGarden      #: take its tab out of the tab bar; it keeps running
tmux2kitty-toggle BrishGarden    #: whichever of the two applies
tmux2kitty-stop BrishGarden      #: stop it and close its window
tmux2kitty-restart BrishGarden   #: stop it and start the same command again in kitty
```

`tmux2kitty-restart` is for a job that must reread something it reads only at
start, such as an API key ([agfi:api-key-rotate] uses it). It keeps the job
shown or hidden. The command comes from a record that `tmux2kitty` writes next
to its marker (`<marker>.cmd`, owner-only: the directory and then the argv, each
NUL-terminated). A job moved before that record existed has none. It is then
left running, and you restart it once with its launcher and move it again.

Pickers take an optional fuzzy query instead of a name:

- `tmux2kitty-fz` lists only the panes `tmux2kitty` would accept, and never
  this shell's own pane. Each row shows the pane and what finally runs in it,
  and dead panes are marked. It accepts several picks, and targets them by
  pane id, which cannot go stale between the pick and the move.
- `tmux2kitty-text-fz`, `tmux2kitty-focus-fz` and `tmux2kitty-stop-fz` pick
  from `tmux2kitty-ls`. Focus takes a single pick.
- `tmux2kitty-hide-fz`, `tmux2kitty-show-fz` and `tmux2kitty-toggle-fz` are
  the generic `kitty-tab-*-fz` pickers restricted to moved jobs' tabs, through
  `kitty_tab_fz_match`. Hide lists only shown jobs, and show only hidden ones;
  show and toggle take a single pick.

[agfi:tmux2kitty] re-runs a pane's `pane_start_command` in its
`pane_start_path`, as a new kitty tab (`--keep-focus`, `--hold`) that is
hidden or shown according to `tmux2kitty_type`, under
`zsh -c` so that it gets the environment `.zshenv` builds. The job then answers
to kitty, and nothing else in the server is touched. This is the stopgap for a
bad server whose other sessions you cannot afford to lose yet. BrishGarden is
the usual case: Hammerspoon reaches it over HTTP, so it does not care where it
runs.

- Everything that can fail runs before the kill: decoding the command,
  finding kitty's socket, a `kitty @ ls`. A refusal leaves the job running
  where it was.
- It kills the pane's whole process tree, waits for it to be gone, and sends
  KILL to anything that ignores TERM. A job holding a port can only be
  restarted once all of it has exited. Zombies count as gone. kitty's
  `--hold` wrapper becomes a zombie that kitty does not reap, not even after
  its window closes, and `kill -0` succeeds on a zombie, which made an early
  version wait out its whole timeout.
- Its window carries the kitty user variable `tmux2kitty=<id>`, a sanitised
  copy of the name. Unlike a title, the program cannot overwrite it, and
  sanitising matters because kitty match expressions split on spaces and read
  the value as a regex. The inspection helpers find the window by that
  variable. `tmux2kitty-text` works from anywhere that can reach the socket,
  including a mosh session on the phone, where no kitty tab is visible.
- It leaves a marker under the state directory. When [agfi:tmuxnew] starts a
  session of the same name, for instance when `brishgarden-boot` is re-run, it
  stops the kitty copy first. Otherwise the two would fight over the port.
  Without a marker, the check is one `test -e`. If the kitty copy cannot be
  stopped, `tmuxnew` starts nothing and fails, and `tmux2kitty-stop` keeps
  the marker so the next attempt tries again. A missing job is plain to see
  and fix; two copies at once are not.
- It refuses a pane whose loss would be more than a restart, because moving it
  would kill whatever runs in it and leave only a fresh prompt.
  `tmux2kitty_force_p=y` overrides all of these, in `tmux2kitty-fz` as well:
  - **An interactive shell.** `tmuxnewsh` wraps even interactive sessions as
    `zsh -c "cd DIR && ... zsh"`, so it peels the `<shell> -c` layers and
    looks at what finally runs. A bare shell counts, and so does a command
    whose last word is one, like `mosh host -- zsh`. That rule also catches an
    ssh tunnel that ends in a remote shell.
  - **A REPL.** This means `ipython` or `python3 -i` given only flags, or a
    REPL as the last word, as in `env VAR=x julia`. `python3 -m http.server`
    and `julia script.jl` are jobs.
  - **A coding agent,** i.e. a session carrying `@agent_session`. The agent
    pane wrapper looks like any other job, so the tmux option is the only
    reliable sign.
- A dead pane (kept by `remain-on-exit`) is restarted in kitty without a kill.
  Its `pane_pid` is the pid its process had, and that number may since belong
  to an unrelated process.
- tmux prints the start command in its own quoting, and zsh's lexer takes it
  off, as in `agent-session.zsh`. A literal newline or tab in a start command
  comes back as the two characters `\n` or `\t`.

### Hiding a moved job's tab

`tmux2kitty-hide`, `tmux2kitty-show` and `tmux2kitty-toggle` pass the job's
kitty window to [agfi:kitty-tab-hide], [agfi:kitty-tab-show] and
[agfi:kitty-tab-toggle], which work on any kitty tab. See
[kitty-tab-hide.md](kitty-tab-hide.md) for how hiding works and what can bring
a hidden tab back. `tmux2kitty-ls` reports each job as shown or hidden by the
same rule.

Knobs: `tmux2kitty_type` (kitty's `--type`: `tab`, `os-window`,
`background`..., plus `hidden`, a tab that is hidden at once),
`tmux2kitty_timeout`, `tmux2kitty_force_p`, `tmux2kitty_state_dir`, and the
extra fz options `tmux2kitty_fz_fz_opts` and `tmux2kitty_moved_fz_opts`
(arrays).

## Repairing a server that went wrong

Responsibility cannot be changed after spawn, so the only real repair is a new
server started from kitty. It kills every session, including any agents
running in them. Do it from a plain kitty window, **outside tmux**:

1. Stop each session's process tree: [agfi:tmux-session-processes-kill] over
   `tmux ls -F '#{session_name}'`. Then run `tmux kill-server`.
2. Look for orphans that outlived their panes (servers such as caddy,
   sftpgo, xray, jupyter, brishgarden's python) and kill them by PID. Killing
   a parent does not kill its children.
3. Stop anything moved with `tmux2kitty` (`tmux2kitty-ls`, `tmux2kitty-stop`),
   or let the launchers reclaim it in the next step.
4. Run `ivy`, which re-runs `launchers/various-darwin.zsh` from kitty.
5. `tmux-server-doctor` should now say kitty.

If you granted the remote-shell daemon anything while working around the bad
server, revoke it under System Settings → Privacy & Security. With the server
fixed, only commands typed directly into a mosh shell answer to the daemon,
and denying them is the right default.

## Untested

- What TCC does when a server's responsible kitty has since exited, e.g.
  kitty restarted while tmux lived on. That is already true of every normal
  boot, so it is not a regression.
- Whether Hammerspoon's `hs` IPC works from a mosh or Tailscale SSH session.
  That is why kitty's unix socket is the spawner here: it works from anywhere.
