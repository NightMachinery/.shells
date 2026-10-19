# The macOS #! kernel leak

On macOS 14 to 26, every exec of a `#!` script leaks a 1K buffer in the kernel
zone `data.kalloc.1024`. That memory is wired: it is never compressed or
swapped, and only a reboot frees it. A machine that runs scripts all day loses
gigabytes over a few weeks, and memory pressure rises with nothing in the
process list to show for it. Reports say the kernel panics when the zone
reaches about 20G.

**macOS 27 fixes it.** People who measured it report that the zone stays flat
and frees memory again
([claude-code#66020](https://github.com/anthropics/claude-code/issues/66020);
background in
[Photon's write-up](https://photon.codes/blog/we-found-a-kernel-memory-leak-in-macos-that-any-shell-script-can-trigger)).
Upgrading is the fix. Everything below is a stopgap until then.

## Do not spread the workaround

- Write new scripts and call sites normally. Do not add `shebang-bypass`,
  new `shebang-bypass-def` names, or explicit interpreters for this leak.
- A path earns a workaround only when a trace shows it runs scripts many
  times a minute.
- On macOS 27, remove the workarounds listed at the end.

## Checking for it

    sudo zprint data.kalloc.1024    # elements in use: about one per #! exec since boot

Measured on macOS 14 (2026-10): 3000 runs of a one-line `#!` script added 3003
elements; 3000 runs of `sh -c true` added noise. Starting the interpreter
yourself (`sh x.sh`) does not leak, because only the kernel's handling of the
`#!` line does.

To see which scripts run, and who runs them, `sudo eslogger exec` reports a
`script` field for each `#!` exec. It needs Full Disk Access for the terminal
it runs in. `fs_usage -f exec` is too heavy for long traces: a full core, and
about 19M of log a minute.

## Workarounds in place

Remove these on macOS 27.

- `shebang-bypass` and `shebang-bypass-def` in
  `zshlang/basic/auto-load/runners.zsh`, and the names it wraps there. `kitty`
  among them is Homebrew's `#!` wrapper around the app's binary, which every
  interactive shell runs once for its completions.
- `~/.shared.sh` keeps the output of `brew shellenv` in
  `~/.cache/brew-shellenv.<shell>` instead of running `brew` in every shell.
  This also saves about 20ms per shell, so keeping it does no harm.
- The Internet Sharing bridge LaunchDaemon (see
  `zshlang/auto-load/others/macOS/net.zsh`) runs its script as `/bin/sh
  <script>`. Also harmless to keep.
