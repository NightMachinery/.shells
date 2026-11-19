# Cache the scripts PATH

Zsh adds a directory from the scripts tree only when it holds an executable
regular file or a symlink to one. It keeps the old recursive glob order and
excludes the scripts tree's `.app` bundles. Zsh also removes nonexistent
directories from the assembled PATH. Bash retains its existing startup
behavior.

Run `path-rebuild` after adding a command to a new directory or changing file
permissions. It rebuilds the directory list and shows the old and new
counts. Start a new shell for the refreshed PATH. New commands in a
directory already on PATH are available immediately.

The cache is under `${XDG_CACHE_HOME:-$HOME/.cache}/night-path/`, with a file
per hostname. Zsh builds it synchronously if missing, unreadable or invalid.
A day-old cache or a newer `night.sh` index, HEAD or HEAD reflog starts a
background refresh. The current shell uses the existing list, and the next
shell picks up the update. A lock serializes writers and the completed file
replaces the old one atomically. Keep the cache on local storage for shared
home directories.

New machines get the hooks from bootstrap stage 30 (`setup/bootstrap/stages/30-zsh.sh`).
On an existing machine, run `path-rebuild-install-hooks` once per `vcsh night.sh` checkout to
refresh after a checkout, merge, commit or rewrite. If a hook already exists
or the checkout uses `core.hooksPath`, add the following line to the relevant
hooks using the checkout's scripts path.

```sh
"${HOME}/scripts/setup/path-refresh-hook.zsh"
```

The helper detaches the refresh before returning. The installer preserves
existing hooks. Run the cache fixture with
`zsh -f zshlang/tests/path-cache.zsh`.
