You need to use `vcsh night.sh` instead of `git` to interact with this repository. Always ignore untracked files, and use `status -uno`.

The root of this git repo is at `~/` as it contains some dotfiles. But we only work with the files inside `~/scripts`; so the root of this project is `~/scripts`. I.e., when I say `./x`, I mean `~/scripts/x`, unless the PWD is otherwise specified.

Read `./PE/Zsh.org`. When you think of something that needs to be in this file, suggest it to me, but don't edit the file yourself unless I tell you to.

Read all scripts in `zshlang/basic`. Reuse functions when possible, DRY.

## Plugins
### Loading

We load our plugins manually in `zshlang/load-others.zsh`.

### Documentaion

If you want to document installation for a plugin, read the readme of another plugin first and use its style.

