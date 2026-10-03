# Xray restart loop in v2-on

[agfi:v2-on] uses [agfi:tmuxnewsh2] to start Xray through the existing
[agfi:loop] helper in its tmux session. After every Xray exit, including status
zero, the loop waits three seconds and starts it again. Output and the iteration
display stay in the pane.
The existing `retry` helper would return after a zero exit.

`v2_on_retry_delay` accepts whole seconds from 1 to 300. `v2_on_config` optionally
selects a config file; otherwise the existing configured default is used:

```zsh
v2_on_retry_delay=5 v2_on_config="${config_file}" v2-on
```

The wrapper checks the required executables, a readable regular config file and
a valid interval before replacing the current session. Failed checks leave that
session running. Config contents are not validated; a readable but invalid
config is retried until corrected or stopped. `tmuxnewsh2` loads `loop` in a
fresh `zsh -c`, forwards the leading `lo_s` assignment and quotes command
arguments, preserving spaces and quotes in file names.

[agfi:v2-off] replaces the same session with the existing direct gost proxy and
stops the old loop and process tree. To stop the whole job:

```zsh
tmux-job-stop v2ray-genrouter
```

`loop` keeps its existing signal behavior: Ctrl-C during the interval stops it,
while interruption of the running command alone can lead to another iteration.
Use `v2-off` or the process-tree stopper for intentional shutdown of the loop,
Xray and any interval sleep. This detects process exits, not hangs or upstream
connection failures. Emacs FIM keeps its configured proxy route.

An existing session keeps its old command. Reload the shell definitions and
run `v2-on` again to activate the loop. Run [agfi:brishz-restart] first when
invoking the updated command through Brish, which retains old definitions.
Restarting the proxy briefly interrupts its connections.
