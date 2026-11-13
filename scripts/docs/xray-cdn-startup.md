# CDN proxy startup

The dedicated CDN Xray backend starts from `launchers/various.zsh`, beside the
other Xray server. Its configuration stays in the private notes repository;
the launcher contains only its path.

To start it manually, run the `eva-cdn-server` line from the launcher in a
configured Zsh shell. Inspect its output with
`tmux capture-pane -p -t eva-cdn-server`.

If the CDN path returns HTTP 502, check that the backend is running and
listening on the upstream port configured in Caddy. The tmux launcher
survives logout but does not automatically restart a crashed backend.
