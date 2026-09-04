# Tailscale Setup

`setup/setup_tailscale.zsh` installs and enables Tailscale without committing
any account, tailnet, hostname, or auth-key material to this public repo.

Run it from this repo:

```zsh
setup/setup_tailscale.zsh
```

Use install-only mode when you want the package and daemon/app installed, but
want to authenticate later:

```zsh
setup/setup_tailscale.zsh --install-only
```

On macOS, the script installs Homebrew's `tailscale-app` cask, which is the
standalone app variant Tailscale recommends for normal macOS use, then opens
the app so macOS can approve the VPN configuration and the user can complete
login.

On Linux, the script downloads Tailscale's official installer at runtime, starts
and enables `tailscaled` with the system service manager when available, then
runs `tailscale up` to print or open the normal login flow. Privileged Linux
commands use `sudo -kA`, following this repo's root-command rule.

The script deliberately does not use `--authkey`, `--login-server`,
`--hostname`, route advertisements, exit-node settings, or Tailscale SSH flags.
Those can reveal private topology, policy, or identity choices and should live
in an untracked local wrapper if needed.

References:

- Tailscale macOS install docs: <https://tailscale.com/docs/install/mac>
- Tailscale macOS variant guidance: <https://tailscale.com/docs/concepts/macos-variants>
- Tailscale Linux install docs: <https://tailscale.com/docs/install/linux>
- Tailscale CLI reference: <https://tailscale.com/docs/reference/tailscale-cli>
