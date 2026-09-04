# Tailscale Setup

`setup/setup_tailscale.zsh` installs and enables the Tailscale CLI daemon
without committing any account, tailnet, hostname, or auth-key material to this
public repo.
It runs with xtrace enabled and `PS4='> '`, so setup commands are visible while
it works.

Run it from this repo:

```zsh
setup/setup_tailscale.zsh
```

Use install-only mode when you want the package and daemon/app installed, but
want to authenticate later:

```zsh
setup/setup_tailscale.zsh --install-only
```

On macOS, the script targets the CLI/headless Homebrew formula, not the GUI
app. If it finds the `tailscale-app` cask or `/Applications/Tailscale.app`, it
reports what it found and asks for explicit confirmation before uninstalling or
removing anything. Then it installs `brew install tailscale`, starts it with
Homebrew services, and uses the formula's own `tailscale` binary for login and
status checks so a stale GUI shim does not get mistaken for the CLI install. If
the GUI remains installed after the confirmation step, the script stops rather
than installing two macOS Tailscale variants side by side.

On Linux, the script downloads Tailscale's official installer at runtime, starts
and enables `tailscaled` with the system service manager when available, then
runs `tailscale up` to print or open the normal login flow. Privileged Linux
commands use `sudo -kA`, following this repo's root-command rule.

The script deliberately does not use `--authkey`, `--login-server`,
`--hostname`, route advertisements, exit-node settings, or Tailscale SSH flags.
Those can reveal private topology, policy, or identity choices and should live
in an untracked local wrapper if needed.

The CLI/headless macOS variant is more scriptable and can run before GUI login,
but it is an administrator-oriented path. The GUI app is friendlier and is
Tailscale's normal recommendation for most macOS users; this repo chooses CLI
mode because setup should be reproducible from shell.

References:

- Tailscale macOS install docs: <https://tailscale.com/docs/install/mac>
- Tailscale macOS variant guidance: <https://tailscale.com/docs/concepts/macos-variants>
- Tailscale Linux install docs: <https://tailscale.com/docs/install/linux>
- Tailscale CLI reference: <https://tailscale.com/docs/reference/tailscale-cli>
