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

## Local firewalls silently drop tailnet traffic

Tailscale SSH is a poor test of local firewall rules. `tailscaled` terminates it
inside its own userspace netstack, so those packets never reach the host packet
filter at all. Something that genuinely crosses the filter, such as mosh's UDP,
can fail while `ssh` to the same node is fine. "SSH still works" proves nothing
here, and neither does `tailscale ping`.

On macOS two independent layers each have to allow it, and each fails by
dropping rather than by reporting an error:

- **The `com.user.publicnet` pf anchor** denies inbound traffic by source
  address and trusts only RFC1918. Tailscale uses `100.64.0.0/10`, which is RFC
  6598 carrier-grade NAT space and not RFC1918, so tailnet peers are dropped
  unless the anchor is installed with `--tailscale`. See
  `launchers/pf/install.org` for the safety argument and the caveats. Note it
  governs IPv4 only: the anchor's `fc00::/7` rule already contains Tailscale's
  ULA prefix, so tailnet IPv6 is trusted either way. A service can therefore
  work over `-6` and time out over v4 on the same host.

- **The macOS Application Firewall**, for any ad-hoc signed Homebrew binary that
  accepts inbound connections. With stealth mode on it drops without a word, and
  because such binaries are spawned over ssh there is no GUI session to show the
  usual "allow incoming connections?" prompt, so it defaults to deny.
  `mosh-server` is the usual casualty. Register it with
  [agfi:firewall-allow-mosh-darwin], and re-run that after every
  `brew upgrade mosh`: it registers the version-stamped Cellar path, so an
  upgrade quietly invalidates it. `firewall-allow-mosh-darwin --check` reports
  the current state and needs no root.

References:

- Tailscale macOS install docs: <https://tailscale.com/docs/install/mac>
- Tailscale macOS variant guidance: <https://tailscale.com/docs/concepts/macos-variants>
- Tailscale Linux install docs: <https://tailscale.com/docs/install/linux>
- Tailscale CLI reference: <https://tailscale.com/docs/reference/tailscale-cli>
