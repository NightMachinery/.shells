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

## Android internet failures: separate DNS from connectivity

An SSH connection into Termux over Tailscale can keep working while Android's
hostname resolution fails. Test from the phone while the fault is present:

```sh
command curl --noproxy '*' --head --connect-timeout 5 --max-time 10 https://example.com
command curl --noproxy '*' --head --connect-timeout 5 --max-time 10 https://1.1.1.1
command dig +time=3 +tries=1 @1.1.1.1 example.com
command dig +time=3 +tries=1 @8.8.8.8 example.com
command dig +time=3 +tries=1 @100.100.100.100 example.com
command dig +tcp +time=3 +tries=1 @100.100.100.100 example.com
```

For a stronger comparison, use an address from the successful public DNS lookup
with `curl --resolve example.com:443:ADDRESS https://example.com`, keeping the
same timeout and proxy options. This bypasses DNS while preserving the hostname
and TLS certificate verification; do not use `--insecure`.

If normal curl reports a resolution timeout, the hostname-preserving bypass
works, public DNS responds, and Quad100 times out, DNS is demonstrably failing
while the tested internet path remains usable. This pattern was reproduced on
Android 14; force-closing and reopening Tailscale restored internet access,
as confirmed by the user. It does not establish whether the cause is Private DNS, app routing,
or a stuck Tailscale resolver. Test a known MagicDNS name too: it should resolve
locally, so its failure is evidence beyond a public upstream DNS failure.

Quad100 (`100.100.100.100`) is a service inside the local Tailscale client, not a
remote public resolver. See [Tailscale's Quad100 documentation](https://tailscale.com/docs/reference/quad100).
Termux's `$PREFIX/etc/resolv.conf` alone does not establish which resolver
Android's system hostname lookups use.

An ordinary Termux SSH account may be unable to read Android's Private DNS,
VPN settings, routes, or connectivity diagnostics. `settings`, `ip route`, and
`dumpsys connectivity` can return permission errors; an installed `sudo` wrapper
does not mean the phone is rooted. Inspect the settings in the phone UI when
shell permissions are insufficient.

Record Private DNS, Use Tailscale DNS, exit-node selection, and app exclusions
before changing anything. Restarting Tailscale tests for stale client state
while retaining DNS preferences. Temporarily disabling Use Tailscale DNS tests
the DNS override, but can lose MagicDNS and split-DNS name resolution. Compare
before and after, change one setting at a time, and do not claim a fix until
normal hostname-based requests work again. See [client DNS preferences](https://tailscale.com/docs/features/client/manage-preferences).

References:

- Tailscale macOS install docs: <https://tailscale.com/docs/install/mac>
- Tailscale macOS variant guidance: <https://tailscale.com/docs/concepts/macos-variants>
- Tailscale Linux install docs: <https://tailscale.com/docs/install/linux>
- Tailscale CLI reference: <https://tailscale.com/docs/reference/tailscale-cli>
