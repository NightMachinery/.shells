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
  [agfi:firewall-allow-mosh-darwin], and re-run that whenever mosh hangs and
  after every `brew upgrade mosh`. An upgrade changes the version-stamped
  Cellar path, and an entry has also gone stale with no upgrade while ALF kept
  listing it as allowed. So `--check`, which only reports ALF's claim, is not
  proof. `firewall-allow-mosh-darwin --probe PEER` is: it has an ssh peer send
  UDP to a disposable mosh-server and counts the bytes that arrive.

  Homebrew's `tailscaled` is another casualty, and a quieter one: nothing
  fails, Tailscale just relays every connection through DERP. Two Macs on the
  same LAN moved data at about 2.7 MB/s that way. Register it with
  [agfi:tailscale-firewall-allow-darwin], again after every
  `brew upgrade tailscale`. Then restart the daemon
  (`sudo brew services restart tailscale`): its old socket stays blocked, and
  its log keeps saying "netcheck: UDP is blocked" until then. Confirm with
  `tailscale-firewall-allow-darwin --probe PEER`, which passes only when
  `tailscale ping` finds a direct path. The Developer ID signed Tailscale.app
  does not need this.

  Homebrew's `tailscaled` is only linker-signed (`Identifier=a.out`). On one
  Mac, registering and restarting it changed nothing until it was re-signed
  ad-hoc with a real identifier, so the function now does that first whenever
  it finds the linker signature.

  One topology stays relayed regardless. When one Mac is the Internet Sharing
  hotspot for the other, the hotspot's `tailscaled` binds its UDP socket to
  the default-route interface, so its packets to the peer's `192.168.2.x`
  address leave through the uplink and are lost. A packet capture showed the
  peer's replies arriving over the hotspot bridge while every packet back went
  out the uplink. Tailscale has no supported setting for this:
  `TS_BIND_TO_INTERFACE_BY_ROUTE` does not apply to the listening socket. For
  bulk copies there, [agfi:mu-rsp-to] finds the LAN route itself (see
  [agfi:h-rsp-lan-route]).

## Tailscale SSH sessions run throttled on macOS

Homebrew's `tailscaled` LaunchDaemon sets no `ProcessType`, so launchd gives
the job light CPU and I/O limits, and every Tailscale SSH session inherits them
as a child of `tailscaled` (mosh-server started that way too). Between two Macs
a CPU loop ran 5x slower over Tailscale SSH than locally, and an interactive zsh
took 20 s to start instead of 4.8 s, which looks like `ssh HOST -t zsh` hanging.
[agfi:tailscale-fix-process-priority] sets `ProcessType` to `Interactive` and
reloads the daemon, detached so it survives being run over Tailscale SSH.
Re-run it after `brew services restart tailscale` or an upgrade, which rewrite
the plist; `--check` shows the state.

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
