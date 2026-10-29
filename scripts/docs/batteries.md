# batteries

`batteries` (alias `bb`) displays batteries across the tailnet in one terminal
dashboard: computers and phones first, followed by connected peripherals.
Shared headphones appear once with every reporting host beside them. Charge
bars are green above 40%, amber up to 40%, and red at or below 20%. Piped output
has no color by default. `?%` means no reading, not an empty battery.

```zsh
bb                                 # this machine and every visible tailnet peer
bb --local                         # no discovery or remote queries
bb --host laptop --host phone       # repeatable tailnet name or SSH alias
bb --exclude laptop --online        # omit this host and offline devices
bb --timeout 10                     # per-host deadline in seconds
bb --watch 30                       # explicit refresh; Ctrl-C stops
bb --json                           # one JSON document
bb --json | jq '.devices[] | {name, part, level_percent, hosts, conflict}'
```

Open a fresh Night Zsh shell, or source
`$NIGHTDIR/zshlang/auto-load/others/monitor/batteries.zsh` in an existing one.
If calling through BrishGarden after an update, run `brishz-restart` first.

## Queries without another daemon

Python 3.9+ and existing `tailscale` and `ssh` commands are sufficient on the
requesting machine. Remote Unix devices need Python 3.9+ and working SSH.
The collector is streamed over SSH stdin and executes in memory. Peers need no
installation, running battery service or matching checkout. Literal SSH aliases
are matched to tailnet DNS names through `~/.ssh/config` and its Include files,
preserving host-specific users, ports and routing.

At most eight hosts are queried concurrently. SSH uses batch mode and never
asks for a password. The per-host deadline defaults to 18 seconds; each source
command has at most four seconds within a smaller overall probe budget. A slow
peer cannot block the remaining hosts indefinitely. The command neither changes
sleep settings nor sends Wake-on-LAN packets. macOS can independently allow
network activity during sleep according to its existing power settings.
Headphone queries never use `--auto-connect`.

Nothing polls after the command exits. `--watch` gives fresh readings at the cost
of recurring network and battery queries. Its interval is the pause after each
completed round, at least five seconds. It cannot be combined with `--json`.
Offline peers remain visible unless `--online` is used. iPhones appear in the
terminal dashboard only after a battery upload; JSON retains their status.
If discovery fails, local readings remain available and `--host` can still
select SSH targets.
Partial or failed queries return exit status 1. Devices already known to be
offline or unsupported do not count as query failures.

## Sources and identity

- macOS: `pmset` for the computer, `system_profiler` for connected Bluetooth
  devices and generic earbud/case levels, and `ioreg` for HID batteries.
  Installed `sonyctl` and `bosectl` use the same battery JSON protocol as
  [the headphone helpers](headphones-battery.md). Only connected headsets are
  queried, individually addressed with `--mac`.
- Linux: `/sys/class/power_supply` and `bluetoothctl` supply available batteries
  and connected peripheral readings. No Bluetooth scan is started.
- Android: existing Termux SSH, Termux:API, and the `termux-api` package provide
  `termux-battery-status`. Normal Termux APIs do not expose connected Bluetooth
  peripheral battery levels; that limitation is displayed explicitly.
- iOS: Tailscale supplies reachability, not a remotely callable battery API.
  A publisher is required. Built-in Shortcuts can upload the phone's own battery
  as described below, but cannot provide a general peripheral-battery inventory.

Identical Bluetooth addresses identify a shared peripheral across hosts. Parts
such as left, right and case retain separate readings. Alternate names are
preserved in JSON. Matching names alone never cause a merge; addressless devices
stay scoped to their host. Address randomization or different Classic/LE
addresses can therefore prevent a cross-host merge. Conflicting percentages
appear as a range, with every host's observation retained. Devices exposing
neither a supported inventory entry nor a battery reading cannot be discovered.

## iPhone uploads with Shortcuts

The iPhone pushes a reading; the dashboard displays the last successful upload
and its age. Running `bb` cannot remotely start an ordinary iPhone Shortcut.
No HTTP listener or extra daemon is needed: uploads use the Mac's existing SSH.

On the iPhone, create a shortcut called **Publish battery**:

1. Read the percentage with **Get Battery Status** (called **Get Battery Level**
   on some versions). Also read whether it is charging if offered.
2. Use If to turn charging into the text `yes` or `no`. If unavailable, use the
   fixed text `unknown`.
3. Build a Text value with this JSON, substituting the battery and charging
   variables for the example values:

   ```json
   {"level_percent":76,"charging":"no"}
   ```

4. Add **Run Script over SSH**, targeting the receiving Mac's Tailscale DNS name
   and normal SSH account. Use its supported authentication and approve the
   first connection while unlocked. In its script field, insert the JSON Text
   variable inside the single quotes below:

   ```sh
   printf '%s\n' 'BATTERY_JSON_VARIABLE' |
     python3 "$HOME/scripts/python/batteries.py" --receive phone-tailnet-name
   ```

   Replace `phone-tailnet-name` with the phone's actual short name from
   `tailscale status`. The JSON must contain only the generated numeric
   percentage and fixed charging text. The receiving Mac needs this script;
   update another Mac's checkout through the normal Git remote first if needed.

5. Run it once unlocked and check `bb` on the Mac. Create personal automations
   on charger connection/disconnection and selected battery thresholds, running
   **Publish battery**. Select **Run Immediately** or disable **Ask Before
   Running**, as offered by your iOS version. Enable running while locked if
   present. Avoid actions requiring interaction. Test a charger-triggered upload
   with the screen locked.

Charger events plus thresholds every 10% give useful updates without a continuous
loop, with more initial setup. Additional time-of-day automations can provide
occasional reports while the battery stays between thresholds. Tailscale must be
connected and the receiving Mac reachable when an upload runs. Its Shortcuts
**Connect** action can be added if needed. Failed uploads leave the last reading
intact. Locked execution depends on actions, permissions and networking; an
unlocked test alone does not prove unattended operation.

Uploads are stored atomically under `${XDG_CACHE_HOME:-~/.cache}/batteries/published/`,
outside the repository. Every uploaded reading shows its age. After one hour it
is also marked stale; change this with `--max-age SECONDS`. Querying the receiving
Mac brings its stored phone readings into another laptop's default dashboard.
A restricted `--host` query must include the receiver if the upload is not local.

Apple documents [automatic execution](https://support.apple.com/en-euro/guide/shortcuts/apdfbdbd7123/ios)
and [battery/charger triggers](https://support.apple.com/en-euro/guide/shortcuts/apde31e9638b/ios).
Tailscale documents its [Shortcuts actions](https://tailscale.com/docs/features/mac-ios-shortcuts).

## JSON contract

`--json` produces `schema_version: 1`, UTC `generated_at`, `hosts`, deduplicated
`devices`, and discovery `warnings`. Hosts include status, original battery
observations, warnings, limitations and observation time when available.
Cached phone hosts also include `age_seconds` and `stale`.

Each device/part has `identity`, `name`, alternate `names`, `kind`, `part`,
reporting `hosts`, and individual `observations`. `level_percent` is an integer
only when all known readings agree; otherwise it is null. `level_min`,
`level_max`, `conflict`, `charging`, and `charging_conflict` preserve differences
without inventing a consensus. Unknown levels stay null; zero is a real reading.
Parts of a shared peripheral share its `identity` and differ by `part`.

Implementation: `zshlang/auto-load/others/monitor/batteries.zsh`,
`python/batteries.py`, and `python/tests/test_batteries.py`.
