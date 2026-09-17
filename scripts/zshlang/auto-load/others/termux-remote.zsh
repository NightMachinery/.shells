##
#: Remote control of a Termux phone over SSH: clock alarms, timers,
#: notifications and toasts.
#:
#: The alarms are owned by the phone's own clock app, so they ring through Doze
#: and silent mode and survive Termux being killed, the SSH connection dropping
#: and this machine sleeping. The phone must be reachable when the alarm is
#: *set*, never when it rings.
#:
#: See =./docs/remote-termux-alarms.md= for the failure modes, which are not
#: obvious: =am= reports success even when nothing happened.
##
function h-termux-host-assert {
    : "usage: h-termux-host-assert HOST

Reject hosts that SSH could read as options or split into extra arguments."

    if (( $# != 1 )) || [[ -z "$1" || "$1" == -* || "$1" == *[[:space:]]* ]] ; then
        ecerr "${funcstack[2]:-$0}: exactly one non-empty host is required; it may not begin with '-' or contain whitespace"
        return 2
    fi
}

function h-termux-ssh {
    : "usage: h-termux-ssh HOST SCRIPT

Run SCRIPT, a single shell program, on a Termux HOST over SSH.

Uses [agfi:command] ssh so it bypasses our interactive kitty wrapper, allocates
no TTY, and is detached from local stdin so it cannot eat an enclosing
pipeline's data."

    ensure-cmd ssh @RET
    h-termux-host-assert "${1}" @RET

    local host="$1" script="${2}"
    if [[ -z "$script" ]] ; then
        ecerr "$0: empty remote script"
        return 2
    fi

    command ssh -n -T "$host" "$script"
}
##
#: The remote programs are stored whole and quoted once, rather than assembled
#: by interpolation, so that user-supplied text can never reach the remote shell
#: as code. Every value crosses as an environment variable instead.
##
typeset -g h_termux_alarm_script='
tz="$alarm_tz"
[ -n "$tz" ] || tz="$(getprop persist.sys.timezone 2>/dev/null)"
[ -n "$tz" ] || tz=UTC

#: %-H and %-M drop the zero padding on purpose: am parses --ei values as Java
#: integers, so a padded 08 is read as octal and rejected with
#: For input string: 8 under radix 8.
hm="$(TZ="$tz" date -d "@$alarm_epoch" +"%-H %-M" 2>/dev/null)" || {
    printf "%s\n" "termux-alarm: could not render epoch: $alarm_epoch" >&2
    exit 2
}
h="${hm% *}" ; m="${hm#* }"

#: termux-am hands the intent to the already-running Termux app over a socket.
#: Plain am spawns app_process and loads am.apk, which measured ~706ms of the
#: ~1000ms an alarm takes, against ~7ms for the dispatch itself. The socket only
#: exists on Termux app versions that create files/apps, so fall back silently.
#: Two paths because upstream moved it: termux-am-socket 1.5.0 looks under
#: apps/com.termux, newer app builds serve it under apps/termux-app. Checking
#: only one silently loses the speedup on half the version combinations.
AM=am
if command -v termux-am >/dev/null 2>&1 ; then
    for _s in "${PREFIX:-/data/data/com.termux/files/usr}/../apps/com.termux/termux-am/am.sock" \
              "${PREFIX:-/data/data/com.termux/files/usr}/../apps/termux-app/termux-am/am.sock" ; do
        if [ -S "$_s" ] ; then
            AM=termux-am
            break
        fi
    done
fi

set --
[ -n "$alarm_component" ] && set -- "$@" -n "$alarm_component"
set -- "$@" -a android.intent.action.SET_ALARM \
    --ei android.intent.extra.alarm.HOUR "$h" \
    --ei android.intent.extra.alarm.MINUTES "$m" \
    --ez android.intent.extra.alarm.SKIP_UI "$alarm_skip_ui" \
    --ez android.intent.extra.alarm.VIBRATE "$alarm_vibrate"
[ -n "$alarm_msg" ] && set -- "$@" --es android.intent.extra.alarm.MESSAGE "$alarm_msg"

out="$($AM start "$@" 2>&1)" ; rc=$?
if [ "$rc" -ne 0 ] ; then
    printf "%s\n" "$out" >&2
    exit "$rc"
fi
#: Silent on success. The local side already printed the time; repeating it
#: here is noise, except in the one case that matters, where this phone renders
#: the epoch differently because the two machines disagree about the timezone.
if [ -n "$alarm_expect_hm" ] && [ "$alarm_expect_hm" != "$h:$m" ] ; then
    printf "%s\n" "termux-alarm: phone set $h:$m ($tz), not $alarm_expect_hm as shown locally" >&2
fi
'

typeset -g h_termux_timer_script='
#: termux-am hands the intent to the already-running Termux app over a socket.
#: Plain am spawns app_process and loads am.apk, which measured ~706ms of the
#: ~1000ms an alarm takes, against ~7ms for the dispatch itself. The socket only
#: exists on Termux app versions that create files/apps, so fall back silently.
#: Two paths because upstream moved it: termux-am-socket 1.5.0 looks under
#: apps/com.termux, newer app builds serve it under apps/termux-app. Checking
#: only one silently loses the speedup on half the version combinations.
AM=am
if command -v termux-am >/dev/null 2>&1 ; then
    for _s in "${PREFIX:-/data/data/com.termux/files/usr}/../apps/com.termux/termux-am/am.sock" \
              "${PREFIX:-/data/data/com.termux/files/usr}/../apps/termux-app/termux-am/am.sock" ; do
        if [ -S "$_s" ] ; then
            AM=termux-am
            break
        fi
    done
fi

set --
[ -n "$timer_component" ] && set -- "$@" -n "$timer_component"
set -- "$@" -a android.intent.action.SET_TIMER \
    --ei android.intent.extra.alarm.LENGTH "$timer_seconds" \
    --ez android.intent.extra.alarm.SKIP_UI "$timer_skip_ui"
[ -n "$timer_msg" ] && set -- "$@" --es android.intent.extra.alarm.MESSAGE "$timer_msg"

out="$($AM start "$@" 2>&1)" ; rc=$?
if [ "$rc" -ne 0 ] ; then
    printf "%s\n" "$out" >&2
    exit "$rc"
fi
printf "%ss\n" "$timer_seconds"
'
##
function h-termux-alarm-set {
    : "usage: h-termux-alarm-set HOST TIMESPEC [MESSAGE...]

Set an alarm in HOST's clock app. TIMESPEC is anything [agfi:datenat-unix-v2]
accepts: compact durations like 1h30m, 90m or '1h:30m later', and prose like
'7:30', 'tomorrow 8am', 'in 3 hours' or 'next friday 9am'.

The spec is parsed here and crosses the wire as a unix timestamp, which HOST
renders into a wall-clock hour and minute against Android's *system* timezone.
An epoch is unambiguous, so a disagreement between this machine's timezone and
the phone's cannot shift the alarm. Override the rendering zone with alarm_tz.

The resolved time is printed before the intent is sent, so a misparse can be
aborted with ctrl-c. Set termux_alarm_dryrun=y to stop there and send nothing,
which is how to check a spec without leaving an alarm behind: alarms cannot be
removed programmatically.

SET_ALARM carries only an hour and a minute, so Android schedules the next
occurrence; a time already past today rolls over to tomorrow by itself.

Set termux_alarm_component to pin the receiving activity, which must be the one
declaring the SET_ALARM filter: pinning a launcher activity instead succeeds and
silently discards the extras. Left empty the intent is implicit and follows the
phone's default clock app."

    local host="${1}" spec="${2}" msg="${@[3,-1]}"
    if (( $# < 2 )) || [[ -z "$spec" ]] ; then
        ecerr "usage: $0 HOST TIMESPEC [MESSAGE...]"
        return 2
    fi

    local epoch
    epoch="$(datenat-unix-v2 "$spec")" @RET

    #: Printed before the intent goes out, so a misparse can be caught with
    #: ctrl-c rather than discovered when the alarm does not ring. This is local
    #: wall-clock time; the line HOST prints afterwards is its own rendering, and
    #: the two differ only when the machines disagree about the timezone.
    local tz="${alarm_tz}"

    local expect_hm
    if [[ -n "$tz" ]] ; then
        expect_hm="$(TZ="$tz" gdate -d "@${epoch}" +'%-H:%-M')" @RET
    else
        expect_hm="$(gdate -d "@${epoch}" +'%-H:%-M')" @RET
    fi

    local label="alarm @ ${host}"
    if [[ -n "$tz" ]] ; then
        #: name the zone, since it is then not this machine's
        label+=" [${tz}]"
    fi
    if bool "${termux_alarm_dryrun}" ; then
        label+=" (dry run)"
    fi

    #: Rendered in the same zone the phone will use, or this machine's when
    #: none is forced; showing local time for an alarm_tz alarm would name an
    #: hour the phone is never going to ring at. Seconds dropped because
    #: SET_ALARM carries only an hour and a minute.
    local human
    if [[ -n "$tz" ]] ; then
        human="$(TZ="$tz" unix2human_sec=n unix2human "$epoch")" @RET
    else
        human="$(unix2human_sec=n unix2human "$epoch")" @RET
    fi
    ecgray "${label}: ${human}"

    if bool "${termux_alarm_dryrun}" ; then
        return 0
    fi

    local component="${termux_alarm_component}"
    local skip_ui="${termux_alarm_skip_ui:-true}"
    local vibrate="${termux_alarm_vibrate:-true}"

    h-termux-ssh "$host" "env \
alarm_tz=$(gquote-sq "$tz") \
alarm_epoch=$(gquote-sq "$epoch") \
alarm_expect_hm=$(gquote-sq "$expect_hm") \
alarm_msg=$(gquote-sq "$msg") \
alarm_component=$(gquote-sq "$component") \
alarm_skip_ui=$(gquote-sq "$skip_ui") \
alarm_vibrate=$(gquote-sq "$vibrate") \
sh -c $(gquote-sq "$h_termux_alarm_script")"
}

function h-termux-timer-set {
    : "usage: h-termux-timer-set HOST DURATION [MESSAGE...]

Start a countdown timer in HOST's clock app. DURATION is anything
[agfi:dur2sec] accepts: a bare seconds count, or 90s, 30m, 2h, 3d."

    local host="${1}" dur="${2}" msg="${@[3,-1]}"
    if (( $# < 2 )) || [[ -z "$dur" ]] ; then
        ecerr "usage: $0 HOST DURATION [MESSAGE...]"
        return 2
    fi

    local seconds
    seconds="$(dur2sec "$dur")" @RET

    local component="${termux_alarm_component}"
    local skip_ui="${termux_alarm_skip_ui:-true}"

    h-termux-ssh "$host" "env \
timer_seconds=$(gquote-sq "$seconds") \
timer_msg=$(gquote-sq "$msg") \
timer_component=$(gquote-sq "$component") \
timer_skip_ui=$(gquote-sq "$skip_ui") \
sh -c $(gquote-sq "$h_termux_timer_script")"
}

function h-termux-notify {
    : "usage: h-termux-notify HOST TITLE [CONTENT...]

Post an Android notification on HOST via termux-api. Set termux_notify_id to
reuse (replace) an existing notification instead of stacking a new one.

This is not an alarm: notifications are batched by Doze and do not ring. Use
[agfi:h-termux-alarm-set] when the phone has to wake you."

    local host="${1}" title="${2}" content="${@[3,-1]}"
    if (( $# < 2 )) || [[ -z "$title" ]] ; then
        ecerr "usage: $0 HOST TITLE [CONTENT...]"
        return 2
    fi

    local id="${termux_notify_id}"
    local priority="${termux_notify_priority:-high}"

    local -a cmd
    cmd=(termux-notification --title "$title" --content "$content" --priority "$priority")
    if [[ -n "$id" ]] ; then
        cmd+=(--id "$id")
    fi

    h-termux-ssh "$host" "$(gquote-sq "$cmd[@]")"
}

function h-termux-toast {
    : "usage: h-termux-toast HOST MESSAGE...

Flash a transient toast on HOST's screen via termux-api."

    local host="${1}" msg="${@[2,-1]}"
    if (( $# < 2 )) || [[ -z "$msg" ]] ; then
        ecerr "usage: $0 HOST MESSAGE..."
        return 2
    fi

    h-termux-ssh "$host" "termux-toast -- $(gquote-sq "$msg")"
}
##
#: The clock app's API handler, pinned. This is the activity that declares the
#: SET_ALARM intent filter; it is a vendor rename of AOSP's HandleApiCalls, and it
#: shows no UI.
#:
#: Pinned because an implicit intent silently does nothing whenever several apps
#: handle SET_ALARM and none is the default: Android raises an invisible chooser
#: and every signal still reports success. An explicit component cannot reach it,
#: and a renamed class fails loudly with exit 2 instead.
#:
#: Do NOT pin com.android.deskclock.AlarmClock: that is the launcher activity,
#: declares no filter, and silently discards the extras.
#:
#: Empty string restores implicit resolution, which is portable to other phones.
#: See ./docs/remote-termux-alarms.md
typeset -g tealy_alarm_component="${tealy_alarm_component-com.android.deskclock/com.android.deskclock.HandleSetAlarmActivity}"

function tealy-alarm {
    : "usage: tealy-alarm TIMESPEC [MESSAGE...]

Set an alarm on the phone. See [agfi:h-termux-alarm-set]."

    local termux_alarm_component="${termux_alarm_component-$tealy_alarm_component}"

    h-termux-alarm-set tealy "$@"
}

function tealy-timer {
    : "usage: tealy-timer DURATION [MESSAGE...]

Start a countdown timer on the phone. See [agfi:h-termux-timer-set]."

    local termux_alarm_component="${termux_alarm_component-$tealy_alarm_component}"

    h-termux-timer-set tealy "$@"
}

function tealy-notify {
    : "usage: tealy-notify TITLE [CONTENT...]

Post a notification on the phone. See [agfi:h-termux-notify]."

    h-termux-notify tealy "$@"
}

function tealy-toast {
    : "usage: tealy-toast MESSAGE...

Flash a toast on the phone. See [agfi:h-termux-toast]."

    h-termux-toast tealy "$@"
}
##
##
#: SSH hardening and session inspection for a Termux host.
#:
#: The phone runs a stock Termux sshd: it accepts passwords, listens on every
#: interface, and - because the termux-services unit is usually down while a
#: hand-started sshd holds the port - records no authentication log at all.
#:
#: fail2ban is not an option here and never will be. It is not packaged for
#: Termux, and an unrooted Android exposes no netfilter, so there is nothing
#: for it to ban with; OpenSSH dropped tcpwrappers in 6.7, so hosts.deny is
#: gone too. OpenSSH 9.8+ PerSourcePenalties does the same job inside the
#: daemon, with no privileges, and is what the drop-in below tunes.
#:
#: See ./docs/remote-termux-ssh.md
##
typeset -g termux_harden_trusted_cidrs="${termux_harden_trusted_cidrs:-100.64.0.0/10 192.168.0.0/16 10.0.0.0/8 172.16.0.0/12 127.0.0.1}"

typeset -g h_termux_harden_script='
prefix=${PREFIX:-/data/data/com.termux/files/usr}
conf_dir=$prefix/etc/ssh/sshd_config.d
conf=$conf_dir/10-hardening.conf
rc=0

emit() {
    #: $1 is the body of the Match block, which is the one part that varies.
    cat <<EOF
#: Written by termux-harden. Edits here are lost on the next run; change the
#: function instead.
#:
#: A drop-in rather than sshd_config itself, because pkg upgrade replaces
#: sshd_config and leaves this directory alone.

PasswordAuthentication no
KbdInteractiveAuthentication no
AuthenticationMethods publickey

MaxAuthTries 2
LoginGraceTime 20

#: VERBOSE records the key fingerprint of every accepted login. Without it the
#: log can say that someone got in, but not who.
LogLevel VERBOSE

#: In-daemon rate limiting, which is what stands in for fail2ban here.
#: 24:64 is the load-bearing line: at the stock 32:128 an attacker holding a
#: /64 rotates source addresses for free and never accrues a penalty.
PerSourceNetBlockSize 24:64
PerSourcePenalties authfail:30s max:1800s
PerSourceMaxStartups 4

#: Forwarding stays on: reverse tunnels from this host are a normal use.
#: Agent forwarding does not, because agent hijacking on a phone is not worth
#: the convenience.
AllowTcpForwarding yes
AllowAgentForwarding no
X11Forwarding no

#: Anything outside the trusted ranges is refused. Binding to the tailnet
#: address instead would fail to bind whenever sshd starts while the VPN is
#: down, which is a worse failure than answering-and-refusing.
Match Address $harden_match
$1
EOF
}

if test "$harden_dryrun" = y ; then
    emit "    RefuseConnection yes"
    exit 0
fi

mkdir -p "$conf_dir" || exit 2

backup=
if test -f "$conf" ; then
    backup=$conf.bak.$$
    cp "$conf" "$backup" || exit 2
fi

restore() {
    if test -n "$backup" ; then
        cp "$backup" "$conf"
        rm -f "$backup"
    else
        rm -f "$conf"
    fi
}

#: RefuseConnection (OpenSSH 9.8+) is preferred: it drops the connection
#: before authentication, does not depend on the login name, and earns the
#: source a penalty. DenyUsers only refuses after a username was offered, but
#: it is Match-legal much further back, so it is the fallback.
mech=RefuseConnection
emit "    RefuseConnection yes" > "$conf" || { restore ; exit 2 ; }

if ! sshd -t 2>/dev/null ; then
    mech=DenyUsers
    emit "    DenyUsers *" > "$conf" || { restore ; exit 2 ; }

    if ! sshd -t 2>/dev/null ; then
        printf "%s\n" "termux-harden: neither Match variant passes sshd -t. sshd said:" >&2
        sshd -t >&2
        restore
        exit 3
    fi
fi

rm -f "$backup"
printf "%s\n" "termux-harden: installed $conf (deny mechanism: $mech)"

if test "$harden_passwd_delete" = y ; then
    if test -e "$HOME/.termux_authinfo" ; then
        if passwd -d >/dev/null 2>&1 ; then
            printf "%s\n" "termux-harden: removed the password credential; key login only"
        else
            printf "%s\n" "termux-harden: WARNING: passwd -d failed; the credential is still there" >&2
            rc=1
        fi
    else
        printf "%s\n" "termux-harden: no password credential was set"
    fi
fi

if test "$harden_supervise" != y ; then
    exit $rc
fi

SVDIR=$prefix/var/service
export SVDIR

#: sv-enable removes the service directory down file, so the daemon also comes
#: back after a reboot rather than only right now.
if command -v sv-enable >/dev/null 2>&1 ; then
    sv-enable sshd >/dev/null 2>&1
fi

#: pgrep -x, matching the exact process name: the master is sshd, while the
#: connection running this script is an sshd-session, so -x cannot match us
#: and kill the branch we are sitting on. A -f pattern would match our own
#: command line and do exactly that. Existing sshd-session children outlive
#: their master regardless, so no live session is dropped here.
old_pids=$(pgrep -x sshd 2>/dev/null)
if test -n "$old_pids" ; then
    kill $old_pids 2>/dev/null
fi

sv up sshd >/dev/null 2>&1

i=0
while test "$i" -lt 10 ; do
    sleep 1
    case "$(sv status sshd 2>&1)" in
        run:*) break ;;
    esac
    i=$((i + 1))
done

status=$(sv status sshd 2>&1)
case "$status" in
    run:*)
        printf "%s\n" "termux-harden: sshd supervised; auth log at $prefix/var/log/sv/sshd/current"
        ;;
    *)
        printf "%s\n" "termux-harden: WARNING: supervised start failed. sv says: $status" >&2
        rc=1

        #: Reachability is the thing that must not be lost, so check what is
        #: actually running rather than assuming the kill above did nothing.
        if test -n "$(pgrep -x sshd 2>/dev/null)" ; then
            printf "%s\n" "termux-harden: an unsupervised sshd is still serving; the host stays reachable, but nothing is logging." >&2
        else
            printf "%s\n" "termux-harden: nothing is listening; starting a bare sshd so the host does not go dark." >&2
            sshd
        fi
        ;;
esac

exit $rc
'

function termux-harden {
    : "usage: termux-harden HOST

Install a key-only sshd policy on a Termux HOST, drop its password credential,
and move sshd under termux-services so that authentication is actually logged.

Writes \$PREFIX/etc/ssh/sshd_config.d/10-hardening.conf, a drop-in rather than
an edit to sshd_config, which pkg upgrade replaces. The file is only left in
place if \`sshd -t\` accepts it; otherwise the previous one is restored and
this fails, so a bad config can never be what the daemon restarts into.

Access is restricted by *source address*, not by ListenAddress: sshd keeps
binding 0.0.0.0, and everything outside \$termux_harden_trusted_cidrs is
refused. Binding to a VPN address instead fails to bind whenever sshd starts
while the VPN happens to be down.

Knobs:
  termux_harden_trusted_cidrs   ranges that may connect
  termux_harden_dryrun=y        print the drop-in, change nothing
  termux_harden_passwd_keep=y   leave the password credential in place
  termux_harden_supervise=n     do not touch the service or restart sshd

See [agfi:termux-ssh-list] and [agfi:termux-ssh-log] for what it then records."

    h-termux-host-assert "${1}" @RET
    local host="$1"

    #: Built here rather than on the phone so the negation list cannot be
    #: mangled by a remote shell: Match Address takes "everything except
    #: these", so each trusted range crosses as a negated pattern.
    local -a cidrs=( ${=termux_harden_trusted_cidrs} )
    if (( ${#cidrs} == 0 )) ; then
        ecerr "$0: termux_harden_trusted_cidrs is empty; refusing to lock every source out"
        return 2
    fi
    local match="*,${(j:,:)${cidrs[@]/#/!}}"

    local dryrun=n passwd_delete=y supervise=y
    bool "${termux_harden_dryrun}" && dryrun=y
    bool "${termux_harden_passwd_keep}" && passwd_delete=n
    [[ "${termux_harden_supervise}" == n ]] && supervise=n

    h-termux-ssh "$host" "env \
harden_match=$(gquote-sq "$match") \
harden_dryrun=$(gquote-sq "$dryrun") \
harden_passwd_delete=$(gquote-sq "$passwd_delete") \
harden_supervise=$(gquote-sq "$supervise") \
sh -c $(gquote-sq "$h_termux_harden_script")"
}
##
typeset -g h_termux_ssh_list_script='
now=$(date +%s)

for d in /proc/[0-9]* ; do
    p=${d#/proc/}

    c=$(tr "\0" " " < "$d/cmdline" 2>/dev/null) || continue
    test -n "$c" || continue

    #: Skip the daemon and its per-connection children. They inherit
    #: SSH_CONNECTION from whatever shell started the daemon, so reading it off
    #: them names a peer from whenever that was - months ago, on a host where
    #: sshd was started by hand. Only the process *under* a session, the login
    #: shell or the remote command, carries a value set for that session.
    #: Termux also does not set the OpenSSH process title, so the cmdline is a
    #: bare "sshd-session -R" and cannot be parsed for a peer either.
    case "$c" in
        sshd|sshd\ *|*sshd-session*) continue ;;
    esac

    conn=$(tr "\0" "\n" < "$d/environ" 2>/dev/null | sed -n "s/^SSH_CONNECTION=//p" | head -n 1)
    test -n "$conn" || continue

    #: The /proc entry is created when the process is, so its mtime is a start
    #: time that needs no clock tick arithmetic.
    start=$(stat -c %Y "$d" 2>/dev/null) || continue
    test -n "$start" || continue

    set -- $conn
    printf "%s %s %s %s\n" "$1" "$2" "$p" "$((now - start))"
done | awk "
function trusted(ip,   a, n) {
    #: IPv6 gets the same policy as IPv4: anything not globally routable is
    #: ours. fd00::/8 is the ULA range, which is where tailscale puts its own
    #: v6 addresses (fd7a:115c::/32), so flagging it would cry wolf on a
    #: perfectly ordinary tailnet peer that happened to connect over v6.
    if (index(ip, \":\")) {
        if (ip == \"::1\") return 1
        if (tolower(substr(ip, 1, 2)) == \"fd\") return 1
        if (tolower(substr(ip, 1, 4)) == \"fe80\") return 1
        return 0
    }

    n = split(ip, a, \".\")
    if (n != 4) return 0          #: unparseable: flag it for a human
    if (a[1] == 127) return 1
    if (a[1] == 10) return 1
    if (a[1] == 192 && a[2] == 168) return 1
    if (a[1] == 172 && a[2] >= 16 && a[2] <= 31) return 1
    if (a[1] == 100 && a[2] >= 64 && a[2] <= 127) return 1   #: tailscale CGNAT
    return 0
}
function dur(s) {
    if (s == \"\") return \"?\"
    if (s < 60) return s \"s\"
    if (s < 3600) return int(s / 60) \"m\"
    if (s < 86400) return int(s / 3600) \"h\" int((s % 3600) / 60) \"m\"
    return int(s / 86400) \"d\" int((s % 86400) / 3600) \"h\"
}
{
    key = \$1 \" \" \$2
    procs[key]++
    #: (key in age) rather than a bare >, so a connection whose oldest process
    #: is 0 seconds old still records an age. Uninitialised compares equal to
    #: 0, so > alone silently leaves it empty for a brand new session.
    if (!(key in age) || \$4 > age[key]) { age[key] = \$4 ; pid[key] = \$3 }
}
END {
    if (length(procs) == 0) { print \"no live ssh sessions\" ; exit 0 }
    printf \"%-18s %-7s %-8s %-7s %s\n\", \"PEER\", \"PORT\", \"AGE\", \"PROCS\", \"\"
    for (k in procs) {
        split(k, f, \" \")
        printf \"%-18s %-7s %-8s %-7s %s\n\", f[1], f[2], dur(age[k]), procs[k], trusted(f[1]) ? \"\" : \"<-- NOT a trusted source\"
    }
}
"
'

function termux-ssh-list {
    : "usage: termux-ssh-list HOST

List the live SSH sessions on a Termux HOST: peer address, peer port, age of
the oldest process on that connection, and how many processes it owns.

A peer outside the tailnet and RFC1918 is called out, since that is the signal
worth noticing. An IPv6 peer is flagged too, on the assumption that a human
should look rather than that the tool should guess.

Reads /proc directly. ss, netstat and /proc/net/tcp are all denied to apps from
Android 10 on, so there is no socket table to consult.

See [agfi:termux-ssh-log] for what happened before now, which needs
[agfi:termux-harden] to have put sshd under supervision first."

    h-termux-host-assert "${1}" @RET

    h-termux-ssh "$1" "sh -c $(gquote-sq "$h_termux_ssh_list_script")"
}
##
typeset -g h_termux_ssh_log_script='
prefix=${PREFIX:-/data/data/com.termux/files/usr}
log=$prefix/var/log/sv/sshd/current

if test ! -s "$log" ; then
    printf "%s\n" "termux-ssh-log: $log is empty or missing." >&2
    printf "%s\n" "termux-ssh-log: sshd is not logging, which means it is not running under termux-services. Run termux-harden. Until then there is no record of who has logged in." >&2
    exit 1
fi

printf "%s\n" "== last $log_lines lines =="
tail -n "$log_lines" "$log"

printf "\n%s\n" "== tally by source =="
awk "
#: Match the outcome lines only. A bare /Accepted/ also catches
#: \"Accepted key ED25519 ... found at ...\", which is the key-match record
#: rather than a login and carries no source address, so it would show up as a
#: phantom accept from an unknown host.
/Accepted (publickey|password|keyboard-interactive|none)/ { kind = \"accepted\" ; got = 1 }
/Failed (publickey|password|keyboard-interactive|none)/   { kind = \"failed\" ; got = 1 }
/Invalid user/                                            { kind = \"invalid-user\" ; got = 1 }
got != 1 { next }
{
    got = 0
    ip = \"?\"
    for (i = 1 ; i < NF ; i++) if (\$i == \"from\") { ip = \$(i + 1) ; break }
    n[ip \" \" kind]++
}
END {
    if (length(n) == 0) { print \"no authentication events recorded yet\" ; exit 0 }
    for (k in n) printf \"%6d  %s\n\", n[k], k
}
" "$log" | sort -rn
'

function termux-ssh-log {
    : "usage: termux-ssh-log HOST [LINES]

Show the sshd authentication log on a Termux HOST and tally it by source
address. LINES defaults to 40.

This only has anything to show once sshd runs under termux-services, which is
what [agfi:termux-harden] arranges: a hand-started sshd logs through syslog,
and an unprivileged Android app cannot read logcat, so those events go nowhere
at all. An empty log here is therefore a finding, not a quiet day."

    h-termux-host-assert "${1}" @RET
    local host="$1" lines="${2:-40}"

    if [[ "$lines" != <-> ]] ; then
        ecerr "$0: LINES must be a number, got: ${lines}"
        return 2
    fi

    h-termux-ssh "$host" "env \
log_lines=$(gquote-sq "$lines") \
sh -c $(gquote-sq "$h_termux_ssh_log_script")"
}
##
function tealy-harden {
    : "usage: tealy-harden

Harden sshd on the phone. See [agfi:termux-harden]."

    termux-harden tealy "$@"
}

function tealy-ssh-list {
    : "usage: tealy-ssh-list

Show the phone's live SSH sessions. See [agfi:termux-ssh-list]."

    termux-ssh-list tealy "$@"
}

function tealy-ssh-log {
    : "usage: tealy-ssh-log [LINES]

Show the phone's sshd auth log. See [agfi:termux-ssh-log]."

    termux-ssh-log tealy "$@"
}
##
