#!/bin/sh
# Re-apply the com.user.publicnet pf ruleset, repairing it first if macOS has
# reset things underneath us.
#
# Run by /Library/LaunchDaemons/com.user.publicnet.pf.plist at boot, and again
# whenever /etc/pf.conf changes (WatchPaths).
#
# INSTALLED TO /usr/local/libexec AND OWNED BY root:wheel, MODE 755.
# This matters: a LaunchDaemon runs as root, so if this script lived somewhere
# the login user could write, any process running as that user could edit it
# and obtain root. Do not "helpfully" move it into ~/scripts.
#
# What it heals:
#   - /etc/pf.conf replaced by a macOS update, losing the anchor lines. This is
#     the single most likely way the protection lapses, and it is silent.
#   - /etc/pf.anchors/com.user.publicnet deleted, restored from the copy kept
#     next to this script.

set -u

ANCHOR_NAME='com.user.publicnet'
ANCHOR_DST="/etc/pf.anchors/${ANCHOR_NAME}"
ANCHOR_MASTER="/usr/local/libexec/${ANCHOR_NAME}.anchor"
PF_CONF='/etc/pf.conf'
TAG='publicnet-pf'

log() { echo "$(date '+%Y-%m-%dT%H:%M:%S') [$TAG] $*" ; }

# 1. Anchor file present?
if [ ! -f "$ANCHOR_DST" ] ; then
    if [ -f "$ANCHOR_MASTER" ] ; then
        log "anchor missing at ${ANCHOR_DST}; restoring from master copy"
        cp "$ANCHOR_MASTER" "$ANCHOR_DST" || { log 'FAILED to restore anchor'; exit 1; }
        chown root:wheel "$ANCHOR_DST"
        chmod 644 "$ANCHOR_DST"
    else
        log "FATAL: neither ${ANCHOR_DST} nor ${ANCHOR_MASTER} exists"
        exit 1
    fi
fi

# 2. Is it still referenced from the main ruleset? A macOS update replaces
#    /etc/pf.conf wholesale, which drops our two lines without warning.
if ! grep -q "$ANCHOR_NAME" "$PF_CONF" 2>/dev/null ; then
    log "pf.conf no longer references the anchor (likely a macOS update); re-adding"
    cp "$PF_CONF" "${PF_CONF}.bak-selfheal-$(date +%Y%m%d-%H%M%S)" 2>/dev/null
    printf '\nanchor "%s"\nload anchor "%s" from "%s"\n' \
        "$ANCHOR_NAME" "$ANCHOR_NAME" "$ANCHOR_DST" >> "$PF_CONF" \
        || { log 'FAILED to append anchor lines'; exit 1; }
fi

# 3. Validate before loading. A broken ruleset that fails to load leaves pf in
#    whatever state it was in, which may be "no rules at all".
#
#    Exit status alone is not enough: pfctl prints diagnostics such as
#    "unknown icmp6-type" and still exits 0, silently dropping the offending
#    rule from the loaded set. Treat any output that is not the routine -f
#    warning as a failure.
pf_out=$(pfctl -n -f "$PF_CONF" 2>&1) ; pf_rc=$?
pf_err=$(printf '%s\n' "$pf_out" \
    | grep -viE 'could result in flushing|present in the main ruleset|see /etc/pf\.conf' \
    | grep -vE '^[[:space:]]*$')
if [ "$pf_rc" -ne 0 ] || [ -n "$pf_err" ] ; then
    log 'FATAL: ruleset does not validate; refusing to load'
    printf '%s\n' "$pf_err" | sed "s/^/$(date '+%Y-%m-%dT%H:%M:%S') [$TAG]   /"
    exit 1
fi

# 4. Load. -E enables with a reference count so this and Internet Sharing can
#    both hold pf on without either disabling it under the other.
if pfctl -E -f "$PF_CONF" >/dev/null 2>&1 ; then
    log 'ruleset loaded'
else
    log 'FAILED to load ruleset'
    exit 1
fi

# 5. Confirm the rules actually landed in the anchor, rather than trusting the
#    exit code -- pfctl has a habit of succeeding while doing nothing useful.
count=$(pfctl -a "$ANCHOR_NAME" -s rules 2>/dev/null | grep -c .)
if [ "${count:-0}" -gt 0 ] ; then
    log "verified: ${count} rules active in anchor ${ANCHOR_NAME}"
else
    log "WARNING: anchor ${ANCHOR_NAME} is empty after load"
    exit 1
fi

exit 0
