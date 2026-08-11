##
#: Where are we physically? Currently a single question: are we at the LMU/CIS office?
#:
#: Used by [agfi:bell-auto] to decide whether repeated audible bells are socially
#: acceptable. See [[../../../../docs/bell-auto.md]].
##
typeset -ga office_p_domains=( 'cis.uni-muenchen.de' )
typeset -ga office_p_subnets=( '129.187.148.128/25' )
#: A VPN can hand us an LMU address from anywhere, so only physical links count.
typeset -g office_p_iface_re='^en[0-9]+$'
typeset -g office_p_cache_ttl="${office_p_cache_ttl:-120}"

redis-defvar office_p_override
aliasfn office-on office_p_override_set y
aliasfn office-off office_p_override_set n
aliasfn office-auto office_p_override_del
##
function h-ipv4-to-int {
    : "converts a dotted-quad IPv4 address to its integer form"
    local ip="${1}"

    local -a o
    o=( "${(@s:.:)ip}" )
    (( ${#o} == 4 )) || return 1

    #: `10#` forces base 10; a zero-padded octet would otherwise be read as octal.
    print -r -- $(( (10#$o[1] << 24) | (10#$o[2] << 16) | (10#$o[3] << 8) | 10#$o[4] ))
}

function h-ipv4-in-subnet-p {
    #: @warn Keep backticks out of the `:` docstrings. They are double-quoted, so zsh
    #: runs command substitution on them -- a docstring showing an example call of the
    #: enclosing function makes it recurse on every invocation.
    : "returns 0 iff <ip> falls inside <cidr>, e.g. h-ipv4-in-subnet-p 10.0.0.7 10.0.0.0/24"
    local ip="${1}" cidr="${2}"
    assert-args ip cidr @RET

    local base="${cidr%%/*}" bits="${cidr##*/}"

    local a b
    a="$(h-ipv4-to-int "$ip")" || return 1
    b="$(h-ipv4-to-int "$base")" || return 1

    local mask=$(( bits == 0 ? 0 : ((0xFFFFFFFF << (32 - bits)) & 0xFFFFFFFF) ))
    (( (a & mask) == (b & mask) ))
}
##
function office-p-net {
    : "returns 0 iff the default route looks like the CIS office network"
    local domains=( "${office_p_domains[@]}" )
    local subnets=( "${office_p_subnets[@]}" )
    local iface_re="${office_p_iface_re}"

    local iface
    iface="$(net-default-interface)" || return 1

    #: Without this guard, connecting to the LMU VPN from home would look like the
    #: office, because the tunnel hands us an LMU address and LMU resolvers.
    [[ "$iface" =~ "$iface_re" ]] || return 1

    #: Cheap (~12ms) and specific to CIS rather than "somewhere on the LMU network".
    local dns d
    dns="$(command scutil --dns 2>/dev/null)" || dns=''
    for d in "$domains[@]" ; do
        if [[ "$dns" == *"$d"* ]] ; then
            return 0
        fi
    done

    local ip net
    ip="$(net-default-ipv4)" || return 1
    for net in "$subnets[@]" ; do
        if h-ipv4-in-subnet-p "$ip" "$net" ; then
            return 0
        fi
    done

    return 1
}

function office-p-display {
    : "returns 0 iff an external display is attached
Fallback signal for [agfi:office-p]: we are usually docked at a desk when at the office."
    external-display-p
}

function office-p {
    : "returns 0 iff we are (probably) at the LMU/CIS office

Layered: manual override ([agfi:office-on] / [agfi:office-off] / [agfi:office-auto]),
then the network fingerprint, then an attached external display."
    ##
    local ttl="${office_p_cache_ttl:-120}"

    #: Checked before the cache, so toggling the override takes effect immediately.
    local override
    override="$(office_p_override_get 2>/dev/null)" || override=''
    if test -n "$override" ; then
        bool "$override"
        return $?
    fi

    #: Memoised because `stop_mode=auto` makes [agfi:bell-auto] ask on every bell, and
    #: [agfi:bella-zsh-maybe] fires one bell per completed interactive command. Unlike
    #: [agfi:bell-enabled-p] the answer changes when you walk somewhere, hence the TTL.
    typeset -g office_p_cache office_p_cache_t
    if test -n "$office_p_cache" && (( (EPOCHSECONDS - ${office_p_cache_t:-0}) < ttl )) ; then
        bool "$office_p_cache"
        return $?
    fi

    local ret=1
    if office-p-net || office-p-display ; then
        ret=0
    fi

    if (( ret == 0 )) ; then
        office_p_cache=y
    else
        office_p_cache=n
    fi
    office_p_cache_t="$EPOCHSECONDS"

    return "$ret"
}
aliasfn office-is office-p

function office-public-audio-p {
    : "returns 0 iff audio playing right now would be audible to colleagues

At the office and not on headphones. The concern is the people in the room
rather than the room itself, so headphones remove it entirely.

Accepts the same optional <name> <transport> arguments as [agfi:headphones-p],
for callers that already know the output device and want to avoid the lookup."
    office-p && ! headphones-p "$@"
}
aliasfn office-public-audio-is office-public-audio-p

function office-p-explain {
    : "shows what each [agfi:office-p] backend thinks, bypassing the cache"
    local override
    override="$(office_p_override_get 2>/dev/null)" || override=''

    ec "override: ${override:-<unset>}"
    office-p-net && ec "office-p-net: yes" || ec "office-p-net: no"
    office-p-display && ec "office-p-display: yes" || ec "office-p-display: no"
    ec "interface: $(net-default-interface 2>/dev/null)"
    ec "ipv4: $(net-default-ipv4 2>/dev/null)"
    ec "displays: $(displays-get 2>/dev/null | prefixer -o ', ' --skip-empty)"
}
##
