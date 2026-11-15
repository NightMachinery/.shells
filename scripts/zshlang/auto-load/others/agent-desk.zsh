##
#: The machine side of agent-desk (github.com/NightMachinery/agent-desk): the
#: commands its ~/.config/agent-desk/config.toml names. The core runs each
#: under sh with the event as JSON on stdin. See docs/agent-desk.md.
##
typeset -g agent_desk_tlg_idle_min_s="${agent_desk_tlg_idle_min_s:-600}"

function agent-desk-config-install {
    : "usage: agent-desk-config-install [--force]; writes ~/.config/agent-desk/config.toml naming the commands below"
    #: Generated, not linked: clicks run under launchd, whose PATH finds
    #: Apple's /bin/zsh, which lacks the PCRE module our startup needs (see
    #: [agfi:h-agent-done-notify]). So the file names this machine's own zsh.
    ##
    local force=n
    [[ "${1:-}" == --force ]] && force=y
    local dir="${XDG_CONFIG_HOME:-${HOME}/.config}/agent-desk"
    local file="${dir}/config.toml" zsh_path
    if test -e "${file}" && ! bool "${force}" ; then
        ecerr "$0: ${file} exists; pass --force to replace it"
        return 1
    fi
    zsh_path="$(whence -p zsh)" @RET
    zsh_path="${zsh_path:a}"
    command mkdir -p "${dir}" @RET
    print -r -- "# Written by agent-desk-config-install; see ~/scripts/docs/agent-desk.md.
notify_command = \"'${zsh_path}' -c agent-desk-notify\"
dismiss_command = \"'${zsh_path}' -c agent-desk-dismiss\"
open_command = \"'${zsh_path}' -c agent-desk-open\"" > "${file}" @RET
    ec "${file}"
}

function h-agent-desk-fields {
    #: The payload on stdin into `agent_desk_f': id, display, title, path,
    #: kind, level, the binary, and whether the event is from this host.
    ##
    local json
    json="$(command cat)" @RET
    ensure-cmd jq @RET
    typeset -gA agent_desk_f=()
    local -a f
    f=( "${(@ps:\t:)$(print -r -- "${json}" | command jq --raw-output '
        [ (.event.id // ""), (.display_id // ""), ((.event.title // "") | gsub("[\t\n\r]"; " ")),
          (.path // ""), (.event.kind // ""), (.event.level // ""), (.agent_desk // ""),
          (.local | tostring), (.event.host // "") ] | @tsv')}" ) @RET
    agent_desk_f=( id "${f[1]-}" display "${f[2]-}" title "${f[3]-}" path "${f[4]-}"
        kind "${f[5]-}" level "${f[6]-}" bin "${f[7]-}" local "${f[8]-}" host "${f[9]-}" )
    test -n "${agent_desk_f[id]}${agent_desk_f[path]}"
}

function agent-desk-notify {
    : "notify_command: an Agent Desk notification for the event on stdin"
    #: One group per event id, so one question never replaces another, and
    #: [agfi:agent-desk-dismiss] can take back exactly this card.
    ##
    h-agent-desk-fields || return $?
    local -A f=( "${(@kv)agent_desk_f}" )

    local what="${f[level]}" dnd=n
    case "${f[kind]}" in
        ask) [[ "${f[level]}" == blocked ]] && dnd=y ;;
        tell) what=notice ;;
        done) what=done ;;
    esac
    local body="${f[path]} · ${what}"
    [[ "${f[local]}" == true ]] || body+=" · @${f[host]}"
    local title="${f[title]:-${f[display]}}"
    (( ${#title} > 80 )) && title="${title[1,79]}…"

    local -a notif_on_click=()
    test -n "${f[bin]}" && notif_on_click=( "${f[bin]}" open "${f[display]}" )
    notif_app=desk notif_ignore_dnd_p="${dnd}" notif_tlg=n \
        notif_group="agent-desk-${f[id]}" notif_title="${title}" \
        notif_body="${body}" notif_subtitle='' notif_image='' \
        notif "${title}" >/dev/null 2>&1 || true

    if [[ "${f[kind]}" == ask && "${f[level]}" == blocked && "${f[local]}" == true ]] &&
        h-agent-desk-away-p ; then
        #: Title only: the body may hold more than belongs on a phone's lock screen.
        tnotif "agent-desk ${f[display]}: ${title}" >/dev/null 2>&1 || true
    fi
}

function h-agent-desk-away-p {
    #: The user is away: the screen is locked, or the keyboard and mouse have
    #: been idle for `agent_desk_tlg_idle_min_s'. Cannot tell means not away.
    ##
    h-agent-usage-screen-locked-p 2>/dev/null && return 0
    local idle
    idle="$(h-agent-usage-idle-s 2>/dev/null)" || return 1
    (( idle >= agent_desk_tlg_idle_min_s ))
}

function agent-desk-dismiss {
    : "dismiss_command: removes the notification of the event on stdin"
    ##
    h-agent-desk-fields || return $?
    notif_app=desk notif-os-remove "agent-desk-${agent_desk_f[id]}"
}

function agent-desk-open {
    : "open_command: opens the inbox scoped to the item's node in a new kitty window"
    ##
    h-agent-desk-fields || return $?
    local -A f=( "${(@kv)agent_desk_f}" )
    local bin="${f[bin]:-agent-desk}" sock
    if sock="$(kitty-socket-get 2>/dev/null)" &&
        command kitty @ --to "${sock}" launch --type=os-window --title "agent-desk: ${f[path]}" \
            "${bin}" inbox "${f[path]}" >/dev/null ; then
        isDarwin && command open -a kitty
        return 0
    fi
    notif_app=desk notif_group="agent-desk-open" notif_title='Cannot open agent-desk' \
        notif_body="No kitty to open it in. Run: agent-desk inbox ${f[path]}" \
        notif "agent-desk inbox ${f[path]}" >/dev/null 2>&1
    return 1
}
