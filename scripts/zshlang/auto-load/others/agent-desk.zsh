##
#: The machine side of agent-desk (github.com/NightMachinery/agent-desk): the
#: commands its ~/.config/agent-desk/config.toml names. The core runs each
#: under sh with the event as JSON on stdin. See docs/agent-desk.md.
##
typeset -g agent_desk_tlg_idle_min_s="${agent_desk_tlg_idle_min_s:-600}"
#: A notification click reuses a browser tab already on the dashboard (see
#: [agfi:h-agent-desk-open-tab]); n opens a new tab each time.
typeset -g agent_desk_open_reuse_tab="${agent_desk_open_reuse_tab:-y}"

function agent-desk-config-install {
    : "usage: agent-desk-config-install [--force] [--base-url URL] [--remote-label NAME] [--ssh-host HOST=TARGET]...
Writes ~/.config/agent-desk/config.toml naming the commands below, the
dashboard's address (base_url), the away test, the remote channel's name and
the machines whose desk is pushed to at once (ssh_hosts). Each value defaults
to a variable, so private ones can live outside this repository:
agent_desk_base_url (else the desk app's address from html-reports' apps.py),
agent_desk_remote_label (else Telegram) and the associative array
agent_desk_ssh_hosts."
    #: Generated, not linked: clicks run under launchd, whose PATH finds
    #: Apple's /bin/zsh, which lacks the PCRE module our startup needs (see
    #: [agfi:h-agent-done-notify]). So the file names this machine's own zsh.
    ##
    local force=n base_url="${agent_desk_base_url-}" label="${agent_desk_remote_label:-Telegram}"
    local -A hosts=()
    (( ${+agent_desk_ssh_hosts} )) && hosts=( "${(@kv)agent_desk_ssh_hosts}" )
    while (( $# )) ; do
        case "$1" in
            --force) force=y ;;
            --base-url) base_url="${2-}" ; shift ;;
            --base-url=*) base_url="${1#*=}" ;;
            --remote-label) label="${2-}" ; shift ;;
            --remote-label=*) label="${1#*=}" ;;
            --ssh-host) [[ "${2-}" == ?*=?* ]] || { ecerr "$0: --ssh-host takes HOST=TARGET" ; return 2 } ; hosts[${2%%=*}]="${2#*=}" ; shift ;;
            *) ecerr "$0: unknown argument: $1" ; return 2 ;;
        esac
        shift
    done
    local dir="${XDG_CONFIG_HOME:-${HOME}/.config}/agent-desk"
    local file="${dir}/config.toml" zsh_path
    if test -e "${file}" && ! bool "${force}" ; then
        ecerr "$0: ${file} exists; pass --force to replace it"
        return 1
    fi
    test -n "$base_url" || base_url="$(h-agent-desk-base-url-guess)" || base_url=''
    zsh_path="$(whence -p zsh)" @RET
    zsh_path="${zsh_path:a}"
    command mkdir -p "${dir}" @RET

    local z="'${zsh_path}' -c"
    local -a lines=(
        "# Written by agent-desk-config-install; see ~/scripts/docs/agent-desk.md."
        "notify_command = $(h-agent-desk-toml-str "$z agent-desk-notify")"
        "dismiss_command = $(h-agent-desk-toml-str "$z agent-desk-dismiss")"
        "open_command = $(h-agent-desk-toml-str "$z agent-desk-open")"
        "away_command = $(h-agent-desk-toml-str "$z agent-desk-away-p")"
        "remote_label = $(h-agent-desk-toml-str "$label")"
    )
    test -n "$base_url" && lines+=( "base_url = $(h-agent-desk-toml-str "$base_url")" )
    if (( ${#hosts} )) ; then
        lines+=( '' '[ssh_hosts]' )
        local h
        for h in "${(@ko)hosts}" ; do
            lines+=( "$(h-agent-desk-toml-str "$h") = $(h-agent-desk-toml-str "${hosts[$h]}")" )
        done
    fi
    lines+=( '' '[tasks]'
        "launch_command = $(h-agent-desk-toml-str "$z agent-desk-task-launch")"
        "quota_command = $(h-agent-desk-toml-str "$z agent-desk-quota-json")"
    )
    lines+=( '' '[quota]'
        "stopped_command = $(h-agent-desk-toml-str "$z paseo-auto-continue-stopped")"
        "auto_continue_command = $(h-agent-desk-toml-str "$z 'agent-desk-auto-continue \"\$@\"' zsh \"\$@\"")"
    )
    if (( ${+commands[hs]} )) ; then
        lines+=( '' '[screen]'
            "banner_on = $(h-agent-desk-toml-str "$z 'agent-desk-screen-banner on'")"
            "banner_off = $(h-agent-desk-toml-str "$z 'agent-desk-screen-banner off'")"
        )
    fi
    print -rl -- "${lines[@]}" > "${file}" @RET
    ec "${file}"
}

function agent-desk-auto-continue {
    : "[quota] auto_continue_command: list prints the Paseo agents registered to resume themselves when their quota returns; on|off ID registers or unregisters one (paseo-auto-continue)."
    case "$1" in
        (list) h-paseo-auto-continue list | command cut -f1 ;;
        (on) paseo-auto-continue-on "$2" ;;
        (off) paseo-auto-continue-off "$2" ;;
        (*) ecerr "usage: agent-desk-auto-continue list|on ID|off ID" ; return 2 ;;
    esac
}

function agent-desk-screen-banner {
    : "banner_on/banner_off of agent-desk's screen lock: the Hammerspoon agent banner. on reads AGENT_DESK_REASON and AGENT_DESK_SECONDS."
    local reason secs
    case "$1" in
        (on)
            reason="${AGENT_DESK_REASON:-An agent is driving the screen}"
            reason="${reason//\]==\]/] ==]}"
            secs="${AGENT_DESK_SECONDS:-1800}"
            [[ "${secs}" == <-> ]] || secs=1800
            command hs -q -c "agentBannerOn([==[${reason}]==], ${secs})" </dev/null
            ;;
        (off) command hs -q -c 'agentBannerOff()' </dev/null ;;
        (*) ecerr "usage: agent-desk-screen-banner on|off" ; return 2 ;;
    esac
}

function agent-desk-task-launch {
    : "launch_command: starts a Paseo agent on the deferred task on stdin and prints its id. The task's profile is provider[/model][@thinking], default \$agent_desk_task_profile or codex/gpt-6.1-terra@high; Codex runs in auto-review mode."
    local json prompt cwd title profile provider thinking id
    json="$(command cat)" @RET
    prompt="$(print -r -- "${json}" | command jq -r '.prompt')" @RET
    cwd="$(print -r -- "${json}" | command jq -r '.cwd // empty')"
    title="$(print -r -- "${json}" | command jq -r '"\(.display_id) \(.title)"')"
    id="$(print -r -- "${json}" | command jq -r '.id')"
    profile="$(print -r -- "${json}" | command jq -r '.profile // empty')"
    test -n "${profile}" || profile="${agent_desk_task_profile:-codex/gpt-6.1-terra@high}"
    provider="${profile%%@*}"
    thinking=''
    [[ "${profile}" == *@* ]] && thinking="${profile#*@}"
    test -d "${cwd}" || cwd="${HOME}"
    local -a args=( run --no-wait -q --provider "${provider}" --cwd "${cwd}" --title "${title[1,80]}" --label "agent-desk.task=${id}" )
    test -n "${thinking}" && args+=( --thinking "${thinking}" )
    [[ "${provider}" == codex* ]] && args+=( --mode auto-review )
    #: The user launches it, so it is no agent's child.
    command env -u PASEO_AGENT_ID "${agent_desk_paseo:-paseo}" "${args[@]}" "${prompt}"
}

function agent-desk-quota-json {
    : "quota_command: prints every account's usage as {\"gauges\": {name: {pct, reset}}}, through the quota-management skill's readers"
    local dir="${HOME}/.claude/skills/quota-management/scripts"
    test -r "${dir}/quota_readers.py" || { ecerr "$0: no quota readers at ${dir}" ; return 1 }
    QUOTA_READERS_DIR="${dir}" command python3 -c '
import json, os, sys
sys.path.insert(0, os.environ["QUOTA_READERS_DIR"])
import quota_readers as q
_, readers = q.choose_readers()
gauges = {}
for source, reader in readers.items():
    gauges.update(q.measure(source, reader)[0])
print(json.dumps({"gauges": gauges}))
'
}

function h-agent-desk-toml-str {
    : "a TOML basic string holding \$1"
    local s="${1}"
    s="${s//\\/\\\\}"
    s="${s//\"/\\\"}"
    ec "\"${s}\""
}

function h-agent-desk-base-url-guess {
    : "the desk app's address as html-reports' apps.py knows it, where that is installed"
    local apps="${HOME}/code/skills/html-reports/skills/html-reports/bin/apps.py" out
    test -x "$apps" || return 1
    out="$(gtimeout 20 "$apps" status desk 2>/dev/null)" || return 1
    out="${${(@M)${(f)out}:#desk *}[1]}"
    out="${${(s: :)out}[2]}"
    [[ "$out" == http*://* ]] || return 1
    ec "$out"
}

function h-agent-desk-fields {
    #: The payload on stdin into `agent_desk_f': id, display, title, path,
    #: kind, level, the binary, whether the event is from this host, its host,
    #: a done report's report file, and the routing fields (action, channel,
    #: dest, handle, url, project, class, agent title, and the kind of the
    #: event that caused a dismiss). The body goes to `agent_desk_body'.
    ##
    local json
    json="$(command cat)" @RET
    ensure-cmd jq @RET
    typeset -gA agent_desk_f=()
    local -a f
    f=( "${(@ps:\t:)$(print -r -- "${json}" | command jq --raw-output '
        def one: tostring | gsub("[\t\n\r]"; " ");
        [ (.event.id // ""), (.display_id // ""), ((.event.title // "") | one),
          (.path // ""), (.event.kind // ""), (.event.level // ""), (.agent_desk // ""),
          (.local | tostring), (.event.host // ""), (.event.meta.report // ""),
          (.action // ""), (.channel // ""), ((.dest // "") | one), "",
          (.url // ""), (.project // ""), (.class // ""), ((.agent_title // "") | one),
          (.cause.kind // "") ] | map(gsub("\t"; " ")) | @tsv')}" ) @RET
    agent_desk_f=( id "${f[1]-}" display "${f[2]-}" title "${f[3]-}" path "${f[4]-}"
        kind "${f[5]-}" level "${f[6]-}" bin "${f[7]-}" local "${f[8]-}" host "${f[9]-}"
        report "${f[10]-}" action "${f[11]-}" channel "${f[12]-}" dest "${f[13]-}"
        handle "${f[14]-}" url "${f[15]-}" project "${f[16]-}" class "${f[17]-}"
        agent_title "${f[18]-}" cause "${f[19]-}" )
    #: Read apart from the rest: a Telegram handle holds a tab.
    agent_desk_f[handle]="$(print -r -- "${json}" | command jq --raw-output '.handle // ""')" || agent_desk_f[handle]=''
    typeset -g agent_desk_body agent_desk_refs
    agent_desk_body="$(print -r -- "${json}" | command jq --raw-output '.event.body // ""')" || agent_desk_body=''
    #: The id mentions in the title and body, mapped to their pages.
    agent_desk_refs="$(print -r -- "${json}" | command jq --compact-output '.refs // {}')" || agent_desk_refs='{}'
    test -n "${agent_desk_f[id]}${agent_desk_f[path]}"
}

function h-agent-desk-what {
    : "what the item is, in words, from agent_desk_f"
    case "${agent_desk_f[class]:-${agent_desk_f[kind]}-${agent_desk_f[level]}}" in
        ask-blocked) ec 'blocked question' ;;
        ask-default) ec 'question with a default' ;;
        response) ec 'response to you' ;;
        tell*|tell-notify) ec notice ;;
        done*) ec 'done report' ;;
        *) ec "${agent_desk_f[kind]}" ;;
    esac
}

function agent-desk-notify {
    : "notify_command: raises the event on stdin on one channel and prints its handle: a desktop card, or a Telegram message (channel remote)"
    ##
    h-agent-desk-fields || return $?
    case "${agent_desk_f[channel]}" in
        remote) h-agent-desk-notify-remote ;;
        *) h-agent-desk-notify-desktop ;;
    esac
}

function h-agent-desk-notify-desktop {
    #: One group per event id, so one question never replaces another, and
    #: [agfi:agent-desk-dismiss] can take back exactly this card. The group is
    #: the handle.
    ##
    local -A f=( "${(@kv)agent_desk_f}" )

    local what="${f[level]}"
    case "${f[kind]}" in
        tell) what=notice ; [[ "${f[class]}" == response ]] && what=response ;;
        done) what=done ;;
    esac
    local body="${f[path]} · ${what}"
    [[ "${f[local]}" == true ]] || body+=" · @${f[host]}"
    local title="${f[title]:-${f[display]}}"
    (( ${#title} > 80 )) && title="${title[1,79]}…"
    local group="agent-desk-${f[id]}"

    local -a notif_on_click=()
    test -n "${f[bin]}" && notif_on_click=( "${f[bin]}" open "${f[display]}" )
    notif_app=desk notif_ignore_dnd_p=n notif_tlg=n \
        notif_group="${group}" notif_title="${title}" \
        notif_body="${body}" notif_subtitle='' notif_image='' \
        notif "${title}" >/dev/null 2>&1 || true
    ec "${group}"
}

typeset -g agent_desk_tlg_body_max="${agent_desk_tlg_body_max:-3000}"
function h-agent-desk-notify-remote {
    #: One rich Telegram message: the item's title, where it is and who wrote
    #: it, a link to its page, and the body folded away. Its handle lets
    #: [agfi:agent-desk-dismiss] delete it once the item is read. The core
    #: decides when to raise this (the notify.remote setting); the destination
    #: is notify.remote.dest, else `tlg_notifs'.
    ##
    local -A f=( "${(@kv)agent_desk_f}" )
    local dest="${f[dest]:-${tlg_notifs-}}"
    local title="${f[title]:-${f[display]}}"
    title="$(h-agent-desk-link-ids "${agent_desk_refs:-{\}}" <<<"${title}")" || title="${f[title]:-${f[display]}}"
    local -a lines=( "**${title}**" "${f[path]} · $(h-agent-desk-what)${f[agent_title]:+ · ${f[agent_title]}} · ${f[display]}" )
    test -n "${f[url]}" && lines+=( '' "[Open on the desk](${f[url]})" )
    local body="${agent_desk_body}"
    if test -n "$body" ; then
        (( ${#body} > agent_desk_tlg_body_max )) && body="${body[1,agent_desk_tlg_body_max]}…"
        body="$(h-agent-desk-link-ids "${agent_desk_refs:-{\}}" <<<"${body}")" || body="${agent_desk_body}"
        lines+=( '' '<details><summary>Details</summary>' '' "$body" '' '</details>' )
    fi
    tsend-tracked "$dest" rich "${(F)lines}"
}

function h-agent-desk-link-ids {
    : "usage: h-agent-desk-link-ids REFS_JSON <<<TEXT
Prints TEXT with each id mention named in REFS_JSON (the notify payload's
refs: id to page URL) turned into a Markdown link [q-8tqk](url): plain
mentions, and inline code spans that are exactly one id. Fenced code blocks,
longer code spans and existing links are left as they are."
    command python3 -c '
import json, re, sys
refs = json.loads(sys.argv[1] or "{}")
text = sys.stdin.read()
if text.endswith("\n"):
    text = text[:-1]
ID = re.compile(r"\b[a-z]-[0-9a-hjkmnp-tv-z]{4,}\b")
def plain(s):
    return ID.sub(lambda m: "[%s](%s)" % (m.group(0), refs[m.group(0)]) if m.group(0) in refs else m.group(0), s)
def line(s):
    out, rest = [], s
    while rest:
        m = re.search(r"`[^`]*`|\[[^\]]*\]\([^)]*\)", rest)
        if not m:
            out.append(plain(rest))
            break
        out.append(plain(rest[:m.start()]))
        tok = m.group(0)
        inner = tok[1:-1] if tok.startswith("`") else None
        out.append("[%s](%s)" % (inner, refs[inner]) if inner in refs else tok)
        rest = rest[m.end():]
    return "".join(out)
fence = False
lines = []
for l in text.split("\n"):
    if l.lstrip().startswith("```"):
        fence = not fence
        lines.append(l)
        continue
    lines.append(l if fence else line(l))
print("\n".join(lines))
' "$1"
}

function agent-desk-away-p {
    : "away_command: exits 0 when the user is away from this machine"
    h-agent-desk-away-p
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
    : "dismiss_command: takes back the notification of the event on stdin: its desktop card, or its Telegram message (deleted, or edited down to one line once too old to delete)"
    ##
    h-agent-desk-fields || return $?
    local -A f=( "${(@kv)agent_desk_f}" )
    if [[ "${f[channel]}" == remote ]] ; then
        local verb=read
        case "${f[cause]}" in
            answer) verb=answered ;;
            set|link|clear) verb=replaced ;;
        esac
        tsend-retract-handle "${f[handle]}" "✓ ${verb}: ${f[title]:-${f[display]}}"
        return $?
    fi
    #: A notification raised before handles existed has none: its group is the id.
    notif_app=desk notif-os-remove "${f[handle]:-agent-desk-${f[id]}}"
}

typeset -g agent_desk_tabs_app="${agent_desk_tabs_app:-${HOME}/Applications/Agent Desk Tabs.app}"

function agent-desk-tabs-install {
    : "usage: agent-desk-tabs-install
Builds macos/desk-tabs/main.swift into ~/Applications/Agent Desk Tabs.app
(agent_desk_tabs_app), the helper behind [agfi:h-agent-desk-open-tab]. It is
ad-hoc signed with the default designated requirement, so each build (or a
replaced binary) asks for the Automation permission again on its first click,
rather than inheriting the grant."
    local src="${NIGHTDIR:-${HOME}/scripts}/macos/desk-tabs/main.swift"
    local app="${agent_desk_tabs_app}" tmp
    [[ -e "${src}" ]] || { ecerr "$0: missing ${src}" ; return 1 }
    tmp="$(command mktemp -d)" || return $?
    {
        h-agent-desk-tabs-build "${src}" "${tmp}/a.app" || return $?
        command rm -rf -- "${app}" || return $?
        command mkdir -p "${app:h}" || return $?
        command mv -- "${tmp}/a.app" "${app}" || return $?
        ec "Installed ${app}; the first notification click asks to let it control the browser."
    } always {
        command rm -rf -- "${tmp}"
    }
}

function h-agent-desk-tabs-build {
    : "usage: h-agent-desk-tabs-build SRC APP: compiles and signs the bundle APP"
    local src="$1" app="$2" plist="$2/Contents/Info.plist" kv
    command mkdir -p "${app}/Contents/MacOS" || return $?
    command xcrun swiftc -O -framework AppKit -o "${app}/Contents/MacOS/agent-desk-tabs" "${src}" || return $?
    command plutil -create xml1 "${plist}" || return $?
    for kv in CFBundleIdentifier=com.user.night-agent-desk.tabs 'CFBundleName=Agent Desk Tabs' \
        CFBundleExecutable=agent-desk-tabs CFBundlePackageType=APPL CFBundleVersion=1 \
        CFBundleShortVersionString=1.0 LSMinimumSystemVersion=11.0 \
        'NSAppleEventsUsageDescription=Switches your open agent-desk tab to the item you clicked, instead of opening a new tab.' ; do
        command plutil -insert "${kv%%=*}" -string "${kv#*=}" "${plist}" || return $?
    done
    command plutil -insert LSUIElement -bool true "${plist}" || return $?
    command codesign --force --sign - "${app}" || return $?
}

function h-agent-desk-open-tab {
    : "usage: h-agent-desk-open-tab URL
Hands URL to the Agent Desk Tabs helper ([agfi:agent-desk-tabs-install]),
which switches a browser tab already on the dashboard to it, or opens it
normally. Only that helper holds the permission to control the browser, and
it only moves a dashboard tab to another dashboard page. Returns 1 when the
helper is not installed, so the caller opens URL itself."
    local url="${1-}"
    [[ -n "${url}" && -x "${agent_desk_tabs_app}/Contents/MacOS/agent-desk-tabs" ]] || return 1
    #: Through LaunchServices, so the helper (not this shell's app) is the
    #: one macOS asks about; -n so its arguments reach a fresh instance.
    command open -n -g -a "${agent_desk_tabs_app}" --args "${url}"
}

function agent-desk-open {
    : "open_command: opens a done report, else the item's page on the dashboard, else the inbox scoped to the item's node in a new kitty window"
    ##
    h-agent-desk-fields || return $?
    local -A f=( "${(@kv)agent_desk_f}" )
    #: A done report opens the report itself, as Agent Done's card did.
    if [[ "${f[kind]}" == done && -n "${f[report]}" && -e "${f[report]}" ]] ; then
        h-agent-done-open-report "${f[report]}"
        return $?
    fi
    if test -n "${f[url]}" ; then
        if isDarwin ; then
            if bool "${agent_desk_open_reuse_tab}" && h-agent-desk-open-tab "${f[url]}" ; then
                return 0
            fi
            command open "${f[url]}"
        else
            command xdg-open "${f[url]}"
        fi
        return $?
    fi
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

function h-agent-desk-on-node-p {
    : "usage: h-agent-desk-on-node-p <app> <session-id>; the session has joined a desk node and the desk notifies, so its questions and done report raise their own notifications ([agfi:h-bell-agent-hook] skips its turn-end bell)"
    #: The bell hook runs in BrishGarden, which has none of the agent's
    #: environment, so the session is named to `agent-desk whoami' explicitly.
    ##
    local app="${1}" id="${2}"
    test -n "$id" || return 1
    (( ${+commands[agent-desk]} )) || return 1
    h-agent-desk-notifies-p || return 1
    local -a env
    case "${app:l}" in
        claude*) env=( AI_AGENT=claude-code CLAUDE_CODE_SESSION_ID="$id" ) ;;
        codex*) env=( AI_AGENT=codex CODEX_THREAD_ID="$id" ) ;;
        *) return 1 ;;
    esac
    command env -u PASEO_AGENT_ID -u TMUX_PANE -u AI_AGENT -u CLAUDE_CODE_SESSION_ID \
        -u CODEX_THREAD_ID -u CODEX_SESSION_ID -u ANTIGRAVITY_CONVERSATION_ID \
        "${env[@]}" gtimeout 5 agent-desk whoami >/dev/null 2>&1
}

##
#: agent-done's use of the desk ([agfi:agent-done]): the report lists the
#: session's unanswered blocked questions, and the session's end is filed as a
#: done event. Neither refuses or removes anything.
##
function h-agent-done-desk-open {
    : "the open blocked questions of this session's node, one \"<id>  <title>\" per line"
    ##
    (( ${+commands[agent-desk]} )) || return 1
    ensure-cmd jq @RET
    command agent-desk show --mine --format json 2>/dev/null | command jq --raw-output '
        .needs_you[].items[] | select(.event.level == "blocked") | "\(.display_id)  \(.event.title)"'
}

function h-agent-desk-notifies-p {
    : "agent-desk on this machine has a notify_command, so its events raise cards"
    #: The same file the core reads (config.Path in the agent-desk core).
    ##
    local file="${AGENT_DESK_CONFIG:-${XDG_CONFIG_HOME:-${HOME}/.config}/agent-desk/config.toml}"
    test -r "${file}" && command grep -Eq '^[[:space:]]*notify_command[[:space:]]*=[[:space:]]*"[^"]' "${file}"
}

function h-agent-done-desk-file {
    : "usage: h-agent-done-desk-file <title> <report>; files the session's end on agent-desk; fails when it is not installed or refuses (a worker)"
    #: The summary comes from `agent_done_summary', as for the report.
    ##
    local title="${1}" report="${2}"
    (( ${+commands[agent-desk]} )) || return 1
    print -r -- "${agent_done_summary-}" |
        command agent-desk done --title "${title:-Session finished}" --report "${report}"
}
