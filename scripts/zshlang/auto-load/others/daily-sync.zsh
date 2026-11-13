##
#: Daily repository sync across several machines, driven by a scheduled agent.
#: The agent keeps what needs judgement (grouping commits, the privacy check,
#: conflicts, repairing paper links); everything mechanical is here, so a run
#: is a handful of commands. See =docs/daily-sync.md=.
#:
#: Which repositories and machines are synced is private and lives in
#: `daily_sync_config', outside this repository.
#:
#: The other machines are driven over ssh with scripts generated here, so they
#: need git and zsh but not this file: a change to it takes effect everywhere
#: on the next run, before the sync itself has delivered it.
##
typeset -g daily_sync_config="${daily_sync_config:-${nightNotesPrivate}/configs/daily-sync/config.zsh}"
typeset -g daily_sync_runs="${daily_sync_runs:-${HOME}/.agents/runs/daily-sync}"
typeset -g daily_sync_job="${daily_sync_job:-daily-sync-metadata}"

#: Sent to every machine ahead of the generated commands. Repositories are named
#: relative to ~; NIGHTDIR is the vcsh repository rooted at ~ itself.
typeset -g h_daily_sync_prelude
h_daily_sync_prelude="$(command cat <<'DAILY_SYNC_PRELUDE'
emulate -R zsh
export PS4='> '
setopt LOCAL_OPTIONS PIPE_FAIL PRINT_EXIT_VALUE ERR_RETURN
setopt TYPESET_SILENT NO_CASE_GLOB multios re_match_pcre extendedglob pipefail interactivecomments hash_executables_only
setopt NO_BANG_HIST 2>/dev/null || true
#: stderr, with the trace when `daily_sync_trace=y' adds XTRACE, goes to the run's stderr log.
#: Under ERR_RETURN a failing plain command ends the function, so every
#: command that may fail sits in an `if' or ends in `|| true'.
zmodload zsh/stat zsh/datetime
builtin cd -- "${HOME}"
typeset -g ds_tmp=''
typeset -gi ds_n=0

function ds-repo {
    #: Sets the caller's G to the git command of <repo>. It is an array run with
    #: `command', not a function, so capturing its stderr does not capture a trace.
    if [[ "$1" == NIGHTDIR ]] ; then
        G=(vcsh night.sh)
    else
        G=(git -C "${HOME}/$1")
    fi
}

function ds-exists {
    [[ "$1" == NIGHTDIR || -e "${HOME}/$1/.git" ]]
}

function ds-inprog {
    local -a G
    ds-repo "$1"
    local d p
    for d in rebase-merge rebase-apply MERGE_HEAD ; do
        p="$(command "${G[@]}" rev-parse --path-format=absolute --git-path "${d}" 2>/dev/null)" || continue
        if [[ -e "${p}" ]] ; then
            return 0
        fi
    done
    return 1
}

function ds-git {
    #: ds-git <repo> <git arguments...>, with git's stderr on stdout.
    local -a G
    ds-repo "$1"
    shift
    command "${G[@]}" "$@" 2>&1
}

function ds-job {
    #: Runs up to six commands at once; ds-flush prints their output in order.
    if [[ -z "${ds_tmp}" ]] ; then
        ds_tmp="$(command mktemp -d)"
    fi
    ds_n+=1
    "$@" > "${ds_tmp}/${ds_n}" &
    if (( ds_n % 6 == 0 )) ; then
        wait || true
    fi
    return 0
}

function ds-flush {
    wait || true
    local i
    for (( i = 1 ; i <= ds_n ; i++ )) ; do
        command cat -- "${ds_tmp}/${i}" || true
    done
    if [[ -n "${ds_tmp}" && -d "${ds_tmp}" ]] ; then
        command rm -r -- "${ds_tmp}"
    fi
    ds_tmp=''
    ds_n=0
    return 0
}

function ds-status {
    #: ds-status <repo> <kind> <min-age-hours>: one status block, or `CLEAN <repo>'.
    local repo="$1" kind="$2" min_age="${3:-0}"
    if ! ds-exists "${repo}" ; then
        print -r -- "MISSING ${repo}"
        return 0
    fi
    local -a G counts uno entries out
    local -A new_in
    ds-repo "${repo}"
    local branch='' up='' note='' root="${HOME}/${repo}" e xy p tag
    local -i ahead=0 behind=0 n_tracked=0 skip=0 age=0
    if [[ "${repo}" == NIGHTDIR ]] ; then
        root="${HOME}"
        uno=(-uno)
    fi
    if [[ "${kind}" != mirror ]] ; then
        command "${G[@]}" fetch --quiet 2>/dev/null || note+=' FETCH-FAILED'
    fi
    branch="$(command "${G[@]}" rev-parse --abbrev-ref HEAD 2>/dev/null)" || true
    up="$(command "${G[@]}" rev-parse --abbrev-ref '@{upstream}' 2>/dev/null)" || true
    if [[ -n "${up}" ]] ; then
        counts=( $(command "${G[@]}" rev-list --left-right --count 'HEAD...@{upstream}' 2>/dev/null) ) || true
        ahead="${counts[1]:-0}"
        behind="${counts[2]:-0}"
    elif [[ "${kind}" != mirror ]] ; then
        note+=' NO-UPSTREAM'
    fi
    if ds-inprog "${repo}" ; then
        note+=' REBASE-OR-MERGE-IN-PROGRESS'
    fi
    entries=( "${(@0)$(command "${G[@]}" status --porcelain -z ${uno} 2>/dev/null)}" ) || true
    for e in "${entries[@]}" ; do
        if [[ -z "${e}" ]] ; then
            continue
        fi
        if (( skip )) ; then
            skip=0
            continue
        fi
        xy="${e[1,2]}" p="${e[4,-1]}" tag=''
        if [[ "${xy}" == [RC]* ]] ; then
            skip=1
        fi
        if [[ "${xy}" == '??' ]] ; then
            if [[ "${kind}" == *code ]] ; then
                tag='leave'
            fi
        else
            n_tracked+=1
            if [[ "${kind}" == *code ]] ; then
                tag='commit-ok'
                if [[ -e "${root}/${p}" ]] ; then
                    age=$(( (EPOCHSECONDS - $(zstat +mtime -- "${root}/${p}")) / 3600 ))
                    if (( age < min_age )) ; then
                        tag="too-new(${age}h)"
                    fi
                fi
            fi
        fi
        if [[ "${xy}" == '??' ]] ; then
            new_in[${p:h}]=$(( ${new_in[${p:h}]:-0} + 1 ))
        fi
        out+=( "  ${xy} ${tag:+${tag} }${p}" )
    done
    #: Many new files in one directory become a single line.
    for p in "${(@k)new_in}" ; do
        if (( new_in[${p}] >= 4 )) ; then
            out=( "${(@)out:#  \?\? (leave |)${(b)p}/[^/]##}" )
            out+=( "  ?? ${tag:+${tag} }${p}/ (${new_in[${p}]} new files in this directory)" )
        fi
    done
    if [[ ${#out} -eq 0 && ${ahead} -eq 0 && ${behind} -eq 0 && -z "${note}" ]] ; then
        print -r -- "CLEAN ${repo}"
        return 0
    fi
    print -r -- "${repo} [${kind}] ${branch} ahead=${ahead} behind=${behind}${note}"
    if [[ "${kind}" == *code && ${n_tracked} -gt 100 ]] ; then
        print -r -- "  OVER-100: ${n_tracked} tracked files changed; commit nothing in this repository"
    fi
    if (( ${#out} > 60 )) ; then
        out=( "${(@)out[1,60]}" "  ... $(( ${#out} - 60 )) more" )
    fi
    if (( ${#out} )) ; then
        print -rl -- "${out[@]}"
    fi
    return 0
}

function ds-sync {
    #: ds-sync <repo> <rw|ff> [<remote>[:<branch>]]...: pull, and in rw mode push,
    #: also to the extra remotes. Never leaves a rebase or merge half done: a
    #: conflict is aborted and reported.
    local repo="$1" mode="$2"
    shift 2
    if ! ds-exists "${repo}" ; then
        print -r -- "MISSING ${repo}"
        return 0
    fi
    local -a G nosign how
    ds-repo "${repo}"
    if ds-inprog "${repo}" ; then
        print -r -- "BLOCKED ${repo}: a rebase or merge is in progress"
        return 0
    fi
    if ! command "${G[@]}" rev-parse --abbrev-ref '@{upstream}' &>/dev/null ; then
        print -r -- "NO-UPSTREAM ${repo}: skipped"
        return 0
    fi
    local before='' after='' out='' files='' spec msg='' dirty=''
    local -i pulled=0 pushed=0 try=0 ok=0
    before="$(command "${G[@]}" rev-parse HEAD)" || true
    if [[ "${mode}" == ff ]] ; then
        if ! out="$(command "${G[@]}" pull --ff-only 2>&1)" ; then
            print -r -- "FF-REFUSED ${repo}: ${${(f)out}[-1]}"
            return 0
        fi
    else
        #: `pull --rebase' refuses a dirty tree; a merge leaves those files alone.
        how=(--rebase)
        dirty="$(command "${G[@]}" status --porcelain -uno 2>/dev/null)" || true
        if [[ -n "${dirty}" ]] ; then
            how=(--no-rebase --no-edit)
        fi
        for try in 1 2 ; do
            if out="$(command "${G[@]}" ${nosign} pull ${how} 2>&1)" ; then
                ok=1
                break
            fi
            files="$(command "${G[@]}" diff --name-only --diff-filter=U 2>/dev/null | command tr '\n' ' ')" || true
            if ds-inprog "${repo}" ; then
                command "${G[@]}" merge --abort &>/dev/null || command "${G[@]}" rebase --abort &>/dev/null || true
            fi
            if [[ ${try} -eq 1 && "${out}" == *'failed to sign'* ]] ; then
                nosign=(-c commit.gpgsign=false -c rebase.gpgSign=false)
                msg=' (unsigned: gpg failed)'
                continue
            fi
            break
        done
        if (( ! ok )) ; then
            if [[ -n "${files}" ]] ; then
                print -r -- "CONFLICT ${repo} (pull ${how[1]}, aborted): ${files}"
            else
                print -r -- "PULL-FAILED ${repo}: ${${(f)out}[-1]}"
            fi
            return 0
        fi
    fi
    after="$(command "${G[@]}" rev-parse HEAD)" || true
    if [[ "${before}" != "${after}" ]] ; then
        pulled="$(command "${G[@]}" rev-list --count "${before}..@{upstream}" 2>/dev/null)" || true
    fi
    if [[ "${mode}" == rw ]] ; then
        pushed="$(command "${G[@]}" rev-list --count '@{upstream}..HEAD' 2>/dev/null)" || true
        if (( pushed > 0 )) ; then
            if ! out="$(command "${G[@]}" push 2>&1)" ; then
                print -r -- "PUSH-FAILED ${repo}: ${${(f)out}[-1]}"
                return 0
            fi
        fi
        for spec in "$@" ; do
            if [[ "${spec}" == *:* ]] ; then
                how=( "${spec%%:*}" "${spec#*:}" )
            else
                how=( "${spec}" HEAD )
            fi
            if ! out="$(command "${G[@]}" push "${how[@]}" 2>&1)" ; then
                print -r -- "PUSH-FAILED ${repo} -> ${spec}: ${${(f)out}[-1]} (never force it)"
                return 0
            fi
            if [[ "${out}" != *'up-to-date'* ]] ; then
                msg+=" pushed-to=${spec}"
            fi
        done
    fi
    if [[ ${pulled} -gt 0 || ${pushed} -gt 0 || -n "${msg}" ]] ; then
        print -r -- "ok ${repo}: pulled=${pulled} pushed=${pushed}${msg}"
    else
        print -r -- "SAME ${repo}"
    fi
    return 0
}
DAILY_SYNC_PRELUDE
)"

function h-daily-sync-config {
    : "Load the private configuration; see docs/daily-sync.md for its variables."
    if [[ ! -r "${daily_sync_config}" ]] ; then
        ecerr "$0: no configuration at ${daily_sync_config}"
        return 1
    fi
    source "${daily_sync_config}" @RET
    typeset -g daily_sync_local="${daily_sync_local:-$(command hostname -s)}"
    typeset -gi daily_sync_min_age_hours="${daily_sync_min_age_hours:-7}"
    ensure-array daily_sync_peers daily_sync_notes daily_sync_private_code daily_sync_public \
        daily_sync_server_notes daily_sync_server_no_mirror daily_sync_extra_push
}

function h-daily-sync-run-dir {
    : "Print today's run directory, creating it."
    local dir="${daily_sync_run:-${daily_sync_runs}/$(command date +%Y-%m-%d)}"
    command mkdir -p -- "${dir}" @RET
    ec "${dir}"
}

function h-daily-sync-on {
    : "usage: h-daily-sync-on <machine> ; runs the zsh script on stdin there, after the prelude"
    local machine="$1" trace_p="${daily_sync_trace:-n}" run
    assert-args machine @RET
    run="$(h-daily-sync-run-dir)" @RET
    {
        ec "${h_daily_sync_prelude}"
        bool "${trace_p}" && ec 'setopt SOURCE_TRACE XTRACE'
        command cat
    } | if [[ "${machine}" == "${daily_sync_local}" ]] ; then
        command zsh -s 2>> "${run}/stderr.log"
    else
        command ssh -o ConnectTimeout=10 -o BatchMode=yes "${machine}" zsh -s 2>> "${run}/stderr.log"
    fi
}

function h-daily-sync-up-p {
    : "Whether <machine> answers; the answer is kept for the rest of this command."
    local machine="$1"
    [[ "${machine}" == "${daily_sync_local}" ]] && return 0
    if [[ -z "${h_daily_sync_up[${machine}]:-}" ]] ; then
        typeset -gA h_daily_sync_up
        if command ssh -o ConnectTimeout=10 -o BatchMode=yes "${machine}" true &>/dev/null ; then
            h_daily_sync_up[${machine}]=y
        else
            h_daily_sync_up[${machine}]=n
        fi
    fi
    [[ "${h_daily_sync_up[${machine}]}" == y ]]
}

function h-daily-sync-mirrors {
    : "Sets reply to the private repositories the server only receives by push."
    reply=( ${${(@)daily_sync_notes:|daily_sync_server_notes}:|daily_sync_server_no_mirror}
            ${(@)daily_sync_private_code:|daily_sync_server_no_mirror} )
}

function h-daily-sync-plan {
    : "usage: h-daily-sync-plan <machine> <st|rw|ff> ; prints the commands for that machine"
    local machine="$1" op="$2"
    local repo kind mode
    local -a repos extra
    if [[ "${machine}" == "${daily_sync_server:-}" ]] ; then
        h-daily-sync-mirrors
        repos=( ${daily_sync_server_notes} ${daily_sync_public} )
        [[ "${op}" == st ]] && repos+=( ${reply} )
    else
        repos=( ${daily_sync_notes} ${daily_sync_private_code} ${daily_sync_public} )
    fi
    for repo in ${repos} ; do
        if [[ "${op}" == st ]] ; then
            if (( ${daily_sync_notes[(Ie)${repo}]} )) ; then
                kind=notes
            elif (( ${daily_sync_public[(Ie)${repo}]} )) ; then
                kind=public-code
            else
                kind=private-code
            fi
            if [[ "${machine}" == "${daily_sync_server:-}" ]] ; then
                (( ${daily_sync_server_notes[(Ie)${repo}]} )) || kind=read-only
                (( ${reply[(Ie)${repo}]} )) && kind=mirror
            fi
            ec "ds-job ds-status ${(qq)repo} ${kind} ${daily_sync_min_age_hours}"
        else
            mode="${op}" extra=()
            #: On the server only its own notes are writable; the rest follows its remote.
            if [[ "${machine}" == "${daily_sync_server:-}" ]] && (( ! ${daily_sync_server_notes[(Ie)${repo}]} )) ; then
                mode=ff
            fi
            if [[ "${mode}" == rw ]] ; then
                #: `repo remote[:branch] [machine]': an extra push, from one machine only when named.
                local spec
                for spec in "${daily_sync_extra_push[@]}" ; do
                    local -a f=( ${=spec} )
                    [[ "${f[1]}" == "${repo}" ]] || continue
                    [[ -z "${f[3]:-}" || "${f[3]}" == "${machine}" ]] && extra+=( "${f[2]}" )
                done
            fi
            ec "ds-job ds-sync ${(qq)repo} ${mode} ${(j: :)${(qq)extra[@]}}"
        fi
    done
    ec ds-flush
}

function h-daily-sync-pass {
    : "usage: h-daily-sync-pass <machine> <st|rw|ff> ; one status or sync pass, summarised"
    local machine="$1" op="$2"
    if ! h-daily-sync-up-p "${machine}" ; then
        ec "## ${machine}: UNREACHABLE, skipped"
        return 0
    fi
    local out
    out="$(h-daily-sync-plan "${machine}" "${op}" | h-daily-sync-on "${machine}")" || out+=$'\n'"ERROR: the script on ${machine} stopped early; see $(h-daily-sync-run-dir)/stderr.log"
    local -a lines=( "${(@f)out}" ) quiet
    quiet=( ${${(M)lines:#(CLEAN|SAME) *}#* } )
    lines=( "${(@)lines:#(CLEAN|SAME) *}" )
    ec "## ${machine} ($( [[ "${op}" == st ]] && ec status || ec "sync ${op}" ))"
    (( ${#lines} )) && [[ -n "${lines[*]}" ]] && print -rl -- "${lines[@]}"
    (( ${#quiet} )) && ec "$( [[ "${op}" == st ]] && ec clean || ec unchanged ): ${(j:, :)quiet}"
    return 0
}

function h-daily-sync-status {
    local machine
    for machine in "${daily_sync_local}" ${daily_sync_peers} ${daily_sync_server:-} ; do
        h-daily-sync-pass "${machine}" st
    done
    local run
    run="$(h-daily-sync-run-dir)" @RET
    if [[ -s "${run}/held" ]] ; then
        ec "## held back by the privacy check"
        command cat -- "${run}/held"
    fi
}

function h-daily-sync-heartbeat {
    : "usage: h-daily-sync-heartbeat <name> <cron> <ttl> <prompt> ; arms it once per run"
    local name="$1" cron="$2" ttl="$3" prompt="$4"
    local run id
    run="$(h-daily-sync-run-dir)" @RET
    [[ -n "${PASEO_AGENT_ID:-}" ]] || { ec "(not inside Paseo: no ${name} heartbeat)" ; return 0 }
    [[ -s "${run}/${name}-id" ]] && return 0
    id="$(command paseo heartbeat create --cron "${cron}" --expires-in "${ttl}" --name "daily-sync-${name}" --json "${prompt}" \
        | command jq --raw-output '.id // .heartbeat.id // empty')"
    if [[ -z "${id}" ]] ; then
        ec "WARNING: could not arm the ${name} heartbeat; carry on without it"
        return 0
    fi
    ec "${id}" > "${run}/${name}-id"
}

function h-daily-sync-heartbeat-drop {
    local name="$1" run
    run="$(h-daily-sync-run-dir)" @RET
    [[ -s "${run}/${name}-id" ]] || return 0
    command paseo heartbeat delete "$(<"${run}/${name}-id")" &>/dev/null || true
    command rm -f -- "${run}/${name}-id"
}

function h-daily-sync-stage {
    : "usage: h-daily-sync-stage [<new-stage>] ; print or set the run's stage"
    local run
    run="$(h-daily-sync-run-dir)" @RET
    if (( $# )) ; then
        ec "$1" > "${run}/stage"
    else
        [[ -r "${run}/stage" ]] && ec "$(<"${run}/stage")"
    fi
    return 0
}

function h-daily-sync-begin {
    local machine
    h-daily-sync-heartbeat watchdog '*/10 * * * *' 3h "Daily sync watchdog. If you are in the middle of the sync right now, ignore this message and carry on. Otherwise the sync is unfinished: run \`zsh -c 'daily-sync next'\` with elevated access and do what it prints."
    ec "## schedule export"
    for machine in "${daily_sync_local}" ${daily_sync_peers} ; do
        h-daily-sync-up-p "${machine}" || continue
        ec "( unsetopt xtrace err_return print_exit_value ; paseo-schedules-export 2>&1 ) | command tail -n 1" | h-daily-sync-on "${machine}" \
            | command sed -e "s|^|${machine}: |"
    done
    [[ -n "$(h-daily-sync-stage)" ]] || h-daily-sync-stage begun
    h-daily-sync-status
    h-daily-sync-next-hint
}

function h-daily-sync-git {
    : "usage: daily-sync git <machine> <repo> <git arguments...>"
    local machine="$1" repo="$2"
    assert-args machine repo @RET
    shift 2
    local verb='' arg
    local -i i=1
    #: The verb is the first word after git's own options (`-c name=value', ...).
    while (( i <= $# )) ; do
        if [[ "${@[i]}" == -c ]] ; then
            i+=2
        elif [[ "${@[i]}" == -* ]] ; then
            i+=1
        else
            verb="${@[i]}"
            break
        fi
    done
    case "${verb}" in
        reset|clean|stash|restore|checkout|switch|config|commit|revert|cherry-pick|filter-branch|gc|rm|mv|branch|tag|remote)
            ecerr "daily-sync git: '${verb}' is not allowed in this job (commit with commit-own)"
            return 64 ;;
    esac
    for arg in "$@" ; do
        [[ "${arg}" == -- ]] && break
        case "${arg}" in
            --force|--force-with-lease*|--amend|--rewrite-pushed|--mirror|--hard)
                ecerr "daily-sync git: '${arg}' is not allowed in this job"
                return 64 ;;
        esac
    done
    if [[ "${verb}" == push ]] && (( ${@[(I)-f]} )) ; then
        ecerr "daily-sync git: forced pushes are not allowed in this job"
        return 64
    fi
    {
        ec "ds-exists ${(qq)repo} || { print -r -- 'no such repository here: '${(qq)repo} ; exit 66 }"
        if [[ "${verb}" == add ]] ; then
            ec "ds-inprog ${(qq)repo} || { print -r -- 'add is only for finishing a conflicted rebase or merge; commit with commit-own' ; exit 64 }"
        fi
        ec "ds-git ${(qq)repo} ${(j: :)${(qq)@}}"
    } | h-daily-sync-on "${machine}"
}

function h-daily-sync-converge {
    local machine run out
    run="$(h-daily-sync-run-dir)" @RET
    out="$(
        h-daily-sync-pass "${daily_sync_local}" rw
        for machine in ${daily_sync_peers} ; do
            h-daily-sync-up-p "${machine}" || { ec "## ${machine}: UNREACHABLE, skipped" ; continue }
            h-daily-sync-clone-missing "${machine}"
            h-daily-sync-pass "${machine}" rw
        done
        [[ -n "${daily_sync_server:-}" ]] && h-daily-sync-pass "${daily_sync_server}" rw
        #: Second local pass: takes in what the others pushed, and repeats the extra pushes.
        h-daily-sync-pass "${daily_sync_local}" rw
        for machine in ${daily_sync_peers} ${daily_sync_server:-} ; do
            h-daily-sync-up-p "${machine}" && h-daily-sync-pass "${machine}" ff
        done
        h-daily-sync-push-mirrors
    )"
    ec "${out}"
    ec "${out}" > "${run}/converge.log"
    local -a blocked=( ${(M)${(@f)out}:#(CONFLICT|BLOCKED) *} )
    if (( ${#blocked} )) ; then
        ec
        ec "UNRESOLVED: ${#blocked} repository line(s) above say CONFLICT or BLOCKED."
        if [[ "$(h-daily-sync-stage)" == (metadata-done|finished) || -e "${run}/conflict-seen" ]] ; then
            h-daily-sync-notify conflict "Notes sync needs you: a merge conflict is blocking at least one repository."
        fi
        : > "${run}/conflict-seen"
    fi
    ec
    out="$(h-daily-sync-status)"
    ec "${out}"
    ec "${out}" > "${run}/status.log"
}

function h-daily-sync-clone-missing {
    : "Clone onto a peer the repositories this machine has and the peer lacks."
    local machine="$1" repo url
    local -a missing
    missing=( ${${(M)${(@f)"$(
        for repo in ${daily_sync_notes} ${daily_sync_private_code} ${daily_sync_public} ; do
            ec "ds-exists ${(qq)repo} || print -r -- MISSING ${(qq)repo}"
        done | h-daily-sync-on "${machine}")"}:#MISSING *}#MISSING } )
    for repo in ${missing} ; do
        url="$(command git -C "${HOME}/${repo}" remote get-url origin 2>/dev/null)" || continue
        ec "command git clone ${(qq)url} \"\${HOME}/\"${(qq)repo} 2>&1 | command tail -n 1" | h-daily-sync-on "${machine}" \
            | command sed -e "s|^|CLONED ${repo} on ${machine}: |"
    done
}

function h-daily-sync-push-mirrors {
    : "Push the private mirrors to the server, creating one that is missing."
    local server="${daily_sync_server:-}" repo branch out
    [[ -n "${server}" ]] || return 0
    h-daily-sync-mirrors
    (( ${#reply} )) || return 0
    ec "## mirrors -> ${server}"
    if ! h-daily-sync-up-p "${server}" ; then
        ec "UNREACHABLE, skipped"
        return 0
    fi
    local -a same
    for repo in ${reply} ; do
        [[ -e "${HOME}/${repo}/.git" ]] || { ec "MISSING ${repo} here" ; continue }
        branch="$(command git -C "${HOME}/${repo}" rev-parse --abbrev-ref HEAD)"
        ec "ds-exists ${(qq)repo} || { command git init --quiet --initial-branch=${(qq)branch} \"\${HOME}/\"${(qq)repo} && ds-git ${(qq)repo} config receive.denyCurrentBranch updateInstead && print -r -- CREATED-MIRROR ${(qq)repo} }" \
            | h-daily-sync-on "${server}"
        if out="$(command git -C "${HOME}/${repo}" push "${server}:${repo}" "${branch}" 2>&1)" ; then
            if [[ "${out}" == *'up-to-date'* ]] ; then same+=( "${repo}" ) ; else ec "ok ${repo}: pushed" ; fi
        else
            ec "MIRROR-REFUSED ${repo}: ${${(f)out}[-1]} (someone edited it on ${server}; leave it)"
        fi
    done
    (( ${#same} )) && ec "unchanged: ${(j:, :)same}"
    return 0
}

function h-daily-sync-notify {
    : "usage: h-daily-sync-notify <key> <message> ; one notification per key per run"
    local key="$1" message="$2" run
    run="$(h-daily-sync-run-dir)" @RET
    [[ -e "${run}/notified-${key}" ]] && return 0
    notif-tlg-heart "${message} Details: Paseo agent ${${PASEO_AGENT_ID:-unknown}[1,8]}." &>/dev/null \
        && : > "${run}/notified-${key}"
    ec "(notification sent: ${key})"
}

function h-daily-sync-hold {
    : "usage: daily-sync hold <machine> <repo> <file> <what was found>"
    local machine="$1" repo="$2" file="$3" run
    assert-args machine repo file @RET
    run="$(h-daily-sync-run-dir)" @RET
    ec "${machine} ${repo}: ${file} (${*[4,-1]})" >> "${run}/held"
    #: The notification names no file: even a file name can be private.
    h-daily-sync-notify "held-${machine}-${repo:t}" "Daily sync held back a commit in ${repo:t} on ${machine}: possible private information."
}
##
function h-daily-sync-metadata-log {
    ec "${nightNotes}/.logs/org-paper-metadata-updater/$(command date +%Y-%m-%d).log"
}

function h-daily-sync-updater {
    : "usage: h-daily-sync-updater <rc-file> <directory> ; one logged updater run"
    local rc_file="$1" dir="$2"
    org-paper-metadata-updater --write --scraper page-data --bibtex "${dir}"
    ec "$?" > "${rc_file}"
}

function h-daily-sync-metadata-job {
    : "The background job: the updater over each repository in turn, then a wake-up."
    local run="$1" ; shift
    local repo log
    log="$(h-daily-sync-metadata-log)"
    for repo in "$@" ; do
        log-to "${log}" h-daily-sync-updater "${run}/rc-${repo:t}" "${HOME}/${repo}"
    done
    : > "${run}/DONE"
    builtin cd -- "${HOME}"
    [[ -z "${PASEO_AGENT_ID:-}" ]] || command paseo send --no-wait "${PASEO_AGENT_ID}" \
        "The metadata updater finished. Run \`zsh -c 'daily-sync next'\` with elevated access and do what it prints."
}

function h-daily-sync-metadata-start {
    local run repo
    local -a repos skipped
    run="$(h-daily-sync-run-dir)" @RET
    for repo in ${daily_sync_metadata:-${daily_sync_notes}} ; do
        if [[ -z "$(command git -C "${HOME}/${repo}" status --porcelain 2>&1)" ]] \
            && ! command grep --quiet --extended-regexp "^(CONFLICT|BLOCKED|PULL-FAILED) ${repo}[ :]" "${run}/converge.log" 2>/dev/null ; then
            repos+=( "${repo}" )
        else
            skipped+=( "${repo}" )
        fi
    done
    (( ${#skipped} )) && ec "not clean or blocked, so no metadata update: ${(j:, :)skipped}"
    if (( ${#repos} == 0 )) ; then
        ec "No repository is eligible for the metadata update."
        h-daily-sync-stage metadata-done
        : > "${run}/DONE"
        h-daily-sync-next-hint
        return 0
    fi
    command rm -f -- "${run}/DONE" "${run}"/rc-*(N)
    tmux-job-start "${daily_sync_job}" PASEO_AGENT_ID="${PASEO_AGENT_ID:-}" daily_sync_config="${daily_sync_config}" \
        h-daily-sync-metadata-job "${run}" "${repos[@]}" @RET
    h-daily-sync-heartbeat-drop watchdog
    h-daily-sync-heartbeat metadata '*/30 * * * *' 8h "Daily sync: metadata updater wait. Run \`zsh -c 'daily-sync next'\` with elevated access and do what it prints."
    h-daily-sync-stage metadata
    ec "Metadata updater started for: ${(j:, :)repos}"
    h-daily-sync-next-hint
}

function h-daily-sync-metadata-status {
    setopt localoptions bareglobqual
    local run repo rc log
    run="$(h-daily-sync-run-dir)" @RET
    if [[ ! -e "${run}/DONE" ]] ; then
        tmux-job-status "${daily_sync_job}"
        case $? in
            0) ec "RUNNING. It can take hours. End your turn now, without doing anything else; you will be woken." ; return 0 ;;
            2) ec "UNKNOWN: the tmux server is not reachable from the sandbox. Run this command again with elevated access." ; return 0 ;;
        esac
        ec "The job stopped before finishing; repositories without an rc below were not run."
    fi
    h-daily-sync-heartbeat-drop metadata
    h-daily-sync-heartbeat watchdog '*/10 * * * *' 2h "Daily sync watchdog. If you are in the middle of the sync right now, ignore this message and carry on. Otherwise the sync is unfinished: run \`zsh -c 'daily-sync next'\` with elevated access and do what it prints."
    h-daily-sync-stage metadata-done
    log="$(h-daily-sync-metadata-log)"
    ec "## updater exit codes (nonzero only means some papers failed; 75 means not run, another updater held the lock)"
    for repo in ${daily_sync_metadata:-${daily_sync_notes}} ; do
        rc='not run' ; [[ -r "${run}/rc-${repo:t}" ]] && rc="$(<"${run}/rc-${repo:t}")"
        ec "${repo}: rc=${rc}"
    done
    ec "## counts (log: ${log})"
    command perl -ne 's/\e\[[0-9;]*m//g; print if /^files=\d+ /' "${log}" 2>/dev/null | command tail -n "${#${(@)daily_sync_metadata:-${daily_sync_notes}}}"
    ec "## failures today, by cause (prior = earlier daily logs with the same non-transient failure)"
    h-daily-sync-metadata-failures "${log}" "${log:h}"/*.log(N)
    h-daily-sync-metadata-review
    h-daily-sync-next-hint
}

function h-daily-sync-metadata-failures {
    : "usage: h-daily-sync-metadata-failures <today's log> <all logs...>"
    command perl - "$@" <<'EOF'
use strict; use warnings;
my ($today, @logs) = @ARGV;
sub parse {
    my ($f) = @_; my %h;
    open(my $fh, '<', $f) or return {};
    while (my $l = <$fh>) {
        $l =~ s/\e\[[0-9;]*m//g; chomp $l;
        next unless $l =~ m{^(/\S.*?\.org):(\d+): (.*)$};
        my ($file, $line, $rest) = ($1, $2, $3);
        my $url = '';
        if ($rest =~ m{^(https?://\S+): (.*)$}) { ($url, $rest) = ($1, $2); }
        my $class = $rest =~ /HTTP (429|5\d\d)|no such host|timed? ?out|deadline exceeded|connection (reset|refused)|EOF/i ? 'TRANSIENT'
                  : $rest =~ /HTTP 404|did not redirect/i ? 'STALE-ID'
                  : $rest =~ /missing (abstract|authors)/i ? 'INCOMPLETE'
                  : 'OTHER';
        $h{$url ne '' ? $url : "$file: $rest"} = [$class, "$file:$line", $url, $rest];
    }
    return \%h;
}
my $now = parse($today);
my %prior;
for my $log (@logs) {
    next if $log eq $today;
    my $old = parse($log);
    for my $k (keys %$old) { $prior{$k}++ if $old->{$k}[0] ne 'TRANSIENT'; }
}
my %n;
for my $k (sort { $now->{$a}[0] cmp $now->{$b}[0] || $a cmp $b } keys %$now) {
    my ($class, $where, $url, $rest) = @{ $now->{$k} };
    $n{$class}++;
    next if $class eq 'TRANSIENT';
    my $p = $prior{$k} // 0;
    printf "%s prior=%d%s %s %s: %s\n", $class, $p, ($p >= 3 ? ' TAG-CANDIDATE' : ''), $where, $url, $rest;
}
print "TRANSIENT: $n{TRANSIENT} (left for the next run)\n" if $n{TRANSIENT};
print "none\n" unless %$now;
EOF
}

function h-daily-sync-metadata-review {
    : "List what the updater changed, fixing the whitespace it leaves behind."
    local repo file root
    local -a changed other
    ec "## files changed in the notes repositories since the last commit"
    for repo in ${daily_sync_metadata:-${daily_sync_notes}} ; do
        root="${HOME}/${repo}"
        changed=( "${(@0)$(command git -C "${root}" ls-files --modified -z 2>/dev/null)}" )
        changed=( "${(@)changed:#}" )
        (( ${#changed} )) || continue
        #: Trailing whitespace and a blank line at the end sit on lines the updater wrote.
        command git -C "${root}" diff --check 2>/dev/null | command perl -e '
            my %ws; my %eof;
            while (<STDIN>) {
                if (/^(.+\.org):(\d+): trailing whitespace/) { $ws{$1}{$2} = 1 }
                elsif (/^(.+\.org):\d+: new blank line at EOF/) { $eof{$1} = 1 }
            }
            for my $f (keys %{{ %ws, %eof }}) {
                open(my $in, "<", "$ARGV[0]/$f") or next; my @l = <$in>; close $in;
                for my $n (keys %{ $ws{$f} // {} }) { $l[$n - 1] =~ s/[ \t]+(\r?\n?)$/$1/ if $n <= @l }
                if ($eof{$f}) { pop @l while @l && $l[-1] =~ /^\s*$/ }
                open(my $out, ">", "$ARGV[0]/$f") or next; print $out @l; close $out;
                print "  whitespace fixed: $f\n";
            }' "${root}"
        other=( "${(@)changed:#*.org}" )
        ec "${repo}: ${#changed} file(s)$( (( ${#other} )) && ec ", NOT .org (hold these back): ${(j:, :)other}" )"
        command git -C "${root}" diff --stat 2>/dev/null | command tail -n 25
        command git -C "${root}" diff --check 2>/dev/null | command head -n 5
    done
    return 0
}

function h-daily-sync-metadata-rerun {
    : "usage: daily-sync metadata rerun <directory>... ; after repointing links"
    local dir run log
    run="$(h-daily-sync-run-dir)" @RET
    log="$(h-daily-sync-metadata-log)"
    for dir in "$@" ; do
        log-to "${log}" h-daily-sync-updater "${run}/rc-rerun" "${dir}"
        ec "${dir}: rc=$(<"${run}/rc-rerun")"
    done
    command perl -ne 's/\e\[[0-9;]*m//g; print if /^files=\d+ /' "${log}" | command tail -n "$#"
}

function h-daily-sync-metadata-commit {
    : "usage: daily-sync metadata commit <repo> [<file to hold back>...]"
    local repo="$1" ; shift
    assert-args repo @RET
    local root="${HOME}/${repo}"
    local -a files
    files=( "${(@0)$(command git -C "${root}" ls-files --modified -z -- '*.org')}" )
    files=( "${(@)files:#}" )
    files=( "${(@)files:|argv}" )
    (( ${#files} )) || { ec "${repo}: nothing to commit" ; return 0 }
    command git -C "${root}" commit-own -m "org-paper-metadata-updater: updated at $(command date +%Y-%m-%d)" -- "${files[@]}"
}
##
function h-daily-sync-leftover {
    : "Committable changes in the status that ended the last converge, minus held files."
    local run
    run="$(h-daily-sync-run-dir)" @RET
    [[ -r "${run}/status.log" ]] || return 0
    command perl -e '
        my ($log, $held) = @ARGV; my %held;
        if (open(my $h, "<", $held)) { while (<$h>) { $held{"$1 $2 $3"} = 1 if /^(\S+) (\S+): (.*) \(.*\)$/ } }
        open(my $f, "<", $log) or exit 0; my @l = <$f>; my $start = 0;
        my ($machine, $repo, $kind) = ("", "", "");
        for my $line (@l[$start .. $#l]) {
            chomp $line;
            if ($line =~ /^## (\S+) \(status\)/) { $machine = $1; $repo = ""; next }
            if ($line =~ /^(\S.*?) \[([a-z-]+)\] /) { ($repo, $kind) = ($1, $2); next }
            next unless $repo ne "" && $line =~ /^  (..) (.*)$/;
            my ($xy, $rest) = ($1, $2);
            next if $xy eq ".." || $rest =~ /^(OVER-100|\.\.\. )/ || $line =~ /^  OVER-100/;
            my $ok = $kind eq "notes" || ($kind =~ /code$/ && $rest =~ s/^commit-ok //);
            next unless $ok;
            (my $file = $rest) =~ s{/ \(\d+ new files in this directory\)$}{/};
            next if $held{"$machine $repo $file"};
            print "  $machine $repo: $xy $file\n";
        }' "${run}/status.log" "${run}/held"
}

function h-daily-sync-next-hint {
    case "$(h-daily-sync-stage)" in
        begun)
            ec
            ec "NEXT: commit what the status above lists (rules are in your instructions), using"
            ec "  zsh -c 'daily-sync git <machine> <repo> diff -- <file>...'"
            ec "  zsh -c 'daily-sync git <machine> <repo> commit-own -m \"<message>\" -- <file>...'"
            ec "Then, even when there was nothing to commit, run: zsh -c 'daily-sync converge'" ;;
        converged)
            local -a left
            left=( "${(@f)$(h-daily-sync-leftover)}" )
            left=( "${(@)left:#}" )
            ec
            if (( ${#left} )) ; then
                ec "STILL TO COMMIT (${#left}): these are committable and were left out:"
                print -rl -- "${(@)left[1,25]}"
                ec
                ec "NEXT: commit them now (diff first; privacy check in [public-code]; a file you must not"
                ec "publish goes to 'daily-sync hold'), then run again: zsh -c 'daily-sync converge'"
                ec "Only when a file truly cannot be committed, go on and name it in the report:"
                ec "  zsh -c 'daily-sync metadata start'"
            else
                ec "NEXT: if a line above says CONFLICT, resolve it or leave it blocked (rules are in your"
                ec "instructions), and run converge again after resolving. Then run: zsh -c 'daily-sync metadata start'"
            fi ;;
        metadata)
            ec
            ec "NEXT: write the Phase 1 report as your message and end your turn. You will be woken"
            ec "when the updater finishes; then run: zsh -c 'daily-sync next'" ;;
        metadata-done)
            ec
            ec "NEXT: repair failing papers and review the changes (rules are in your instructions), commit"
            ec "each repository listed above with: zsh -c 'daily-sync metadata commit <repo>'"
            ec "Then, even when nothing changed, run: zsh -c 'daily-sync finish'" ;;
        finished)
            ec
            ec "ALL DONE. Write the final report as your message and end your turn." ;;
        *)
            ec "NEXT: run: zsh -c 'daily-sync begin'" ;;
    esac
}

function h-daily-sync-next {
    #: A slow step that is still running, or whose result was not shown yet, comes first.
    h-daily-sync-step '' && return 0
    case "$(h-daily-sync-stage)" in
        '') h-daily-sync-step begin ;;
        begun) h-daily-sync-step status ;;
        metadata|metadata-done) h-daily-sync-metadata-status ;;
        *) h-daily-sync-next-hint ;;
    esac
}

function h-daily-sync-step-run {
    : "The background side of h-daily-sync-step."
    local step="$1" run
    run="$(h-daily-sync-run-dir)" @RET
    daily-sync "${step}" > "${run}/step.out.tmp" 2>&1
    command mv -- "${run}/step.out.tmp" "${run}/step.out"
    : > "${run}/step.done"
}

function h-daily-sync-step {
    : "usage: h-daily-sync-step <step|''> ; start the slow step in the background, or pick up the pending one, and wait briefly for its output"
    #: The agent harness cuts a command off after about 30 seconds, and a model
    #: handles that badly. So a slow step runs as a tmux job, and each call
    #: waits at most `daily_sync_step_wait' seconds before saying to call again.
    local step="$1" max_wait="${daily_sync_step_wait:-20}" job="${daily_sync_step_job:-daily-sync-step}"
    local run rc
    local -i waited=0
    run="$(h-daily-sync-run-dir)" @RET
    tmux-job-running-p "${job}"
    rc=$?
    if (( rc == 2 )) ; then
        ec "The tmux server cannot be reached from the sandbox. Run the same command again with elevated access."
        return 0
    fi
    if (( rc != 0 )) ; then
        if [[ -e "${run}/step.done" && ! -e "${run}/step.shown" && ( -z "${step}" || "$(<"${run}/step.name")" == "${step}" ) ]] ; then
            : #: finished since the last call; shown below
        elif [[ -n "${step}" ]] ; then
            command rm -f -- "${run}/step.done" "${run}/step.shown"
            ec "${step}" > "${run}/step.name"
            tmux-job-start "${job}" PASEO_AGENT_ID="${PASEO_AGENT_ID:-}" daily_sync_inner=y \
                daily_sync_config="${daily_sync_config}" daily_sync_run="${run}" \
                h-daily-sync-step-run "${step}" &>/dev/null @RET
        else
            return 1
        fi
    fi
    while [[ ! -e "${run}/step.done" ]] && (( waited < max_wait )) ; do
        command sleep 1
        waited+=1
    done
    if [[ -e "${run}/step.done" ]] ; then
        command cat -- "${run}/step.out"
        : > "${run}/step.shown"
    else
        ec "STILL WORKING on '$(<"${run}/step.name")'; this is normal, it takes a few minutes."
        ec "NEXT: run now, and again each time it says STILL WORKING: zsh -c 'daily-sync next'"
    fi
    return 0
}

function daily-sync {
    : "usage: daily-sync begin|next|status|converge|finish
       daily-sync git <machine> <repo> <git arguments...>
       daily-sync hold <machine> <repo> <file> <what was found>
       daily-sync metadata start|status|rerun <directory>...|commit <repo> [<held-back file>...]
The mechanical half of the daily multi-machine sync; see docs/daily-sync.md."
    local cmd="${1:-}" inner_p="${daily_sync_inner:-n}"
    (( $# )) && shift
    h-daily-sync-config @RET
    ensure-cmd jq perl @RET
    if [[ "${cmd}" == (begin|status|converge|finish) ]] && ! bool "${inner_p}" ; then
        h-daily-sync-step "${cmd}"
        return $?
    fi
    case "${cmd}" in
        begin) h-daily-sync-begin ;;
        next) h-daily-sync-next ;;
        status) h-daily-sync-status ; h-daily-sync-next-hint ;;
        git) h-daily-sync-git "$@" ;;
        hold) h-daily-sync-hold "$@" ;;
        converge)
            h-daily-sync-converge @RET
            [[ "$(h-daily-sync-stage)" == begun ]] && h-daily-sync-stage converged
            h-daily-sync-next-hint ;;
        metadata)
            local sub="${1:-}"
            (( $# )) && shift
            case "${sub}" in
                start) h-daily-sync-metadata-start ;;
                status) h-daily-sync-metadata-status ;;
                rerun) h-daily-sync-metadata-rerun "$@" ;;
                commit) h-daily-sync-metadata-commit "$@" ;;
                *) ecerr "daily-sync metadata: start, status, rerun or commit" ; return 64 ;;
            esac ;;
        finish)
            h-daily-sync-converge @RET
            h-daily-sync-heartbeat-drop watchdog
            h-daily-sync-heartbeat-drop metadata
            h-daily-sync-stage finished
            h-daily-sync-next-hint ;;
        *)
            ecerr "usage: daily-sync begin|next|status|git|hold|converge|metadata|finish"
            return 64 ;;
    esac
}
