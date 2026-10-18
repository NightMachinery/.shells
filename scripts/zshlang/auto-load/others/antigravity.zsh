##
#: Account definitions belong in the private shell/environment, never here.
#: AGY_ACCOUNTS='m cave fm2' enables generated launchers. See docs/agy_status.md.
function h-agy-accounts {
    local accounts="${AGY_ACCOUNTS:-m cave fm2}" id
    local -a ids=( ${=accounts} )
    local -A seen=() roots=() names=()
    for id in "${ids[@]}" ; do
        if [[ "${id}" != [a-z][a-z0-9_]# || "${id}" == (best|status|account|accounts|t) ]] ; then
            ecerr "$0: invalid or reserved account name: ${id}"
            return 2
        fi
        test -z "${seen[$id]}" || { ecerr "$0: duplicate account: ${id}"; return 2; }
        seen[$id]=y
        local name_var="AGY_ACCOUNT_NAME_${(U)id}" name=""
        name="${(P)name_var}"
        if test -n "${name}" ; then
            if [[ "${name}" != [a-z][a-z0-9_]# || "${name}" == acc<-> ||
                  "${name}" == (best|status|account|accounts|t) || -n "${names[$name]}" ]] ||
                (( ${ids[(Ie)$name]} )) ; then
                ecerr "$0: duplicate, reserved or invalid account name: ${name}"
                return 2
            fi
            names[$name]=y
        fi
        local -a reply
        h-agy-account-info "${id}" unchecked @RET
        test -z "${roots[${reply[3]}]}" || {
            ecerr "$0: accounts must have different state directories"
            return 2
        }
        roots[${reply[3]}]=y
    done
    print -rl -- "${ids[@]}"
}

function h-agy-account-info {
    #: Returns id, label, Gemini root, CLI root, cache in reply.
    local id="${1:-${AGY_ACCOUNT_ID:-m}}" check="${2:-check}"
    if [[ "${id}" == <-> ]] ; then
        local -a numbered=( ${=AGY_ACCOUNTS:-m cave fm2} )
        id="${numbered[$id]}"
    fi
    if [[ "${check}" == check ]] ; then
        local ids
        ids="$(h-agy-accounts)" @RET
        local -a known=( "${(@f)ids}" )
        local candidate name_var
        for candidate in "${known[@]}" ; do
            name_var="AGY_ACCOUNT_NAME_${(U)candidate}"
            [[ -z "${(P)name_var}" || "${(P)name_var}" != "${id}" ]] || { id="${candidate}"; break; }
        done
        (( ${known[(Ie)$id]} )) || { ecerr "$0: unknown account: ${id}"; return 2; }
    fi
    local dir_var="AGY_ACCOUNT_DIR_${(U)id}" label_var="AGY_ACCOUNT_LABEL_${(U)id}"
    local name_var="AGY_ACCOUNT_NAME_${(U)id}"
    local root="${(P)dir_var}"
    if test -z "${root}" ; then
        if [[ "${id}" == (m|acc1) ]] ; then
            root="${HOME}/.gemini"
        else
            root="${AGY_ACCOUNTS_DIR:-${HOME}/.agy-accounts}/${id}"
        fi
    fi
    [[ "${root}" == /* ]] || { ecerr "$0: account directory must be absolute"; return 2; }
    root="${root:A}"
    local cache="${root}/antigravity-cli/quota-cache.json"
    if [[ "${id}" == (m|acc1) ]] ; then
        cache="${AGY_MAIN_STATUS_CACHE_FILE:-${HOME}/tmp/.agy-status/quota-cache.json}"
        #: Another account exports its cache to its children. That ambient
        #: value must never become m's cache when reporting all accounts.
        if [[ "${AGY_ACCOUNT_ID:-m}" == (m|acc1) ]] ; then
            cache="${agy_status_cache_file:-${cache}}"
        fi
    fi
    reply=( "${id}" "${(P)label_var:-${(P)name_var:-${id}}}" "${root}" "${root}/antigravity-cli" "${cache}" )
}

function agy-accounts-refresh {
    : "generates agy-<name> and agy-<name>-t from AGY_ACCOUNTS"
    local ids id
    ids="$(h-agy-accounts)" @RET
    for id in "${(@f)ids}" ; do
        aliasfn "agy-${id}" agy-account "${id}"
        aliasfn "agy-${id}-t" h-tmuxnewsh2-attach-z-with-cmd "agy-${id}"
        local name_var="AGY_ACCOUNT_NAME_${(U)id}" name=''
        name="${(P)name_var}"
        if test -n "${name}" ; then
            aliasfn "agy-${name}" agy-account "${id}"
            aliasfn "agy-${name}-t" h-tmuxnewsh2-attach-z-with-cmd "agy-${name}"
        fi
    done
}

function h-agy-account-of-transcript {
    local transcript="${1:A}" ids id
    local conversation="${1:h:h:h:t}"
    local owner_file="${AGY_ACCOUNTS_DIR:-${HOME}/.agy-accounts}/history-owners/${conversation}"
    if [[ "${conversation}" == [a-zA-Z0-9_-]## && -r "${owner_file}" ]] ; then
        local owner="$(<"${owner_file}")"
        local -a reply
        h-agy-account-info "${owner}" @RET
        ec "${reply[1]}"
        return 0
    fi
    ids="$(h-agy-accounts)" @RET
    local -a reply
    for id in "${(@f)ids}" ; do
        h-agy-account-info "${id}" @RET
        if [[ "${transcript}" == "${reply[4]}/brain/"* ]] ; then
            ec "${id}"
            return 0
        fi
    done
    return 1
}

function agy-account-prepare {
    : "usage: agy-account-prepare N; links shared rules/hooks and syncs statusLine"
    local -a reply
    h-agy-account-info "${1}" @RET
    ensure-cmd python3 @RET
    command python3 "${NIGHTDIR:-${HOME}/scripts}/python/agy_accounts.py" \
        --gemini-dir "${reply[3]}" --shared-dir "${AGY_SHARED_GEMINI_DIR:-${HOME}/.gemini}" \
        --owners-dir "${AGY_ACCOUNTS_DIR:-${HOME}/.agy-accounts}/history-owners"
}

function h-agy-account-argv {
    #: One process boundary for launch, login verification and print mode.
    local -a info
    h-agy-account-info "${1}" @RET
    info=( "${reply[@]}" )
    shift
    local arg
    for arg in "$@" ; do
        case "${arg}" in
            --gemini_dir|--gemini_dir=*|--app_data_dir|--app_data_dir=*)
                ecerr "$0: account state flags are managed by the launcher"
                return 2 ;;
        esac
    done
    local cmd=("${antigravity_cmd[@]:-agy}")
    ensure-cmd "${cmd[1]}" @RET
    #: These flags exist but are deliberately hidden from --help in agy 1.4.2.
    #: The env root is also needed by agy's plugin CLI children. SSH is its
    #: verified file-storage trigger, scoped only to this child process.
    reply=( env ANTIGRAVITY_GEMINI_DIR="${info[3]}"
        AGY_ACCOUNT_ID="${info[1]}" AGY_ACCOUNT_LABEL="${info[2]}"
        agy_status_cache_file="${info[5]}"
        SSH_CONNECTION="${SSH_CONNECTION:-agy-file-auth}"
        "${cmd[@]}" --gemini_dir="${info[3]}" --app_data_dir=antigravity-cli "$@" )
}

function h-agy-account-command {
    local -a reply
    h-agy-account-argv "$@" @RET
    command "${reply[@]}"
}

function agy-account {
    : "usage: agy-account N [agy arguments...]"
    local -a reply
    h-agy-account-info "${1}" @RET
    shift
    local id="${reply[1]}" label="${reply[2]}"
    #: Existing accounts are never rewritten implicitly. The explicit setup
    #: command reconciles shared links and refuses existing independent files.
    if ! test -d "${reply[4]}" ; then
        agy-account-prepare "${id}" @RET
    fi
    h-agy-account-argv "${id}" "$@" @RET
    agent_launch_glyph="🪐/${label} " agent_launch_echo_p=y \
        h-agent-launch agy "${reply[@]}"
}

function antigravity {
    agy-account "${AGY_ACCOUNT_ID:-m}" "$@"
}
aliasfn antigravity-m antigravity

function agy {
    #: Bare `agy' in our shell gets the same file-auth guarantee.
    antigravity "$@"
}

function agy-best {
    : "launches the account with most quota for --model (or agy_best_model)"
    local model="${agy_best_model:-}" family=gemini conversation='' arg i=1 model_given=n
    local -a args=( "$@" )
    while (( i <= ${#args} )) ; do
        arg="${args[$i]}"
        case "${arg}" in
            --model) (( i++ )); model="${args[$i]}"; model_given=y ;;
            --model=*) model="${arg#*=}"; model_given=y ;;
            --conversation) (( i++ )); conversation="${args[$i]}" ;;
            --conversation=*) conversation="${arg#*=}" ;;
            --continue|-c)
                ecerr "$0: use an account launcher for --continue, so history ownership is explicit"
                return 2 ;;
        esac
        (( i++ ))
    done
    if test -z "${model}" && test -z "${conversation}" ; then
        ecerr "$0: --model (or agy_best_model) is required so selection matches the model launched"
        return 2
    fi
    if test -n "${model}" && [[ "${model_given}" == n ]] ; then
        args+=( --model "${model}" )
    fi
    case "${model:l}" in
        *claude*|*gpt*|*openai*|*anthropic*) family=3p ;;
        *gemini*) family=gemini ;;
        '') ;;
        *) ecerr "$0: cannot select a quota family for model: ${model}"; return 2 ;;
    esac
    local account json ret=0
    if test -n "${conversation}" ; then
        local transcript
        transcript="$(h-agy-session-transcript-of "${conversation}")" @RET
        account="$(h-agy-account-of-transcript "${transcript}")" @RET
        ecerr "$0: resuming with owning account ${account}"
    else
        json="$(agy-status --json)" || ret=$?
        account="$(ec "${json}" | command jq --exit-status --raw-output --arg family "${family}" '
            [.[] | select(.error == null)
             | . + {available: ([.buckets[] | select(.bucket_id | startswith($family + "-")) | .remaining_percent] | min)}
             | select(.available != null and .available > 0)]
            | sort_by(-.available) | .[0].account // empty')" || {
            ecerr "$0: no account has known remaining ${family} quota (status exit ${ret})"
            return 1
        }
        ecerr "$0: selected ${account} for ${family} (minimum of known 5h/weekly remaining quota)"
    fi
    agy-account "${account}" "${args[@]}"
}

function agy-acc-login {
    : "usage: agy-acc-login N; starts owner-operated sign-in in detached tmux"
    local -a reply
    h-agy-account-info "${1}" @RET
    local id="${reply[1]}" session="agy-login-${reply[1]}"
    ensure-cmd tmux @RET
    #: Never use tmuxnewsh2 here: it replaces an existing named session.
    if command tmux has-session -t "=${session}" 2>/dev/null ; then
        ec "Sign-in already open. Attach: tmux attach-session -t ${(q)session}"
        return 0
    fi
    agy-account-prepare "${id}" @RET
    #: The root and label travel as arguments; a tmux server may have old env.
    local script
    script="$(gquote zsh -c 'agy-account-hook "$1" "$2" "$3" h-agy-account-login-run' \
        agy-login "${id}" "${reply[3]}" "${reply[2]}")" @RET
    command tmux new-session -d -s "${session}" "${script}" @RET
    ec "Attach: tmux attach-session -t ${(q)session}"
    ec "Complete sign-in in that pane; after you exit agy, models verifies the account."
}

function h-agy-account-login-run {
    local -a reply
    h-agy-account-info "${AGY_ACCOUNT_ID}" @RET
    if test -s "${reply[4]}/antigravity-oauth-token" ; then
        ec "File credentials already exist; verifying without signing out."
    else
        antigravity --mode plan
    fi
    local ret=0
    h-agy-account-command "${AGY_ACCOUNT_ID}" models || ret=$?
    ec "Account verification exited ${ret}. Press Enter to close this pane."
    read -r
    return "${ret}"
}

function agy-account-hook {
    #: Explicit transport across Brish, whose persistent env is not agy's env.
    local id="${1:-m}" root="${2:-${HOME}/.gemini}" label="${3:-m}"
    shift 3
    local -x AGY_ACCOUNT_ID="${id}" AGY_ACCOUNT_LABEL="${label}"
    local -x ANTIGRAVITY_GEMINI_DIR="${root}"
    local -x "AGY_ACCOUNT_DIR_${(U)id}=${root}"
    local -x "AGY_ACCOUNT_LABEL_${(U)id}=${label}"
    local -x AGY_ACCOUNTS="${AGY_ACCOUNTS:-m cave fm2} ${id}"
    #: Deduplicate before the normal registry validation.
    local -a ids=( ${=AGY_ACCOUNTS} )
    AGY_ACCOUNTS="${(j: :)${(u)ids}}"
    local agy_home="${root}/antigravity-cli"
    local agy_summaries_db="${agy_home}/conversation_summaries.db"
    (( $# && ${+functions[$1]} )) || return 0
    local -a reply
    h-agy-account-info "${id}" >/dev/null 2>&1 || return 0
    "$@" || return 0
}

agy-accounts-refresh
##
