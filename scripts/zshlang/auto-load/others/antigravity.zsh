##
#: Account definitions belong in the private shell/environment, never here.
#: AGY_ACCOUNTS='m cave fm2' enables generated launchers. See docs/agy_status.md.
function h-agy-accounts {
    local accounts="${AGY_ACCOUNTS:-m cave fm2}" id
    local -a ids=( ${=accounts} )
    local -A seen=() roots=() names=()
    for id in "${ids[@]}" ; do
        if [[ "${id}" != [a-z][a-z0-9_]# || "${id}" == (best|status|account|accounts|t|api|api_*) ]] ; then
            ecerr "$0: invalid or reserved account name: ${id}"
            return 2
        fi
        test -z "${seen[$id]}" || { ecerr "$0: duplicate account: ${id}"; return 2; }
        seen[$id]=y
        local name_var="AGY_ACCOUNT_NAME_${(U)id}" name=""
        name="${(P)name_var}"
        if test -n "${name}" ; then
            if [[ "${name}" != [a-z][a-z0-9_]# || "${name}" == acc<-> ||
                  "${name}" == (best|status|account|accounts|t|api|api_*) || -n "${names[$name]}" ]] ||
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
    #: API-billed profiles ([agfi:agy-api]) are named api_NAME and are never
    #: in AGY_ACCOUNTS, so quota reports and agy-best leave them out.
    if [[ "${check}" == check && "${id}" != api_[a-z][a-z0-9_]# ]] ; then
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
    local default_label="${id}"
    if [[ "${id}" == api_*_light ]] ; then
        default_label="api-${${id#api_}%_light}-light"
    elif [[ "${id}" == api_* ]] ; then
        default_label="api-${id#api_}"
    fi
    reply=( "${id}" "${(P)label_var:-${(P)name_var:-${default_label}}}" "${root}" "${root}/antigravity-cli" "${cache}" )
}

function h-agy-account-launcher {
    #: The launcher that resumes a conversation owned by account ID.
    if [[ "${1}" == api_*_light ]] ; then
        ec "agy-api-${${1#api_}%_light}-light"
    elif [[ "${1}" == api_* ]] ; then
        ec "agy-api-${1#api_}"
    else
        ec "agy-${1}"
    fi
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
    #: An API profile's -light variant gets no global instructions.
    local -a light=()
    [[ "${reply[1]}" != api_*_light ]] || light=( --light )
    command python3 "${NIGHTDIR:-${HOME}/scripts}/python/agy_accounts.py" \
        --gemini-dir "${reply[3]}" --shared-dir "${AGY_SHARED_GEMINI_DIR:-${HOME}/.gemini}" \
        --owners-dir "${AGY_ACCOUNTS_DIR:-${HOME}/.agy-accounts}/history-owners" "${light[@]}"
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

function h-agy-model-catalog {
    #: A stale or absent catalog is refreshed with the read-only models command.
    #: Failed refreshes never turn stale model availability into launch consent.
    local id="${1}" refresh="${2:-cached}" model="${3:-}"
    local ttl="${AGY_MODELS_CACHE_TTL:-3600}" timeout="${AGY_MODELS_TIMEOUT:-20}"
    ensure-cmd python3 @RET
    local -a reply catalog_args
    h-agy-account-info "${id}" @RET
    id="${reply[1]}"
    if ! test -s "${reply[4]}/antigravity-oauth-token" ; then
        ecerr "$0: ${id}: not signed in; run agy-acc-login ${id}"
        return 1
    fi
    catalog_args=( catalog --cache "${reply[4]}/models-cache.json" --account "${id}"
        --ttl "${ttl}" --timeout "${timeout}" )
    [[ "${refresh}" != refresh ]] || catalog_args+=( --refresh )
    test -z "${model}" || catalog_args+=( --model "${model}" )
    h-agy-account-argv "${id}" models @RET
    command python3 "${NIGHTDIR:-${HOME}/scripts}/python/agy_model_catalog.py" \
        "${catalog_args[@]}" -- "${reply[@]}"
}

function agy-models-refresh {
    : "usage: agy-models-refresh [ACCOUNT|--all]; refreshes available model IDs without a model turn"
    ensure-cmd jq @RET
    local ids id catalog ret=0
    if (( $# > 1 )) ; then
        ecerr "$0: expected one account or --all"
        return 2
    elif [[ -z "${1}" || "${1}" == --all ]] ; then
        ids="$(h-agy-accounts)" @RET
    else
        local -a reply
        h-agy-account-info "${1}" @RET
        ids="${reply[1]}"
    fi
    for id in "${(@f)ids}" ; do
        if catalog="$(h-agy-model-catalog "${id}" refresh)" ; then
            ec "${id}: cached $(ec "${catalog}" | command jq '.models | length') available models"
        else
            ecerr "$0: ${id}: catalog refresh failed; stale availability will not be used"
            ret=1
        fi
    done
    return "${ret}"
}

function agy-best {
    : "launches the account with most quota for --model (or agy_best_model)"
    local model="${agy_best_model:-}" conversation='' arg i=1
    local ttl="${AGY_MODELS_CACHE_TTL:-3600}"
    local -a input_args=( "$@" ) args=()
    while (( i <= ${#input_args} )) ; do
        arg="${input_args[$i]}"
        case "${arg}" in
            --model|--conversation)
                if (( i == ${#input_args} )) || [[ -z "${input_args[$((i+1))]}" || "${input_args[$((i+1))]}" == --* ]] ; then
                    ecerr "$0: ${arg} requires a value"
                    return 2
                fi
                (( i++ ))
                if [[ "${arg}" == --model ]] ; then
                    model="${input_args[$i]}"
                else
                    conversation="${input_args[$i]}"
                    args+=( "${arg}" "${conversation}" )
                fi ;;
            --model=*)
                model="${arg#*=}"
                test -n "${model}" || { ecerr "$0: --model requires a value"; return 2; } ;;
            --conversation=*) conversation="${arg#*=}"; args+=( "${arg}" ) ;;
            --continue|-c)
                ecerr "$0: use an account launcher for --continue, so history ownership is explicit"
                return 2 ;;
            --) args+=( "${input_args[@][$i,-1]}" ); break ;;
            *) args+=( "${arg}" ) ;;
        esac
        (( i++ ))
    done
    if test -z "${model}" && test -z "${conversation}" ; then
        ecerr "$0: --model (or agy_best_model) is required so selection matches the model launched"
        return 2
    fi
    ensure-cmd python3 jq @RET
    local account json catalog selected ids id ret=0
    local -a catalog_args reply
    if test -n "${conversation}" ; then
        local transcript
        transcript="$(h-agy-session-transcript-of "${conversation}")" @RET
        account="$(h-agy-account-of-transcript "${transcript}")" @RET
        if test -n "${model}" ; then
            catalog="$(h-agy-model-catalog "${account}" cached "${model}")" || {
                ecerr "$0: owning account ${account} cannot verify the requested model"
                return 1
            }
            model="$(ec "${catalog}" | command jq --exit-status --raw-output '.resolved_model.id')" @RET
        fi
        ecerr "$0: resuming with owning account ${account}"
    else
        ids="$(h-agy-accounts)" @RET
        for id in "${(@f)ids}" ; do
            if h-agy-model-catalog "${id}" >/dev/null ; then
                h-agy-account-info "${id}" @RET
                catalog_args+=( --catalog "${id}" "${reply[4]}/models-cache.json" )
            else
                ecerr "$0: skipping ${id}: current model catalog unavailable"
            fi
        done
        (( ${#catalog_args} )) || { ecerr "$0: no current account model catalogs"; return 1; }
        json="$(agy-status --json)" || ret=$?
        selected="$(ec "${json}" | command python3 \
            "${NIGHTDIR:-${HOME}/scripts}/python/agy_model_catalog.py" select \
            --model "${model}" --ttl "${ttl}" "${catalog_args[@]}")" || {
            ecerr "$0: no eligible account for ${model} (status exit ${ret})"
            return 1
        }
        account="$(ec "${selected}" | command jq --exit-status --raw-output '.account')" @RET
        model="$(ec "${selected}" | command jq --exit-status --raw-output '.model')" @RET
        ecerr "$0: selected ${account} for ${model}, $(ec "${selected}" | command jq --raw-output '.remaining_percent')% remaining across applicable quota windows"
    fi
    test -z "${model}" || args=( --model "${model}" "${args[@]}" )
    agy-account "${account}" "${args[@]}"
}

function agy-acc-login {
    : "usage: agy-acc-login [-t|--tmux] [ACCOUNT]; signs in here, optionally in detached tmux"
    local tmux_p=n account='' arg
    for arg in "$@" ; do
        case "${arg}" in
            -t|--tmux) tmux_p=y ;;
            -h|--help) ec 'usage: agy-acc-login [-t|--tmux] [ACCOUNT]'; return 0 ;;
            -*) ecerr "$0: unknown option: ${arg}"; return 2 ;;
            *)
                test -z "${account}" || { ecerr "$0: expected one account"; return 2; }
                account="${arg}" ;;
        esac
    done
    local -a reply
    h-agy-account-info "${account}" @RET
    local id="${reply[1]}" session="agy-login-${reply[1]}"
    if [[ "${tmux_p}" == n ]] ; then
        agy-account-prepare "${id}" @RET
        local -x AGY_ACCOUNT_ID="${id}" AGY_ACCOUNT_LABEL="${reply[2]}"
        h-agy-account-login-run
        return $?
    fi
    ensure-cmd tmux @RET
    local pane_states row pane dead restart_pane=''
    local -a fields
    #: Never use tmuxnewsh2 here: it replaces even a live named session.
    if command tmux has-session -t "=${session}" 2>/dev/null ; then
        pane_states="$(command tmux list-panes -s -t "=${session}" -F '#{pane_id} #{pane_dead}')" @RET
        for row in "${(@f)pane_states}" ; do
            fields=( ${=row} )
            pane="${fields[1]}" dead="${fields[2]}"
            if [[ "${pane}" != %<-> || "${dead}" != (0|1) ]] ; then
                ecerr "$0: cannot determine login pane state"
                return 1
            fi
            if [[ "${dead}" == 0 ]] ; then
                ec "Sign-in already open. Attach: tmux attach-session -t ${(q)session}"
                return 0
            fi
            restart_pane="${restart_pane:-${pane}}"
        done
        test -n "${restart_pane}" || { ecerr "$0: no login panes found"; return 1; }
    fi
    agy-account-prepare "${id}" @RET
    #: The root and label travel as arguments; a tmux server may have old env.
    local script
    script="$(gquote zsh -c 'agy-account-hook "$1" "$2" "$3" h-agy-account-login-run tmux' \
        agy-login "${id}" "${reply[3]}" "${reply[2]}")" @RET
    if test -n "${restart_pane}" ; then
        #: Without -k, tmux refuses if another caller already revived the pane.
        command tmux respawn-pane -t "${restart_pane}" "${script}" @RET
    else
        command tmux new-session -d -s "${session}" "${script}" @RET
    fi
    ec "Attach: tmux attach-session -t ${(q)session}"
    ec "Complete sign-in in that pane; after you exit agy, models verifies the account."
}

function h-agy-account-login-run {
    local -a reply
    h-agy-account-info "${AGY_ACCOUNT_ID}" @RET
    if test -s "${reply[4]}/antigravity-oauth-token" ; then
        ec "File credentials already exist; verifying without signing out."
    else
        antigravity --mode plan @RET
    fi
    local ret=0
    h-agy-account-command "${AGY_ACCOUNT_ID}" models || ret=$?
    ec "Account verification exited ${ret}."
    if [[ "${1}" == tmux ]] ; then
        ec 'Press Enter to close this pane.'
        read -r || true
    fi
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
    AGY_ACCOUNTS="${(j: :)${(@u)ids}}"
    local agy_home="${root}/antigravity-cli"
    local agy_summaries_db="${agy_home}/conversation_summaries.db"
    (( $# && ${+functions[$1]} )) || return 0
    local -a reply
    h-agy-account-info "${id}" >/dev/null 2>&1 || return 0
    "$@" || return 0
}
##
#: API-billed profiles: `agy-api NAME' runs agy on a Gemini API key, or on
#: Vertex AI through Application Default Credentials, instead of a Google
#: subscription. Each profile is the hidden account api_NAME with its own
#: Gemini root; rules, skills and conversation history are shared as for the
#: other accounts. Credentials come from variables named after the profile,
#: which belong in ~/.privateShell, never here, or from a browser sign-in
#: ([agfi:agy-api-login]). See docs/agy_status.md, "API-billed profiles".
function agy-api-refresh {
    : "generates agy-api-<name>[-light][-t] and agy-api-<name>-login from AGY_API_PROFILES"
    local name
    for name in ${=AGY_API_PROFILES:-personal work} ; do
        h-agy-api-name-check "${name}" @RET
        aliasfn "agy-api-${name}" agy-api "${name}"
        aliasfn "agy-api-${name}-t" h-tmuxnewsh2-attach-z-with-cmd "agy-api-${name}"
        aliasfn "agy-api-${name}-light" agy-api --light "${name}"
        aliasfn "agy-api-${name}-light-t" h-tmuxnewsh2-attach-z-with-cmd "agy-api-${name}-light"
        aliasfn "agy-api-${name}-login" agy-api-login "${name}"
    done
}

function h-agy-api-name-check {
    #: Profile names: lowercase words. A trailing _light is reserved, because
    #: the -light variant of profile NAME is the hidden account api_NAME_light.
    if [[ "${1}" != [a-z][a-z0-9_]# || "${1}" == *_light ]] ; then
        ecerr "agy-api: invalid profile name: ${1:-<none>}"
        return 2
    fi
}

function h-agy-api-gcloud-dir {
    #: Where [agfi:agy-api-login] keeps profile NAME's Google sign-in and
    #: project: a gcloud configuration directory of its own, inside gcloud's,
    #: which the agy driver's deny rules already keep from agy children.
    ec "${AGY_API_GCLOUD_DIR:-${HOME}/.config/gcloud/agy-api}/${1}"
}

function h-agy-api-projects {
    #: Usage: ACCESS_TOKEN; prints "PROJECT_ID<TAB>NAME" for each active project
    #: the token's account can see. The token reaches curl on stdin only.
    local token="${1}" page='' out
    while true ; do
        out="$(print -r -- "Authorization: Bearer ${token}" | command curl --silent --show-error --max-time 30 --header @- \
            "https://cloudresourcemanager.googleapis.com/v1/projects?pageSize=500&filter=lifecycleState:ACTIVE${page:+&pageToken=${page}}")" @RET
        if ! print -r -- "${out}" | command jq --exit-status 'has("error") | not' >/dev/null 2>&1 ; then
            ecerr "$0: could not list projects: $(print -r -- "${out}" | command jq -r '.error.message? // .' 2>/dev/null)"
            return 1
        fi
        print -r -- "${out}" | command jq -r '.projects[]? | "\(.projectId)\t\(.name // "")"'
        page="$(print -r -- "${out}" | command jq -r '.nextPageToken // empty')"
        test -n "${page}" || break
    done
}

function agy-api-login {
    : "usage: agy-api-login NAME [PROJECT]; signs profile NAME in to Vertex AI through Google in the browser.
Without PROJECT, and with none saved, it offers the signed-in account's projects in fzf."
    local name="${1}" project="${2}"
    h-agy-api-name-check "${name}" @RET
    local up="${(U)name}"
    local key_var="AGY_API_KEY_${up}" project_var="AGY_API_PROJECT_${up}"
    if test -n "${(P)key_var}" ; then
        ecerr "$0: ${key_var} is set, so agy-api-${name} bills that Gemini API key; remove it from ~/.privateShell to use Vertex AI"
        return 2
    fi
    local dir
    dir="$(h-agy-api-gcloud-dir "${name}")" @RET
    if test -z "${project}" ; then
        project="${(P)project_var}"
    fi
    if test -z "${project}" && test -r "${dir}/project" ; then
        project="$(<"${dir}/project")"
    fi
    if test -n "${project}" && [[ "${project}" != [a-z][-a-z0-9.:]# ]] ; then
        ecerr "usage: agy-api-${name}-login [PROJECT_ID]   (the Google Cloud project that pays for Vertex AI)"
        return 2
    fi
    if ! (( ${+commands[gcloud]} )) ; then
        ecerr "$0: gcloud is not installed"
        return 127
    fi
    command mkdir -p -m 700 -- "${dir}" @RET
    #: The quota project comes from the core project, so a known project is
    #: recorded by the sign-in itself.
    local -a gcloud_env=( CLOUDSDK_CONFIG="${dir}" )
    test -z "${project}" || gcloud_env+=( CLOUDSDK_CORE_PROJECT="${project}" )
    command env "${gcloud_env[@]}" gcloud auth application-default login @RET

    local token state
    token="$(CLOUDSDK_CONFIG="${dir}" command gcloud auth application-default print-access-token 2>/dev/null)"
    if test -z "${token}" ; then
        ecerr "$0: ${name}: the sign-in left no working credentials in ${dir}"
        return 1
    fi
    if test -z "${project}" ; then
        #: Listed with the new sign-in, so these are that account's projects.
        local -a picked
        picked=( "${(@f)$(h-agy-api-projects "${token}" | command sort |
            fz_no_preview=y fz --delimiter=$'\t' --prompt="agy-api-${name} project> " \
                --header='The Google Cloud project that pays for Vertex AI')}" )
        project="${picked[1]%%$'\t'*}"
        if [[ "${project}" != [a-z][-a-z0-9.:]# ]] ; then
            ecerr "$0: ${name}: no project chosen; rerun as agy-api-${name}-login PROJECT_ID"
            return 2
        fi
        if ! CLOUDSDK_CONFIG="${dir}" command gcloud auth application-default set-quota-project "${project}" ; then
            ecerr "$0: ${name}: ${project} is not the quota project (see gcloud's message); Vertex AI still bills it"
        fi
    fi
    print -r -- "${project}" >| "${dir}/project" @RET
    #: Most first runs fail on a project without the Vertex AI API, so check
    #: it now. The token goes to curl on stdin, never on its command line.
    state="$(print -r -- "Authorization: Bearer ${token}" | command curl --silent --max-time 20 --header @- \
        --header "x-goog-user-project: ${project}" \
        "https://serviceusage.googleapis.com/v1/projects/${project}/services/aiplatform.googleapis.com" |
        command jq -r '.state // .error.message // empty' 2>/dev/null)"
    case "${state}" in
        ENABLED) ec "agy-api-${name}: signed in; Vertex AI is enabled in ${project}. Start it with agy-api-${name}." ;;
        DISABLED) ecerr "agy-api-${name}: signed in, but the Vertex AI API is off in ${project}. Turn it on:"
                  ecerr "  https://console.cloud.google.com/apis/library/aiplatform.googleapis.com?project=${project}"
                  return 1 ;;
        *) ecerr "agy-api-${name}: signed in; could not check the Vertex AI API in ${project}: ${state:-no answer}" ;;
    esac
}

function h-agy-api-settings {
    #: Usage: SETTINGS_FILE [PROVIDER]; sets "modelProvider", or removes it
    #: when PROVIDER is empty. Only that key changes.
    local settings="${1}" provider="${2}"
    ensure-cmd jq @RET
    if test -L "${settings}" ; then
        ecerr "$0: refusing a symlinked profile settings file: ${settings}"
        return 1
    fi
    local current='{}' updated tmp
    if test -s "${settings}" ; then
        current="$(command jq --compact-output . "${settings}")" @RET
    fi
    if test -n "${provider}" ; then
        updated="$(print -r -- "${current}" | command jq --compact-output --arg p "${provider}" '.modelProvider = $p')" @RET
    else
        updated="$(print -r -- "${current}" | command jq --compact-output 'del(.modelProvider)')" @RET
    fi
    [[ "${updated}" != "${current}" ]] || return 0
    tmp="$(command mktemp "${settings:h}/.settings-XXXXXX")" @RET
    if print -r -- "${updated}" | command jq . > "${tmp}" ; then
        command mv -f -- "${tmp}" "${settings}"
    else
        command rm -f -- "${tmp}"
        return 1
    fi
}

function agy-api {
    : "usage: agy-api [--light] NAME [agy arguments...]; agy billed to an API key or a Vertex AI project.
--light runs it without the global instructions (AGENTS.md rules), for plain chat."
    local suffix=''
    if [[ "${1}" == --light ]] ; then
        suffix=_light
        shift
    fi
    local name="${1}"
    h-agy-api-name-check "${name}" @RET
    shift
    local up="${(U)name}"
    local key_var="AGY_API_KEY_${up}" project_var="AGY_API_PROJECT_${up}"
    local location_var="AGY_API_LOCATION_${up}" credentials_var="AGY_API_CREDENTIALS_${up}"
    local key="${(P)key_var}" project="${(P)project_var}" credentials="${(P)credentials_var}"
    #: A browser sign-in (agy-api-NAME-login) supplies what the variables do
    #: not; the variables win.
    local gcloud_dir
    gcloud_dir="$(h-agy-api-gcloud-dir "${name}")" @RET
    if test -z "${project}" && test -r "${gcloud_dir}/project" ; then
        project="$(<"${gcloud_dir}/project")"
    fi
    if test -z "${credentials}" && test -r "${gcloud_dir}/application_default_credentials.json" ; then
        credentials="${gcloud_dir}/application_default_credentials.json"
    fi
    if test -n "${key}" && test -n "${project}" ; then
        ecerr "$0: ${name}: ${key_var} and a Vertex AI project (${project_var} or agy-api-${name}-login) are both set; keep one"
        return 2
    elif test -z "${key}" && test -z "${project}" ; then
        ecerr "$0: ${name}: no credentials; run agy-api-${name}-login PROJECT_ID (Vertex AI), or set ${key_var} (Gemini API key)"
        return 2
    elif test -n "${credentials}" && ! test -r "${credentials}" ; then
        ecerr "$0: ${name}: ${credentials_var} is not a readable file: ${credentials}"
        return 2
    fi
    local -a reply
    h-agy-account-info "api_${name}${suffix}" @RET
    local id="${reply[1]}" label="${reply[2]}" cli="${reply[4]}"
    if ! test -d "${cli}" ; then
        agy-account-prepare "${id}" @RET
    fi
    #: The credentials travel as exported locals: they reach only this agy,
    #: and never appear on a command line. `env -u' drops ambient values that
    #: would win over them (agy prefers GOOGLE_API_KEY to GEMINI_API_KEY).
    local -a drop
    if test -n "${key}" ; then
        h-agy-api-settings "${cli}/settings.json" gemini @RET
        local -x GEMINI_API_KEY="${key}"
        drop=( GOOGLE_API_KEY AGY_ADC_AUTH GOOGLE_GENAI_USE_VERTEXAI )
    else
        h-agy-api-settings "${cli}/settings.json" '' @RET
        local -x AGY_ADC_AUTH=1 GOOGLE_CLOUD_PROJECT="${project}"
        local -x GOOGLE_CLOUD_LOCATION="${(P)location_var:-global}"
        if test -n "${credentials}" ; then
            local -x GOOGLE_APPLICATION_CREDENTIALS="${credentials:A}"
        fi
        drop=( GEMINI_API_KEY GOOGLE_API_KEY GOOGLE_GENAI_USE_VERTEXAI )
    fi
    h-agy-account-argv "${id}" "$@" @RET
    local -a unset_args=()
    local var
    for var in "${drop[@]}" ; do
        unset_args+=( -u "${var}" )
    done
    reply=( "${reply[1]}" "${unset_args[@]}" "${(@)reply[2,-1]}" )
    agent_launch_glyph="🪐/${label} " agent_launch_echo_p=y \
        h-agent-launch agy "${reply[@]}"
}

agy-api-refresh
agy-accounts-refresh
##
