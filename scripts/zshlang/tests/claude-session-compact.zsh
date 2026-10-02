#!/usr/bin/env zsh
# Offline regression: zsh -f zshlang/tests/claude-session-compact.zsh
setopt errexit pipefail

typeset -gr compact_test_root="${0:A:h:h:h}"
source "${compact_test_root}/zshlang/basic/basic.plugin.zsh"
source "${compact_test_root}/zshlang/plugins/agent-session/session.zsh"
source "${compact_test_root}/zshlang/auto-load/others/claude-session.zsh"
#: The basic plugin's coloured diagnostics need the full local colour stack.
function ecerr { print -ru2 -- "$@" }

typeset -g compact_test_dir="$(command mktemp -d)"
typeset -gx TMPDIR="${compact_test_dir}"
typeset -g compact_test_log="${compact_test_dir}/events"
typeset -g compact_test_cwd="${compact_test_dir}/source project"
typeset -g compact_test_source_id=11111111-1111-4111-8111-111111111111
typeset -g compact_test_import_id=22222222-2222-4222-8222-222222222222
typeset -g compact_test_source="${compact_test_dir}/default/projects/p/${compact_test_source_id}.jsonl"
typeset -g compact_test_target="${compact_test_dir}/work/projects/p/${compact_test_import_id}.jsonl"
typeset -g compact_test_mode=boundary compact_test_live=n compact_test_cancel=n compact_test_managed=n
command mkdir -p "${compact_test_cwd}"

typeset -gA claude_code_profile_launchers=(default compact-test-default work compact-test-work)
function h-claude-code-profile-assert { [[ "${1}" == (default|work) ]] }
function h-claude-code-profile-config-home {
    if [[ "${1}" == default ]] ; then
        print -r -- "${HOME}/.claude"
    else
        print -r -- "${compact_test_dir}/work"
    fi
}
function h-claude-code-session-resolve { print -r -- "${1}" }
function h-claude-code-session-profile-of {
    [[ "${1}" == */work/* ]] && print work || print default
}
function claude-code-session-import {
    print -r -- "import|${1}|${2}" >> "${compact_test_log}"
    print -r -- "${compact_test_target}"
}
function h-claude-code-session-dep { return 0 }
function h-agent-session-dir { print -r -- "${compact_test_cwd}" }
function h-claude-code-session-live-list {
    [[ "${compact_test_live}" == error ]] && return 1
    if [[ "${compact_test_live}" == y ]] ; then
        print -r -- $'42\t'"${compact_test_source_id}"$'\tlive\t'"${compact_test_cwd}"$'\t'"${compact_test_source}"$'\t-\tbusy\tbackground'
    elif [[ "${compact_test_live}" == malformed ]] ; then
        print -r -- 'incomplete row'
    elif [[ "${compact_test_live}" == target ]] ; then
        print -r -- $'43\t'"${compact_test_import_id}"$'\tlive\t'"${compact_test_cwd}"$'\t'"${compact_test_target}"$'\t-\tbusy\tinteractive'
    fi
    return 0
}
function h-claude-code-session-select-fz {
    print -r -- "pick|${claude_code_view_session_fz_scope}" >> "${compact_test_log}"
    [[ "${compact_test_cancel}" == y ]] && return 130
    print -r -- "${compact_test_source}"
}
function h-agent-session-resume-run {
    local transcript="${1}"
    shift
    if [[ "${compact_test_managed}" == y ]] ; then
        print -r -- "managed|${transcript}" >> "${compact_test_log}"
        return 0
    fi
    ( builtin cd -q -- "${compact_test_cwd}" && "$@" )
}
function compact-test-launch {
    local profile="${1}" kind=launch
    shift
    (( ${@[(Ie)--print]} )) && kind=compact
    print -r -- "${kind}|${profile}|${CLAUDE_CONFIG_DIR-UNSET}|${PWD}|${claude_max_retries-UNSET}|${CLAUDE_CODE_MAX_RETRIES-UNSET}|${(j:|:)@}" >> "${compact_test_log}"
    print -r -- "markers|${kind}|${CLAUDECODE-UNSET}|${CLAUDE_CODE_SESSION_ID-UNSET}|${CLAUDE_PID-UNSET}|${AI_AGENT-UNSET}|${CODEX_THREAD_ID-UNSET}|${AGENT_SESSION_REUSE_PANE-UNSET}|${AGENT_SESSION_HOOK_ARGS_FILE-UNSET}" >> "${compact_test_log}"
    if [[ "${kind}" == compact ]] ; then
        [[ "${compact_test_mode}" == process-fail ]] && return 7
        if [[ "${compact_test_mode}" == wrong-id ]] ; then
            print -r -- "boundary|00000000-0000-4000-8000-000000000000"
        else
            print -r -- "${compact_test_mode}|${2}"
        fi
    fi
}
function compact-test-default { compact-test-launch default "$@" }
function compact-test-work { compact-test-launch work "$@" }
function agent_session {
    [[ "${1}" == claude && "${2}" == compact-result ]] || return 1
    local contents="$(<"${3}")" mode
    mode="$(command python3 -c 'import os,sys; print(oct(os.stat(sys.argv[1]).st_mode & 0o777))' "${3}")"
    print -r -- "verify|${4}|${3}|${mode}" >> "${compact_test_log}"
    [[ "${mode}" == 0o600 ]] || return 1
    [[ "${contents}" == (boundary|noop)\|"${4}" ]]
}
function compact-test-reset {
    : > "${compact_test_log}"
    compact_test_mode=boundary compact_test_live=n compact_test_cancel=n compact_test_managed=n
}
function compact-test-has {
    command grep -F -- "$1" "${compact_test_log}" >/dev/null || {
        print -ru2 -- "FAIL: missing event: $1"
        command cat "${compact_test_log}" >&2
        return 1
    }
}
function compact-test-no {
    if command grep -F -- "$1" "${compact_test_log}" >/dev/null ; then
        print -ru2 -- "FAIL: unexpected event: $1"
        return 1
    fi
}
function compact-test-removed {
    local row capture
    local -a captures
    captures=("${compact_test_dir}"/claude-session-compact.*(N))
    (( ! ${#captures} )) || { print -ru2 -- 'FAIL: leaked compact capture'; return 1; }
    for row in "${(@f)$(<"${compact_test_log}")}" ; do
        [[ "${row}" == verify\|* ]] || continue
        capture="${${(@s:|:)row}[3]}"
        [[ ! -e "${capture}" ]] || { print -ru2 -- "FAIL: leaked capture ${capture}"; return 1; }
    done
}

{
    compact-test-reset
    #: Exercise empty and incomplete live rows under strict parameter access.
    ( setopt nounset ; h-claude-code-session-compact-idle "${compact_test_source}" )
    compact_test_live=malformed
    ( setopt nounset ; h-claude-code-session-compact-idle "${compact_test_source}" )
    compact-test-no 'compact|'

    compact-test-reset
    CLAUDECODE=1 CLAUDE_CODE_SESSION_ID=old-session CLAUDE_PID=123 AI_AGENT=source-agent CODEX_THREAD_ID=old-thread \
        AGENT_SESSION_REUSE_PANE=old-state AGENT_SESSION_HOOK_ARGS_FILE=old-hooks \
        claude-resume-compact "${compact_test_source}"
    compact-test-has 'markers|compact|UNSET|UNSET|UNSET|UNSET|UNSET|UNSET|UNSET'
    compact-test-has 'markers|launch|1|old-session|123|source-agent|old-thread|old-state|old-hooks'
    compact-test-removed

    compact-test-reset
    CLAUDE_CONFIG_DIR=inherited-work claude_max_retries=999 CLAUDE_CODE_MAX_RETRIES=999 \
        claude-resume-compact "${compact_test_source}" '' --model test-model --effort high --tools Bash 'future task' > "${compact_test_dir}/stdout"
    compact-test-has "compact|default|UNSET|${compact_test_cwd}|2|2|--resume|${compact_test_source_id}|--model|test-model|--effort|high|--print|--output-format|stream-json|--verbose|--tools||--|/compact"
    compact-test-has "launch|default|UNSET|${compact_test_cwd}|999|999|--resume|${compact_test_source_id}|--model|test-model|--effort|high|--tools|Bash|future task"
    compact-test-no 'import|'
    [[ ! -s "${compact_test_dir}/stdout" ]]
    compact-test-removed

    compact-test-reset
    claude-resume-compact-work "${compact_test_source}" --model=test-model
    compact-test-has "import|${compact_test_source}|work"
    compact-test-has "compact|work|${compact_test_dir}/work|${compact_test_cwd}|2|2|--resume|${compact_test_import_id}|--model=test-model|--print"
    compact-test-has "verify|${compact_test_import_id}|"
    compact-test-has "launch|work|${compact_test_dir}/work|"
    [[ "$(command head -n 1 "${compact_test_log}")" == import\|* ]]
    compact-test-removed

    compact-test-reset
    CLAUDE_CONFIG_DIR=inherited-work claude-resume-compact-default "${compact_test_target}"
    compact-test-has 'compact|default|UNSET|'
    compact-test-has 'launch|default|UNSET|'

    compact-test-reset
    if claude-resume-compact-work "${compact_test_dir}/default/projects/p/invalid-id.jsonl" ; then return 1; fi
    compact-test-no 'import|'
    compact-test-no 'compact|'

    compact-test-reset
    if claude-resume-compact "${compact_test_source}" '' --model ; then return 1; fi
    compact-test-no 'compact|'

    compact-test-reset
    if claude_code_session_resume_compact_retries=999 claude-resume-compact "${compact_test_source}" ; then return 1; fi
    compact-test-no 'compact|'
    compact-test-no 'launch|'

    compact-test-reset
    compact_test_live=y
    if agent_session_resume_force_p=y claude_code_session_import_force_p=y claude-resume-compact-work "${compact_test_source}" ; then return 1; fi
    compact-test-no 'import|'
    compact-test-no 'compact|'
    compact-test-no 'launch|'

    compact-test-reset
    compact_test_live=target
    if claude-resume-compact-work "${compact_test_source}" ; then return 1; fi
    compact-test-has 'import|'
    compact-test-no 'compact|'
    compact-test-no 'launch|'

    compact-test-reset
    compact_test_live=error
    if claude-resume-compact "${compact_test_source}" ; then return 1; fi
    compact-test-no 'compact|'

    compact-test-reset
    local saved_cwd="${compact_test_cwd}"
    compact_test_cwd="${compact_test_dir}/missing-directory"
    if agent_session_resume_cd_p=n claude-resume-compact "${compact_test_source}" ; then return 1; fi
    compact_test_cwd="${saved_cwd}"
    compact-test-no 'compact|'
    compact-test-no 'launch|'

    local bad
    for bad in --resume=other --continue --fork-session --session-id=other --print --output-format=json --no-session-persistence --disable-slash-commands -p -rfoo -c ; do
        compact-test-reset
        if claude-resume-compact-work "${compact_test_source}" "${bad}" ; then return 1; fi
        compact-test-no 'import|'
        compact-test-no 'compact|'
    done

    local failure
    for failure in process-fail wrong-id malformed ; do
        compact-test-reset
        compact_test_mode="${failure}"
        if claude-resume-compact "${compact_test_source}" ; then return 1; fi
        compact-test-no 'launch|'
        compact-test-removed
    done

    compact-test-reset
    compact_test_mode=noop
    claude-resume-compact "${compact_test_source}"
    compact-test-has 'launch|'
    compact-test-removed

    compact-test-reset
    compact_test_managed=y
    claude-resume-compact "${compact_test_source}"
    compact-test-has 'compact|'
    compact-test-has "managed|${compact_test_source}"
    [[ "$(command tail -n 1 "${compact_test_log}")" == managed\|* ]]
    compact-test-no 'launch|'

    compact-test-reset
    claude-resume-compact-fz '' --effort low
    compact-test-has 'pick|project'
    compact-test-has '|--effort|low|--print|'
    compact-test-reset
    claude-resume-compact-work-all-fz
    compact-test-has 'pick|all'
    compact-test-has 'import|'
    [[ "${claude_code_session_resume_scope-}" == '' ]]
    compact-test-reset
    compact_test_cancel=y
    if claude-resume-compact-all-fz ; then return 1; fi
    compact-test-has 'pick|all'
    compact-test-no 'compact|'
    compact-test-no 'import|'

    compact-test-reset
    CLAUDE_CONFIG_DIR=legacy-env claude-resume "${compact_test_source}" '' --model normal
    compact-test-has 'launch|default|legacy-env|'
    compact-test-no 'compact|'
    compact-test-no 'verify|'

    compact-test-reset
    @opts compact_p y compact_retries 3 @ claude-resume "${compact_test_source}"
    compact-test-has "compact|default|UNSET|${compact_test_cwd}|3|3|"
    [[ "${magic_opts_prefixes[claude-resume-compact-work-all-fz]}" == claude_code_session_resume ]]
    compact-test-removed
    print -r -- 'ok: Claude compact-before-resume (offline stubs)'
} always {
    command rm -rf -- "${compact_test_dir}"
}
