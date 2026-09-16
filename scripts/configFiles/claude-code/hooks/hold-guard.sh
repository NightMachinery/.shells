#!/usr/bin/env dash
#: Claude Code PreToolUse guard: refuse a tool call that would touch a resource
#: another session is holding. The enforcement half of [agfi:hold-acquire]; the
#: state it reads is written by zshlang/auto-load/others/hold.zsh and the whole
#: story is in scripts/docs/holds.md.
#:
#: Plain `sh' and not a brishz call, for two reasons. A PreToolUse hook blocks
#: by exiting 2, and brishz.dash returns *curl's* exit code rather than the
#: command's, so a hook routed through the garden could never block at all. And
#: this runs before every Bash, Edit and Write, so it has to cost nothing: the
#: common case is no holds, which is one glob and no fork.
#:
#: Fails open by design. A guard that bricks every agent when jq is missing is
#: worse than the accident it prevents. It stops mistakes, not a determined
#: process -- see the "Not a security boundary" section of the doc.

export PATH="$PATH:/usr/local/bin:/opt/homebrew/bin"

hold_dir="${hold_dir:-${HOME}/.night-holds}"

#: Before anything else, including reading stdin: with no holds there is
#: nothing this can say, and that is almost always the case.
[ -d "$hold_dir" ] || exit 0
for f in "$hold_dir"/* ; do
    [ -e "$f" ] && break
    exit 0
done

payload="$(cat)" || exit 0
[ -n "$payload" ] || exit 0

command -v jq > /dev/null 2>&1 || exit 0

#: One jq invocation, not five. Joined on U+001F (unit separator) rather than
#: on a tab: tab is an IFS *whitespace* character, so the shell collapses runs
#: of it into one delimiter and drops empty fields -- a Bash call, whose
#: file_path is empty, would have had its command land in the wrong variable.
#: U+001F is not whitespace, so empty fields survive, and it is stripped from
#: the values first so it cannot appear inside one.
fields="$(printf '%s' "$payload" | jq -j '
    def clean: tostring | gsub("\u001f"; " ");
    [ (.session_id // ""), (.tool_name // ""), (.cwd // ""),
      (.tool_input.file_path // .tool_input.notebook_path // ""),
      (.tool_input.command // "") ] | map(clean) | join("\u001f")' 2>/dev/null)" || exit 0
[ -n "$fields" ] || exit 0

default_ifs="$IFS"
nl="$(printf '\nx')" ; nl="${nl%x}"
us="$(printf '\037')"

IFS="$us"
set -f
# shellcheck disable=SC2086
set -- $fields
set +f
IFS="$default_ifs"

session="$1"
tool="$2"
cwd="$3"
file_path="$4"
bash_command="$5"

#: Mirrors the sanitizing in [agfi:h-hold-holder], so the id written by the
#: shell that took the hold and the id in this payload compare equal.
me="$(printf '%s' "$session" | tr -c 'A-Za-z0-9_@.-' '-')"
[ -n "$me" ] || me='-'

#: Managing a hold necessarily names the resource, so without this the guard
#: would deny the very command that clears it. That is not hypothetical: the
#: holder id is the agent session id, a compaction starts a new one, and the
#: agent would then be locked out of a repository by its own stale hold with no
#: way to release it short of the deadline. Hold management is never the
#: dangerous operation, so it is always allowed.
case "$tool" in
    Bash)
        case "$bash_command" in
            *hold-acquire*|*hold-release*|*hold-renew*|*hold-check*|*hold-status*)
                exit 0 ;;
        esac
        ;;
esac

now="$(date +%s)"

#: `case' rather than a substring test: this is POSIX, fast, and the pattern
#: characters in a path are the caller's problem to have avoided.
under_path() {
    #: $1 the candidate, $2 the directory
    [ -n "$1" ] || return 1
    case "$1" in
        "$2") return 0 ;;
        "$2"/*) return 0 ;;
    esac
    return 1
}

#: A path occurs in a command only when a path boundary sits on each side of
#: it. A plain substring test denied `ls ~/tmpfoo' under a hold on `path:~/tmp',
#: because the held path is a prefix of an unrelated one -- and a guard that
#: cries wolf before every tool call teaches everyone to route around it.
#:
#: Boundary here means: the string end, a `/' (so a file inside the directory
#: still counts), or one of the characters that can end a word in a shell
#: command. Explicit --match literals do NOT go through this: the caller asked
#: for that exact text.
path_named() {
    #: $1 the command, $2 the path
    [ -n "$1" ] && [ -n "$2" ] || return 1

    __rest="$1"
    while : ; do
        case "$__rest" in
            *"$2"*) ;;
            *) return 1 ;;
        esac

        __before="${__rest%%"$2"*}"
        __after="${__rest#*"$2"}"

        #: The character in front, if any, must not be part of a longer word.
        __ok_before=1
        if [ -z "$__before" ] ; then
            __ok_before=0
        else
            case "${__before#"${__before%?}"}" in
                [\ \"\'\(\=\:\,\|\&\;\`\<\>]|'	') __ok_before=0 ;;
            esac
        fi

        if [ "$__ok_before" -eq 0 ] ; then
            case "$__after" in
                '') return 0 ;;
                /*|[\ \"\'\)\:\,\|\&\;\`\<\>]*|'	'*) return 0 ;;
            esac
        fi

        __rest="$__after"
    done
}

contains() {
    #: $1 the haystack, $2 the needle
    [ -n "$1" ] && [ -n "$2" ] || return 1
    case "$1" in
        *"$2"*) return 0 ;;
    esac
    return 1
}

deny() {
    #: $1 the reason, shown to the model and to the user.
    #: JSON on stdout *and* exit 2: exit 2 always blocks, and the message is
    #: taken from permissionDecisionReason when the JSON parses.
    jq -n --arg r "$1" '{hookSpecificOutput:
        {hookEventName: "PreToolUse",
         permissionDecision: "deny",
         permissionDecisionReason: $r}}' 2>/dev/null
    printf '%s\n' "$1" >&2
    exit 2
}

#: Strips leading whitespace with parameter expansion alone -- a command
#: substitution here would be a fork per field per hold, on the hot path.
#: `${v%%[! ]*}' is the run of leading spaces; removing it from the front
#: leaves the value.
trim() {
    __t="$1"
    __t="${__t#"${__t%%[! ]*}"}"
}

for f in "$hold_dir"/* ; do
    [ -f "$f" ] || continue

    until_epoch=''
    holder=''
    resource=''
    reason=''
    matches=''
    path_matches=''

    #: One pass over the file, no forks.
    while IFS= read -r line || [ -n "$line" ] ; do
        case "$line" in
            'until:'*)    trim "${line#until:}"    ; until_epoch="$__t" ;;
            'holder:'*)   trim "${line#holder:}"   ; holder="$__t" ;;
            'resource:'*) trim "${line#resource:}" ; resource="$__t" ;;
            'reason:'*)   trim "${line#reason:}"   ; reason="$__t" ;;
            'match:'*)      trim "${line#match:}"      ; matches="${matches}${nl}${__t}" ;;
            'path-match:'*) trim "${line#path-match:}" ; path_matches="${path_matches}${nl}${__t}" ;;
        esac
    done < "$f"

    #: Expired, unreadable, or ours. Reaping the corpse is
    #: [agfi:h-hold-live-p]'s job and not this one's: a guard on the hot path
    #: has no business deleting files.
    case "$until_epoch" in
        ''|*[!0-9]*) continue ;;
    esac
    [ "$until_epoch" -gt "$now" ] || continue
    [ "$holder" != "$me" ] || continue

    left=$(( (until_epoch - now + 59) / 60 ))
    path_part="${resource#*:}"
    held="held by another session (${holder}) for another ~${left} min. Reason: ${reason}."
    advice="Run 'hold-status' to see it. Do not work in that resource until the holder releases it; if you believe the hold is stale or wrong, ask the user rather than removing it."

    case "$tool" in
        Edit|Write|MultiEdit|NotebookEdit)
            if under_path "$file_path" "$path_part" ; then
                deny "Blocked by a hold: ${file_path} is inside ${resource}, which is ${held} ${advice}"
            fi
            ;;
        Bash)
            if under_path "$cwd" "$path_part" ; then
                deny "Blocked by a hold: this shell's working directory is inside ${resource}, which is ${held} ${advice}"
            fi
            #: The match list is how a resource says what *else* names it. A
            #: vcsh repository needs this: `vcsh night.sh commit' spells the
            #: path nowhere, so the path test alone would let it straight
            #: through. Splitting on newline only, globbing off.
            IFS="$nl"
            set -f
            for m in $matches ; do
                if [ -n "$m" ] && contains "$bash_command" "$m" ; then
                    IFS="$default_ifs"
                    set +f
                    deny "Blocked by a hold: the command names '${m}', which belongs to ${resource} — ${held} ${advice}"
                fi
            done
            for m in $path_matches ; do
                if [ -n "$m" ] && path_named "$bash_command" "$m" ; then
                    IFS="$default_ifs"
                    set +f
                    deny "Blocked by a hold: the command names '${m}', which belongs to ${resource} — ${held} ${advice}"
                fi
            done
            IFS="$default_ifs"
            set +f
            ;;
    esac
done

exit 0
