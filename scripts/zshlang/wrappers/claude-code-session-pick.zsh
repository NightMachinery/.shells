#!/usr/bin/env zshplain.dash
#: The picker that cmd+shift+o falls back to when the kitty window does not say
#: which Claude Code session it shows. [agfi:h-claude-code-session-pick-overlay]
#: runs this in a kitty overlay, passing our PATH along, because fzf needs a
#: terminal and nothing on the other side has one: the hotkey runs in the
#: background, and the garden's shells are not interactive.
#:
#: Everything but fzf itself goes through the garden. The rows come from
#: [agfi:h-claude-code-session-pick-rows], the preview from
#: [agfi:h-claude-code-session-preview], and the choice goes back to
#: [agfi:claude-code-view-session]. `brishzq.zsh' rather than `brishz.dash'
#: because it quotes its arguments, so a path is never pasted into a command
#: line raw.
#:
#: Plain `zsh -f' via zshplain.dash: loading zshlang here would cost seconds on
#: every miss, and everything heavy already lives in the garden.
#:
#: Row layout, tab separated: window id, transcript, label, profile, last
#: activity, relative path, snippet. fzf shows from the label on; the
#: transcript is field 2.
##
brishzq="${NIGHTDIR:-${HOME}/scripts}/zshlang/wrappers/brishz/brishzq.zsh"

if ! rows="$("${brishzq}" h-claude-code-session-pick-rows)" || [[ -z "${rows}" ]] ; then
    print -r -- 'claude-code-session-pick: no live Claude Code session has a transcript to show' >&2
    sleep 2
    exit 1
fi

selected="$(print -r -- "${rows}" |
    fzf --delimiter=$'\t' --with-nth='3..' --no-multi \
        --prompt='Claude Code session> ' \
        --preview 'brishzq.zsh h-claude-code-session-preview {2}' \
        --preview-window 'down,60%,wrap')" || exit 0

fields=( "${(@ps:\t:)selected}" )
transcript="${fields[2]}"
[[ -n "${transcript}" ]] || exit 0

exec "${brishzq}" claude-code-view-session "${transcript}"
