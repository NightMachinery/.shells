#!/usr/bin/env zshplain.dash
#: The picker that cmd+shift+o falls back to when the kitty window does not say
#: which Claude Code session it shows. [agfi:h-claude-code-session-pick-overlay]
#: runs this in a kitty overlay, passing our PATH along, because fzf needs a
#: terminal and nothing on the other side has one: the hotkey runs in the
#: background, and the garden's shells are not interactive.
#:
#: Everything but fzf and the preview goes through the garden. The rows come
#: from [agfi:h-claude-code-session-pick-rows], and the choice goes back to
#: [agfi:claude-code-view-session-bg] when the hotkey opened us -- it passes
#: the tab's key as CLAUDE_VIEW_TAB_KEY, so the conversion runs in the
#: background under that tab's band and a second cmd+shift+o cancels it -- or
#: to the plain [agfi:claude-code-view-session] otherwise. `brishzq.zsh' rather
#: than `brishz.dash' because it quotes its arguments, so a path is never
#: pasted into a command line raw.
#:
#: Plain `zsh -f' via zshplain.dash: loading zshlang here would cost seconds on
#: every miss, and everything heavy already lives in the garden.
#:
#: The preview is the one thing fzf runs itself, as a bare binary: it fires on
#: every cursor move, and going through the garden for it cost ~380ms a
#: keystroke on top of the shell version's own ~200ms. The garden is still asked
#: for the command line, once, since that is where the knobs live --- see
#: [agfi:h-claude-code-session-preview-cmd].
#:
#: alt+enter converts the highlighted transcript in the background and leaves
#: the picker up, so several can be opened without closing it; pressing it again
#: on the same row cancels that one. Its command line comes from the garden the
#: same way, once --- see [agfi:h-claude-code-session-open-cmd]. Enter still
#: opens under the *tab's* key, this under the *transcript's*, which is why the
#: two do not cancel each other.
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

#: The preview command, the alt+enter command and the header, in one round trip.
#: Bare command names when the garden could not be reached, rather than empty
#: strings: `--preview " {2}"' would have fzf try to run the transcript path.
parts=( ${(@f)"$("${brishzq}" h-claude-code-session-fz-parts)"} )
preview_cmd="${parts[1]:-agent_session claude preview}"
open_cmd="${parts[2]:-brishzb.dash claude-code-view-session-toggle}"
#: An array, not `${header:+--header "${header}"}': zsh does not word-split an
#: unquoted expansion, so that form reaches fzf as the single argument
#: `--header alt+enter: ...' rather than as two.
typeset -a header_opt=()
if [[ -n "${parts[3]}" ]] ; then
    header_opt=( --header "${parts[3]}" )
fi

selected="$(print -r -- "${rows}" |
    fzf --delimiter=$'\t' --with-nth='3..' --no-multi --ansi \
        --prompt='Claude Code session> ' \
        --preview "${preview_cmd} {2}" \
        --preview-window 'down,60%,wrap' \
        --bind "alt-enter:execute-silent(${open_cmd} {2})" \
        "${header_opt[@]}")" || exit 0

fields=( "${(@ps:\t:)selected}" )
transcript="${fields[2]}"
[[ -n "${transcript}" ]] || exit 0

if [[ -n "${CLAUDE_VIEW_TAB_KEY}" ]] ; then
    exec "${brishzq}" claude-code-view-session-bg "${transcript}" "${CLAUDE_VIEW_TAB_KEY}"
fi
exec "${brishzq}" claude-code-view-session "${transcript}"
