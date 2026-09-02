##
#: Fill-in-the-middle completion on the command line: =alt+.= sends what is
#: left of the cursor as the prefix and what is right of it as the suffix, and
#: inserts the one-line completion at the cursor.
#:
#: The request itself is [agfi:fim-get] in
#: =zshlang/auto-load/others/fim.zsh=, which is also usable non-interactively.
#: Only the widget, the async plumbing and the bindings live here.
#:
#: See =docs/fim.md=.
##
zmodload zsh/system 2>/dev/null    #: $sysparams[pid], for cancelling
zmodload zsh/datetime 2>/dev/null  #: $EPOCHREALTIME, for the elapsed time
autoload -Uz is-at-least

#: A single in-flight request, so a second =alt+.= supersedes the first rather
#: than racing it. The buffer snapshot is what decides, on arrival, whether the
#: completion still belongs where it was asked for.
typeset -g fim_zle_fd=''
typeset -g fim_zle_pid=''
typeset -g fim_zle_started=''
typeset -g fim_zle_buffer=''
typeset -gi fim_zle_cursor=0

typeset -g fim_zle_us=$'\x1f'  #: field separator of the child's payload

#: Status goes to `zle -M', below the prompt, and is told apart by a symbol
#: rather than by colour.
#:
#: `zle -M' cannot carry colour: it renders the string through ZLE's display
#: code, which *visualises* control characters rather than emitting them, so an
#: SGR escape arrives as a reverse-video `^[' followed by a literal
#: `[38;2;170;170;170m'. `zle -R' behaves the same. Measured on the wire, and
#: easy to get wrong, because `cat -v' renders a real ESC byte and a literal
#: `^'+`[' pair identically.
#:
#: The mechanism that *can* carry colour is POSTDISPLAY plus `region_highlight',
#: which is how zsh-autosuggestions greys its ghost text. Do not use it here.
#: POSTDISPLAY is not a neutral scratch area, it is the suggestion slot, and
#: [agfi:_zsh_autosuggest_accept] does `BUFFER="$BUFFER$POSTDISPLAY"' whenever
#: the cursor is at the end of the line -- so right arrow, `^E', and eleven
#: other accept and partial-accept widgets splice the status message into the
#: command line as real text. Putting a message there also evicts the
#: suggestion you were about to accept, and puts grey text just past the cursor
#: where the trained reflex is to accept it.
#:
#: Hence symbols. They cost nothing, survive anywhere, and are legible on a
#: monochrome terminal or in a pasted transcript.
typeset -g fim_zle_sym_lead="${fim_zle_sym_lead:-❄}"   #: this line is ours
typeset -g fim_zle_sym_wait="${fim_zle_sym_wait:-⋯}"   #: in flight
typeset -g fim_zle_sym_ok="${fim_zle_sym_ok:-✓}"       #: something was inserted
typeset -g fim_zle_sym_none="${fim_zle_sym_none:-∅}"   #: nothing, but no fault
typeset -g fim_zle_sym_err="${fim_zle_sym_err:-✗}"     #: it failed
##
function h-fim-zle-say {
    #: Callable from a widget *and* from the `zle -F' handler: unlike BUFFER,
    #: POSTDISPLAY and friends, `zle -M' works in both.
    #:
    #: A blank line first, so status can never be read as a continuation of
    #: the command you are writing. A newline is safe here even though `zle -M'
    #: visualises control characters -- that treatment is for the unprintable
    #: ones, and a newline comes out as a real line break.
    zle -M $'\n'"${fim_zle_sym_lead} FIM ${1} ${2}"
}
##
function h-fim-zle-child {
    #: Runs in the forked child. Writes one payload to stdout:
    #:   <retcode> US <stderr> US <stdout>
    #:
    #: stderr is captured rather than merged, because merging would splice an
    #: error message into the completion whenever something wrote to stderr on
    #: an otherwise successful call.
    local prefix="${1}" suffix="${2}"

    local errfile
    errfile="$(gmktemp)" || return 1
    #: An `always' block does not run when we are killed; a trap does.
    trap 'command rm -f -- "${errfile}" ; exit 143' TERM INT

    local out ret=0
    out="$(fim-get "${prefix}" "${suffix}" 2>"${errfile}")" || ret=$?

    local err
    err="$(<"${errfile}")"
    command rm -f -- "${errfile}"

    print -rn -- "${ret}${fim_zle_us}${err}${fim_zle_us}${out}"
}
##
function h-fim-zle-cancel {
    #: Forget the pending request, then kill it. Returns true when there was
    #: one, so the caller can decide whether to say anything.
    #:
    #: The order matters for the same reason it does in the Emacs twin: the
    #: state has to be gone before the child dies, so that whatever the death
    #: triggers already looks stale and stays quiet.
    local fd="${fim_zle_fd}" pid="${fim_zle_pid}"

    typeset -g fim_zle_fd='' fim_zle_pid='' fim_zle_started=''

    if test -z "${fd}" ; then
        return 1
    fi

    zle -F "${fd}" 2>/dev/null
    exec {fd}<&- 2>/dev/null

    if test -n "${pid}" ; then
        #: The pid, never the process group. zsh-autosuggestions kills
        #: `-$pid' here to reap anything its strategy forked, but with job
        #: control on, two presses in quick succession can put both process
        #: substitutions in one group -- so killing the group takes the
        #: *replacement* request down along with the one being cancelled, and
        #: it then reports nothing at all. That failed two times in five with
        #: a real endpoint and every single time with the network stubbed out,
        #: which is what made it look like a race in the fd handling.
        #:
        #: Nothing is orphaned by the narrower kill: curl is writing into the
        #: pipe this child holds, so it takes SIGPIPE as soon as the child is
        #: gone.
        kill -TERM "${pid}" 2>/dev/null
    fi

    return 0
}
##
function h-fim-zle-took {
    #: ` in 0.4s', or nothing if we cannot tell.
    local started="${fim_zle_started}"

    if test -z "${started}" || test -z "${EPOCHREALTIME}" ; then
        return 0
    fi

    printf ' in %.1fs' "$(( EPOCHREALTIME - started ))"
}
##
function zle-fim-accept {
    #: The fd handler routes the insertion through this widget, and the
    #: staleness check with it: ZLE's special parameters -- BUFFER, CURSOR --
    #: are only bound inside a widget, and read as empty from a `zle -F'
    #: handler, where the check would fire every single time.
    local out="${1}" took="${2}"

    if [[ "${BUFFER}" != "${fim_zle_buffer}" ]] || (( CURSOR != fim_zle_cursor )) ; then
        #: The line moved under us, so the completion no longer fits where it
        #: was asked for. Dropping it beats inserting it in the wrong place.
        h-fim-zle-say "${fim_zle_sym_none}" "line changed, discarded completion${took}"
        return 0
    fi

    LBUFFER+="${out}"

    #: Invalidate the autosuggestion, which was computed for the line as it was
    #: before this insertion. Because our widgets are named `zle-*' the plugin
    #: never sees the edit and will not do this itself, and a stale suggestion
    #: is not merely out of date:
    #:
    #:   - its `region_highlight' entry still covers the columns the completion
    #:     just landed in, so the code we inserted is painted in the suggestion
    #:     colour and reads as ghost text rather than as the real thing;
    #:   - right arrow still accepts it, appending something that no longer
    #:     follows from the line. With `ec hello' suggesting ` world and then
    #:     some', completing to `ec hello world' left the rest of that
    #:     suggestion live.
    #:
    #: `autosuggest-fetch' rather than `autosuggest-clear', so a suggestion
    #: that fits the *new* line takes its place instead of nothing.
    if (( ${+widgets[autosuggest-fetch]} )) ; then
        zle autosuggest-fetch
    fi

    h-fim-zle-say "${fim_zle_sym_ok}" "inserted ${#out} chars${took}"
}

function zle-fim-widget {
    setopt localoptions extendedglob

    local provider="${fim_provider:-codestral}"
    local model="${fim_provider_model[${provider}]}"
    if test -z "${model}" ; then
        h-fim-zle-say "${fim_zle_sym_err}" "unknown provider '${provider}'"
        return 1
    fi

    h-fim-zle-cancel

    #: `$PREBUFFER', not just `$LBUFFER'. Once zsh is reading a continuation --
    #: an unclosed quote, a `for' waiting for its `done' -- every line before
    #: the current one lives in PREBUFFER, and BUFFER holds only the line being
    #: edited. Without it the model is asked to complete `    ' with no idea
    #: that it is inside a function you opened two lines ago.
    #:
    #: A buffer can also hold real newlines without any continuation, from
    #: ^V^J or from `edit-command-line'; LBUFFER covers that case by itself.
    local buffer="${BUFFER}" prefix="${PREBUFFER}${LBUFFER}" suffix="${RBUFFER}"
    local -i cursor="${CURSOR}"
    local started="${EPOCHREALTIME}"

    local fd
    exec {fd}< <(
        {
            #: Tell the parent our pid, so it can cancel us.
            print -r -- "${sysparams[pid]}"

            fim_provider="${provider}" h-fim-zle-child "${prefix}" "${suffix}"
            #: Nothing may reach the tty from here: it would land in the middle
            #: of the prompt.
        } 2>/dev/null
    )

    #: See zsh-autosuggestions#364: without a forced fork, ^C stops working.
    is-at-least 5.8 || command true

    #: The child prints its pid first thing, so this returns at once; it fails
    #: only on EOF, meaning the fork never got going.
    local pid
    if ! read pid <&$fd ; then
        exec {fd}<&- 2>/dev/null
        h-fim-zle-say "${fim_zle_sym_err}" 'could not start the request'
        return 1
    fi

    typeset -g fim_zle_buffer="${buffer}"
    typeset -gi fim_zle_cursor="${cursor}"
    typeset -g fim_zle_started="${started}"
    typeset -g fim_zle_fd="${fd}" fim_zle_pid="${pid}"

    zle -F "${fd}" h-fim-zle-response
    h-fim-zle-say "${fim_zle_sym_wait}" "requesting ${model}"
}

function h-fim-zle-response {
    #: $1 is the readable fd, $2 an error condition if zle saw one.
    local fd="${1}" condition="${2}"

    #: Tear the handler down on every path, or it fires forever.
    zle -F "${fd}" 2>/dev/null

    local payload=''
    if test -z "${condition}" || [[ "${condition}" == hup ]] ; then
        IFS='' read -rd '' -u "${fd}" payload
    fi
    exec {fd}<&- 2>/dev/null

    if [[ "${fd}" != "${fim_zle_fd}" ]] ; then
        #: Superseded or cancelled while in flight. Its state is already gone,
        #: and saying anything now would report a request nobody is waiting on.
        return 0
    fi

    local took
    took="$(h-fim-zle-took)"
    typeset -g fim_zle_fd='' fim_zle_pid='' fim_zle_started=''

    if [[ "${payload}" != *"${fim_zle_us}"* ]] ; then
        h-fim-zle-say "${fim_zle_sym_err}" "no response${took}"
        return 0
    fi

    local ret="${payload%%${fim_zle_us}*}"
    local rest="${payload#*${fim_zle_us}}"
    local err="${rest%%${fim_zle_us}*}"
    local out="${rest#*${fim_zle_us}}"

    #: `zle -M' shows one line well, and a stacktrace would be several.
    err="${err##*$'\n'}"
    #: We already say FIM, so `fim-get:' would just be said twice.
    err="${err#fim-get: }"

    if [[ "${ret}" != 0 ]] ; then
        h-fim-zle-say "${fim_zle_sym_err}" "${err:-failed (${ret})}"
        return 0
    fi

    if test -z "${out}" ; then
        h-fim-zle-say "${fim_zle_sym_none}" "empty completion${took}"
        return 0
    fi

    zle zle-fim-accept -- "${out}" "${took}"
}
##
#: Escape cancels, and then does whatever it did before. Chaining is why this
#: can be bound permanently: a binding installed only while a request is in
#: flight has to be removed again on every exit path, and a crash between the
#: two would leave Escape wedged.
function zle-fim-escape {
    if h-fim-zle-cancel ; then
        h-fim-zle-say "${fim_zle_sym_none}" 'aborted'
    fi

    #: Whatever Escape did before we took the key over. Safe to call after
    #: reporting now: `zle -M' is not something `_zsh_highlight' can undo,
    #: which it was back when the message lived in `region_highlight'.
    zle vi-cmd-mode
}

function zle-fim-escape-vicmd {
    if h-fim-zle-cancel ; then
        h-fim-zle-say "${fim_zle_sym_none}" 'aborted'
    else
        #: What Escape did here before.
        zle beep
    fi
}
##
#: `zle-' prefixed names are in the default ZSH_AUTOSUGGEST_IGNORE_WIDGETS, so
#: zsh-autosuggestions leaves them alone instead of wrapping them and clearing
#: our message. [agfi:zle-complete-with-dots] is named that way for the same
#: reason.
zle -N zle-fim-widget
zle -N zle-fim-accept
zle -N zle-fim-escape
zle -N zle-fim-escape-vicmd

bindkey -M viins '^[.' zle-fim-widget
bindkey -M vicmd '^[.' zle-fim-widget
bindkey -M viins '^[' zle-fim-escape    #: was vi-cmd-mode
bindkey -M vicmd '^[' zle-fim-escape-vicmd  #: was beep
##
##
