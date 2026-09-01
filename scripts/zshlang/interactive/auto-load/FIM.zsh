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

#: Status goes in POSTDISPLAY, not `zle -M'.
#:
#: `zle -M' cannot carry colour at all: it renders the string through ZLE's
#: display code, which *visualises* control characters rather than emitting
#: them, so an SGR escape arrives as a reverse-video `^[' followed by a
#: literal `[38;2;170;170;170m'. That is not a quirk of `-M'; `zle -R' does
#: the same. Measured on the wire, not inferred -- and easy to miss, because
#: `cat -v' renders a real ESC byte and a literal `^'+`[' pair identically.
#:
#: POSTDISPLAY plus `region_highlight' is the mechanism that does work, and it
#: is what zsh-autosuggestions greys its ghost text with: zle applies the
#: colour itself, so what reaches the terminal is a real `\e[38;5;242m'.
typeset -g fim_zle_style="${fim_zle_style:-fg=242}"
typeset -g fim_zle_style_error="${fim_zle_style_error:-fg=red}"

#: What we last put in POSTDISPLAY, and in which style, so the colour can be
#: put back after something else rebuilds `region_highlight'.
typeset -g fim_zle_post_text=''
typeset -g fim_zle_post_style=''
##
function h-fim-zle-highlight {
    #: (Re-)colour whatever we last posted.
    if test -z "${fim_zle_post_style}" ; then
        return 0
    fi

    #: Only if POSTDISPLAY is still ours: zsh-autosuggestions owns the same
    #: slot and takes it back the moment you type.
    if [[ "${POSTDISPLAY}" != "${fim_zle_post_text}" ]] ; then
        return 0
    fi

    region_highlight+=(
        "${#BUFFER} $(( ${#BUFFER} + ${#POSTDISPLAY} )) ${fim_zle_post_style}"
    )
}

function h-fim-zle-post {
    local text="${1}" style="${2}"

    #: Two spaces so it reads as an annotation rather than as buffer text.
    typeset -g fim_zle_post_text="  ${text}"
    typeset -g fim_zle_post_style="${style}"
    POSTDISPLAY="${fim_zle_post_text}"

    h-fim-zle-highlight
}

function h-fim-zle-say {
    #: Status is dim: a footnote to what you are typing, not part of it.
    h-fim-zle-post "FIM: ${1}" "${fim_zle_style}"
}

function h-fim-zle-say-error {
    #: Failures are the one thing here worth looking up for.
    h-fim-zle-post "FIM: ${1}" "${fim_zle_style_error}"
}

function zle-fim-say {
    #: The same, reachable from the `zle -F' handler. POSTDISPLAY and
    #: `region_highlight' are ZLE parameters, so they are only bound inside a
    #: widget -- exactly like BUFFER and CURSOR, and exactly as silent about it.
    h-fim-zle-post "FIM: ${1}" "${2:-${fim_zle_style}}"
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
        h-fim-zle-say "line changed, discarded completion${took}"
        return 0
    fi

    LBUFFER+="${out}"

    #: We take this widget back from fast-syntax-highlighting at startup (see
    #: [agfi:h-fim-zle-unwrap]), so nothing re-colours the code we just
    #: inserted. Do it by hand -- before our own entry goes on, because
    #: `_zsh_highlight' rebuilds `region_highlight' from scratch.
    if (( ${+functions[_zsh_highlight]} )) ; then
        _zsh_highlight
    fi

    h-fim-zle-say "inserted ${#out} chars${took}"
}

function zle-fim-widget {
    setopt localoptions extendedglob

    local provider="${fim_provider:-codestral}"
    local model="${fim_provider_model[${provider}]}"
    if test -z "${model}" ; then
        h-fim-zle-say-error "unknown provider '${provider}'"
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
        h-fim-zle-say-error 'could not start the request'
        return 1
    fi

    typeset -g fim_zle_buffer="${buffer}"
    typeset -gi fim_zle_cursor="${cursor}"
    typeset -g fim_zle_started="${started}"
    typeset -g fim_zle_fd="${fd}" fim_zle_pid="${pid}"

    zle -F "${fd}" h-fim-zle-response
    h-fim-zle-say "requesting ${model}…"
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
        zle zle-fim-say -- "no response${took}" "${fim_zle_style_error}"
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
        zle zle-fim-say -- "${err:-failed (${ret})}" "${fim_zle_style_error}"
        return 0
    fi

    if test -z "${out}" ; then
        zle zle-fim-say -- "empty completion${took}"
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
        #: One press, one job. Escape cancelled a request, so it does not also
        #: leave insert mode -- press it again for that.
        #:
        #: This is also the only ordering that works. `vi-cmd-mode' is still
        #: one of fast-syntax-highlighting's wrapped widgets, so calling it
        #: runs `_zsh_highlight', which rebuilds `region_highlight' and drops
        #: the colour off the message; and posting the message *after* the
        #: call leaves it a redraw behind, so it never appears at all.
        h-fim-zle-say 'aborted'
        return 0
    fi

    #: Nothing of ours to cancel, so Escape is just Escape.
    zle vi-cmd-mode
}

function zle-fim-escape-vicmd {
    if h-fim-zle-cancel ; then
        h-fim-zle-say 'aborted'
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
zle -N zle-fim-say
zle -N zle-fim-escape
zle -N zle-fim-escape-vicmd

bindkey -M viins '^[.' zle-fim-widget
bindkey -M vicmd '^[.' zle-fim-widget
bindkey -M viins '^[' zle-fim-escape    #: was vi-cmd-mode
bindkey -M vicmd '^[' zle-fim-escape-vicmd  #: was beep
##
autoload -Uz add-zsh-hook

function h-fim-zle-unwrap {
    #: Take our widgets back from fast-syntax-highlighting.
    #:
    #: f-sy-h wraps every widget that exists when it loads, and its wrapper
    #: runs `_zsh_highlight' *after* the widget body -- which rebuilds
    #: `region_highlight' from scratch and throws away the entry that colours
    #: our POSTDISPLAY. The in-flight message then came out in whatever style
    #: f-sy-h had left covering that column (38;5;16, near-black) instead of
    #: gray. Widgets created after f-sy-h loads are never wrapped, which is
    #: why an ad-hoc one sourced at the prompt kept its colour and ours did
    #: not: we load at =.zshrc:271= and f-sy-h at =:572=.
    #:
    #: Re-running `zle -N' binds the name straight back to our function. This
    #: has to happen after the whole of =.zshrc=, hence a one-shot precmd.
    zle -N zle-fim-widget
    zle -N zle-fim-accept
    zle -N zle-fim-say
    zle -N zle-fim-escape
    zle -N zle-fim-escape-vicmd

    add-zsh-hook -d precmd h-fim-zle-unwrap
}

add-zsh-hook precmd h-fim-zle-unwrap
##
