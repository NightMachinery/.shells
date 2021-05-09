function hist-add-unquoted() {
    print -r -S -- "$*"
}
function hist-add() {
    print -r -S -- "$(gq "$@")"
}
alias hist-add-self='hist-add "$0" "$@"'
##
function hist-last() {
    local from="${1:-1}"
    local to="$from"

    if (( from == 0 )) ; then
        ecn $history[$HISTCMD]
    else
        builtin fc -I -nl -$from -$to
    fi
    ##
    # -I
    #       restricts to only internal events (not from $HISTFILE)
    #
    # -L
    #       restricts to only local events (not from other shells, see
    #       SHARE_HISTORY in *Note Description of Options:: - note that
    #       $HISTFILE is considered local when read at startup)
    #
    # -l the resulting events are listed on standard output.
    #
    # -n suppresses event numbers when listing
    ##
}
##
function seal-history-append() {
    assert test -e "$UHIST_FILE" @RET

    sync-append "$UHIST_FILE" $'\n'"${*}"$'\n'
}
function seal-history-unquoted() {
    seal-history-append "hist-add-unquoted $(gq "$*")"
}
function seal-history() {
    seal-history-append "hist-add $(gq "$@")"
}
function seal-history-literal-fc() {
    local line="$(hist-last 0)"
    if [[ "$line" =~ '^\s*(?:dbg\s+)?(?:(?:[^#]*#+)|shl|seal-history-literal)\s+((?:.|\n)*)\s*$' ]] ; then
        local cmd="$match[1]"
        ##
        # seal-history-unquoted "$cmd"
        ##
        hist-add-universal-unquoted "$cmd"
        ##
    else
        ecerr "$0: failed to match: $(gq "$line")"
    fi
}

alias shl='seal-history-literal-fc # '
# this hack works only on a single line, as '#' is single-line only
# use 'seal-history-literal' itself for multiline commands and quote expansions accordingly
##
function hist-add-universal-unquoted() {
    # the history file will get created automatically, but let's check its existence to avoid creating it when the notes repo is not present:
    assert test -e "$UHIST_FILE_FC" @RET

    builtin fc -p -a "$UHIST_FILE_FC"
    # `fc -p' pushes the current history list onto a stack and switches
    #  to a new history list.  If the -a option is also specified, this
    #  history list will be automatically popped when the current
    #  function scope is exited
    hist-add-unquoted "$@"
}
function hist-add-universal() {
    hist-add-universal-unquoted "$(gq "$@")"
}
function seal-history-last-fc() {
    hist-add-universal-unquoted "$(hist-last 1)"
}
alias shla='seal-history-last-fc'
##
function hist-inject() {
    assert test -e "$UHIST_FILE" @RET

    source "$UHIST_FILE"
}
function hist-inject-fc() {
    assert test -e "$UHIST_FILE_FC" @RET

    builtin fc -R -I "$UHIST_FILE_FC"
    # `fc -R' reads the history from the given file, `fc -W' writes the
    # history out to the given file, and `fc -A' appends the history out
    # to the given file.  If no filename is specified, the $HISTFILE is
    # assumed.  If the -I option is added to -R, only those events that
    # are not already contained within the internal history list are
    # added.

    ##
    builtin fc -W "$HISTFILE"
    # `fc -A -I` wasn't enough, we need to rewrite the file completely
    #
    #     `fc -R' reads the history from the given file, `fc -W' writes the
    # history out to the given file, and `fc -A' appends the history out
    # to the given file.  If no filename is specified, the $HISTFILE is
    # assumed.  If the -I option is added to -R, only those events that
    # are not already contained within the internal history list are
    # added.  If the -I option is added to -A or -W, only those events
    # that are new since last incremental append/write to the history
    # file are appended/written.  In any case, the created file will
    # have no more than $SAVEHIST entries.
    ##
}
##
