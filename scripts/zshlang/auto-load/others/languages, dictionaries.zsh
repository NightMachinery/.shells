transi() { trans "$*" | erase-ansi }
function sdc() {
    # it2prof dark
    sdcv --non-interactive --color "$*" | less
    # it2prof 'Hotkey Window'
}
# pdc() { sdc "$(strip "$(pbpaste)" '\s+')" }
alias pdc=ffdict
##
function ffdict() {
    local q="${*}" engine=("${ffdict_e[@]:-sdc}")

    local w words
    if test -z "$q" ; then
        words=("$(strip "$(pbpaste)" '\s+')")
    else
        : "You can use our binding for print-query (currently alt-enter) to print the current query."
        words=( ${(@f)"$(cat /usr/share/dict/words | { fz --exit-0 --query "$q " --print-query || true } | trimsed)"} )
        # `--no-exit-0` has no use for us here
        # https://unix.stackexchange.com/questions/213628/where-do-the-words-in-usr-share-dict-words-come-from

        if (( ${#words} >= 2 )) ; then
            # remove user query:
            words=("${(@)words[2,-1]}")
        fi
    fi
    for w in "${(@)words}" ; do
        revaldbg "$engine[@]" "$w"
    done
}
function wordnet() {
    command wn "$@" -over
}
function ffdict-wn() {
    @opts e wordnet @ ffdict "$@"
}
aliasfn di ffdict
aliasfn dwn ffdict-wn
##
function sp() { ispell<<<"$*" ; }
##
