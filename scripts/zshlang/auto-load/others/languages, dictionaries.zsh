##
function transi() {
    bella_zsh_disable1

    trans "$*" | erase-ansi
}
##
function sdc() {
    bella_zsh_disable1

    local q="$(in-or-args $*)"
    q="$(ec "$q" | trim)"

    # it2prof dark
    sdcv --non-interactive --color "$q" | less
    # it2prof 'Hotkey Window'
}

function sdcfa {
    local res
    if ! res="$(sdc "$@")" ; then
        ec "$res" >&2
        return 1
    fi

    (
    ec $res | perl -ne '$. > 3 && print' | html2plain-std | rtl-reshaper-fast &
    local future=$!

    ec $res | perl -pe '$. > 2 && exit' | rtl-reshaper-fast
    ec $res | perl -ne '$. == 3 && print' | erase-ansi | rtl-reshaper-fast

    wait $future
    )
}
##
function pdc() {
    @opts autopaste y @ ffdict "$@"
}

function ffdict() {
    local q="$*" engine=("${ffdict_e[@]:-sdc}") autopaste="${ffdict_autopaste}"

    bella_zsh_disable1

    local w words
    if test -n "$autopaste" && test -z "$q" ; then
        words=("$(strip "$(pbpaste)" '\s+')")
    else
        : "You can use our binding for print-query (currently alt-enter) to print the current query."
        words=( ${(@f)"$(cat "$WORDLIST0" | { fz --exit-0 --query "$q " --print-query || true } | trimsed)"} )
        # `--no-exit-0` has no use for us here
        # https://unix.stackexchange.com/questions/213628/where-do-the-words-in-usr-share-dict-words-come-from

        if (( ${#words} >= 2 )) ; then
            # remove user query:
            words=("${(@)words[2,-1]}")
        fi
    fi

    arrN ${(@)words}

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
aliasfn spi ffdict_e=true ffdict
aliasfn dwn ffdict-wn
##
function sp() {
    if (( $#@ == 0 )) ; then
        spi
        return $?
    fi
    arrN "$@" | ispell
}
##
