##
function transi() {
    bella_zsh_disable1

    trans "$*" | erase-ansi
}
##
function sdc() {
    bella_zsh_disable1

    local opts=("${sdc_opts[@]}")

    local q="$(in-or-args $*)"
    q="$(ec "$q" | trim)"

    # it2prof dark
    sdcv --non-interactive --color "${opts[@]}" "$q" | less
    # it2prof 'Hotkey Window'
}

function h-sdc-fa {
    #: =--only-data-dir= crashes sdcv 0.5.4 on lookups (=map::at: key not found=), so we isolate via =--use-dict= instead.
    local sdc_opts=(--data-dir "$HOME"/.stardict/dic-fa --use-dict Moin)
    sdc "$@"
}

function sdcfa {
    local res
    if ! res="$(transformer en2per h-sdc-fa "$@")" ; then
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
function h-sdcv-de {
    local q="$(in-or-args $*)"
    q="$(ec "$q" | trim)"

    #: =--only-data-dir= crashes sdcv 0.5.4 on lookups (=map::at: key not found=), so we isolate via =--use-dict= instead.
    sdcv --non-interactive --json-output --data-dir "$HOME"/.stardict/dic-de --use-dict 'Wiktionary German-English' "$q"
}

function h-sdcde-impl {
    bella_zsh_disable1

    local renderer=("${sdcde_renderer[@]:-html2plain-std}")

    local json
    json="$(h-sdcv-de "$@")"
    #: sdcv exits nonzero when it finds nothing, so we judge by the output instead. (Its stderr passes through to ours, so hard errors remain visible.)

    local entries
    entries=( ${(@f)"$(ec "$json" | jq -c '.[]' 2> /dev/null)"} )
    if (( ${#entries} == 0 )) ; then
        ecerr "$0: nothing found for: $*"
        return 1
    fi

    local entry
    {
        for entry in "${entries[@]}" ; do
            {
                Bold
                colorfg 100 200 255
                ec "--> $(ec "$entry" | jq -r '.word')"
                resetcolor
            }
            ec "$entry" | jq -r '.definition' | reval "${renderer[@]}"
            ec
        done
    } | pager-if-tty
    local ret=$?
    if (( ret == 141 )) ; then
        #: SIGPIPE from quitting the pager (or a downstream =head=) early is not an error.
        ret=0
    fi
    return $ret
}

function sdcde-ansi {
    local sdcde_renderer=(html2ansi)
    h-sdcde-impl "$@"
}

function sdcde-w3m {
    local sdcde_renderer=(h-html2text-w3m)
    h-sdcde-impl "$@"
}

function sdcde-glow {
    local sdcde_renderer=(h-html2glow)
    h-sdcde-impl "$@"
}

function sdcde-plain {
    local sdcde_renderer=(html2plain-std)
    h-sdcde-impl "$@"
}

aliasfn sdcde-fancy sdcde-ansi
aliasfn sdcde sdcde-fancy

function h-html2text-w3m {
    w3m -dump -T text/html -O UTF-8 -cols "$(terminal-width-get)"
}

function h-html2glow {
    #: glow only emits ANSI when both =CLICOLOR_FORCE= and an explicit style are set; otherwise its non-tty fallback shows raw markdown markers.
    #: =light= matches our terminal theme (see the =bat= theme in cat.zsh).
    pandoc --wrap=none --from html --to gfm-raw_html - -o - |
        CLICOLOR_FORCE=1 glow - -s light --width "$(terminal-width-get)"
}
##
function stardict-wordlist-gen {
    #: Extracts all lookupable words (headwords + synonyms/inflections) from the stardict dictionaries in the given directory.
    local dic_dir="${1:-$HOME/.stardict/dic-de}"
    local out="${2:-${dic_dir}/wordlist.txt}"

    stardict_wordlist.py "$dic_dir" > "$out" @TRET
}

function h-wordlist-ensure {
    local wordlist="$1" dic_dir="$2"

    if ! test -s "$wordlist" ; then
        stardict-wordlist-gen "$dic_dir" "$wordlist" @TRET
    fi
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
