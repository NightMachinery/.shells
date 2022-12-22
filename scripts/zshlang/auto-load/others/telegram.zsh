alias tsmf='tsendf $me_tel'
###
function tnotif() {
    # @alt notif
    ##
    ensure-array tnotif_opts
    local msg="$*" opts=("${tnotif_opts[@]}")

    # matrix-send-self "$msg"
    tsend --parse-mode markdown "$opts[@]" -- "$tlg_notifs" "$msg"
}

function tnotif-casual() {
    # CASUAL Mode
    ##
    tlg_notifs="$tlg_notifc" tnotif "$@"
}
@opts-setprefix tnotif-casual tnotif
aliasfn tnotifc tnotif-casual
@opts-setprefix tnotifc tnotif
##
function tsendf-book {
    local dest="${tlg_dest:-$tlg_ch_books}"
    local inargs
    in-or-args3 "$@" @RET

    local tmp
    for f in ${inargs[@]} ; do
        tmp="$(gmktemp --suffix='.png')"
        ebook-cover "$f" "$tmp" @RET

        reval-ec tsendf-discrete "$dest" "$f" "$tmp"
    done
}
@opts-setprefix tsendf-book tsend
##
function tsendf {
    local fs=("${@:2}")
    local album_p="${tsend_album_p:-y}"

    local opts=()
    if ! bool "$album_p" ; then
       opts+="--no-album"
    fi

    local f
    for f in ${fs[@]}
    do
        opts+=(--file "$f")
    done

    revaldbg tsend "$opts[@]" -- "$1" ''
}

function tsendf-discrete {
    tsend_album_p=n tsendf "$@"
    # local f
    # for f in "${@:2}"
    # do
    #     tsend -f "$f" -- "$1" ''
    # done
}

function tsend-url {
    local dest="${1:?}" url="${2:?}" msg="$3"

    local tmp=~/tmp/"$(uuidm)"
    pushf $tmp || return $?
    {
        aa-insecure "$url" || return $?
        local f fs=(*(DN))
        for f in ${fs[@]} ; do
            if test -z "$msg" ; then
               msg="${url}"
            elif [[ "$msg" == 'MAGIC_FILE_TAIL' ]] ; then
               msg="${f:t}"
            fi

            tsend --file $f -- $dest "${msg}"
        done
    } always { popf ; command rm -rf "$tmp" }
}

function tsend-urls {
    local dest="${1}" urls=("${@[2,-1]}")
    assert-args dest urls @RET

    local url
    for url in ${urls[@]} ; do
        reval-ec tsend-url "$dest" "$url" 'MAGIC_FILE_TAIL'
    done
}
##
function air {
    zargs -i ___ -- "$@" -- reval-ec tsendf ___ "$(hear-get)"
}
##
function reval-tlg {
    local rec="${reval_tlg_receiver:-${reval_tlg_r:-$me}}"
    local out="$(eval "$(gquote "$@")" 2>&1)"

    ec "$out"
    tsend -- "$rec" "$out"
}
# aliasfn reval-tlg enve

function tlg-file-captioned() {
    local files=("$@") file
    local rec="$me"

    for file in $files[@] ; do
        file="$(realpath-relchild $PWD "$file")"
        tsend --file "$file" -- "$rec" "$file"
        ec "Sent $file to $rec"
    done
}
function tlg-clean-copied() {
    h_tlg-clean-copied "$(h_tlg-clean-copied "$*")" # telegram sometimes uses two of these tags
}
function h_tlg-clean-copied() {
    local text="$*"
    if [[ "$text" =~ '\[[^]]*\]\s*((.|\n)*)' ]] ; then
       text="$match[1]"
    fi
    print -nr -- "$text"
}
function tlg-clean-paste() {
    tlg-clean-copied "$(pbpaste)"
}
##
function podcast2tel() {
    local dest="${podcast2tel_dest:-${me_tel}}"
    local l="$1"
    local title="$rssTitle" # from rss-tsend

    # tsend --file "$l" -- "$dest" "$title" # sometimes did not work
    tsend-url "$dest" "$l" "$title"
}
##
function md2tlg {
    tsend --parse-mode=md -- "${me_tel}" "$(pbpaste)"
}

function org2tlg {
    local dest="${1:-${me_tel}}"
    assert-args dest @RET
    local text
    text="$(cat-paste-if-tty)" @TRET
    text="$(ec "$text" | org-header-rm-shared-level)" @TRET

    if false ; then
       ensure-array pandoc_opts
       local pandoc_opts=("${pandoc_opts[@]}" --markdown-headings=setext)
       #: =setext= means headers are marked with `====`s and `----`s.
       #: It doesn't look pretty in Telegram, as the underline wraps and becomes multiple lines.
    fi

    local text_md
    text_md="$(ec "$text" | org2md | sd '\\'"('|\"|#|\|)" '$1')" @TRET

    tsend --parse-mode=md -- "${dest}" "$text_md" @RET
}

function org2tlg-with-props {
    cat-paste-if-tty \
        | sd --flags i '^:PROPERTIES:$' '' \
        | sd --flags i '^:END:$' '' \
        | sd --flags i '^#\+begin_quote$' '' \
        | sd --flags i '^#\+end_quote$' '' \
        | sd --flags i '^:visibility: folded$' '' \
        | sd -- '^:' '- :' \
        | org2tlg "$@"
    #   | double-newlines \ # inserted paragraphs between the properties which was not nice
}
##
