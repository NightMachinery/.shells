alias tsmf='tsendf $me_tel'
###
function tnotif() {
    # @alt notif
    ##
    ensure-array tnotif_opts
    local msg="$*" opts="${tnotif_opts[@]}"

    (( ${+commands[matrix-send-self]} )) && matrix-send-self "$msg"
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
function tsendf() {
    local f opts=()
    for f in "${@:2}"
    do
        opts+=(--file "$f")
    done
    revaldbg tsend "$opts[@]" -- "$1" ''
}
function tsendf-discrete() {
    local f
    for f in "${@:2}"
    do
        tsend -f "$f" -- "$1" ''
    done
}
function tsend-url() {
    local dest="${1:?}" url="${2:?}" msg="$3"

    local tmp=~/tmp/"$(uuidm)"
    pushf $tmp || return $?
    {
        aacookies "$url" || return $?
        tsend --file * -- $dest "${msg:-$url}"
    } always { popf ; command rm -rf "$tmp" }
}
##
air() { zargs -i ___ -- "$@" -- reval-ec tsendf ___ "$(hear-get)"}
function reval-tlg() {
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

    tsend --parse-mode=md -- "${dest}" "$(cat-paste-if-tty | org2md)"
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
