alias tsmf='tsendf $me'
###
function tnotif() {
    # ALT: notif
    local msg="$*"

    matrix-send-self "$msg"
    tsend --parse-mode markdown -- "$tlg_notifs" "$msg"
}
function tnotifc() {
    # CASUAL Mode
    local msg="$*"

    tsend --parse-mode markdown -- "$tlg_notifc" "$msg"
}
##
function tsendf() {
    
    local f
    for f in "${@:2}"
    do
        tsend -- "$1" '' -f "$f"
    done
}
air() { zargs -i ___ -- "$@" -- reval-ec tsendf ___ "$(mpv-get)"}
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
