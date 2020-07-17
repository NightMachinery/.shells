typeset -g rabbit1=0AF7KyzWP4KAlUk9PVA
typeset -g rabbit0=0AIWO9GXzMv3kUk9PVA
typeset  -g b6=0AFfTUWcPSmcdUk9PVA # short name might cause name conflict
typeset -g b6_0='1cAmBna4bKr7CHtO6Kfvr56UNOWHFHuLv'
typeset -g b7=0AG9i3iMiXl8WUk9PVA

function rcr() {
    # rclone - rudi

    local opts=()
    isI && opts+="--progress"
    RCLONE_CONFIG_RUDI_ROOT_FOLDER_ID="$(url-tail "$rudi")" RCLONE_CONFIG_RABBIT0_ROOT_FOLDER_ID="$(url-tail "$rabbit")" rclone "$opts[@]" --multi-thread-streams=0 --drive-server-side-across-configs "$@"
}
function rclonef() {
    # eval-memoi does not currently support env vars, so rcr is out
    local paths
    paths=( "${(@f)$(memoi_expire=${rfExpire:-$((3600*24*7))} eval-memoi rclone lsf --recursive "${1}:" | fz)}" ) || return 1
    local i
    ##
    # old API design: <cmd>... <entry-of-fuzzy-paths>
    # rexa "rclone $*[1,-2]" "$paths[@]" # rexa can't handle '/'
    ##
    for i in $paths[@] ; do # skip empty paths
        revaldbg rcr copy "${1}:$i" "$2"
    done

}
function r0() {
    local cache="$HOME/base/cache/"
    mkdir -p $cache
    local paths
    paths=( "${(@f)$(memoi_expire=${rfExpire:-$((3600*24*7))} eval-memoi fd --type file . ~/r0 | fz)}" ) || return 1
    rgeval rsp-dl "$paths[@]" "$cache"
    # local i
    # for i in $paths[@] ; do # skip empty paths
    #     rgeval rsp-dl "$i" $cache
    # done
}
function rcrmount() {
    # needs cask osxfuse
    # vfs cache sucks for streaming, use mpv-cache
    # Update: mount generally sucks in macOS. It causes weird hangs (probably in the kernel) that can't be sudo killed -9. Just copy.
    mkdir -p ~/tmp/cache
    rcr mount --daemon --vfs-cache-max-size 10G --vfs-cache-mode off --cache-dir ~/tmp/cache --vfs-cache-max-age $((24*14))h --vfs-cache-poll-interval 1h "$@"
}
function rcrmount-up() {
    # DEPRECATED: Use aa-2drive
    mkdir -p ~/tmp/cache
    rcr mount --vfs-cache-max-size 3G --vfs-cache-mode writes --cache-dir ~/tmp/cache --vfs-cache-max-age $((1))h --vfs-cache-poll-interval 1h "$@"
}
function rcrget() {
    local id="$(url-tail "$1")"
    rudi="$id" rcr copy rudi: rabbit0:g/"$*[2,-1]"
}
function jdlrc() {
    jglob
    local i
    for i in "$@" ; do
        rcr copy -v --no-traverse "./$i" "rabbit0:julia/$jrabbit"
    done
}
function aa-rc() {
    local dest=( $jrabbit "$(basename "$(pwd)")" )
    local jrabbit="${(j|/|)dest}"
    # dvar jrabbit
    local i
    local opts=()

    for i in "$@" ; do
        [[ "$i" =~ '^-' ]] && {
            opts+="$i"
            continue
        }
        local u="$(uuidgen)"
        pushf "$u"
        aa-raw $opts[@] "$i"
        jdlrc *
        rm *
        popf
    done
}
alias hi10-rc='jrabbit=anime fnswap aa aa-rc'
function hi10-dl-rc() {
    # using `hi10-rc hi10-ng ...` works, too
     hi10-rc hi10-dl < hi10-links.txt
}
function rudi-clone() {
    rudi="$rabbit1" rcr --progress --drive-server-side-across-configs sync rabbit0: rudi:
    rudi="$b7" rabbit="$b6" rcr --progress --drive-server-side-across-configs sync rabbit0: rudi:
}
##
function trr-count {
    local count
    count="$(transmission-show "$1" | gsed -n -e '/FILES/,$p' | wc -l)" || {
        ecerr "Is transmission-show installed?"
        return 1
    }
    ec $(($count - 3)) # 2 extras at top and 1 at buttom
}
function rcraa() {
    # leaves ./aa.log behind
    local aaMark="$(uuidm)"
    aaMark="$aaMark" aa --on-download-complete aa-2drive.zsh "$1"
    till-file "$aaMark"
}
renog rcraa
function rcrtrr() {
    local torrent="$1"
    local start="${2:-1}" # this is the only way to resume this process

    test -n "$torrent" || return 1
    if ! test -f "$torrent" ; then
        local u="./trr_$(md5m "$torrent")"
        mkdir -p "$u"
        magnet2torrent "$torrent" "$u"
        torrent=("$u"/*.torrent)
    fi
    local count
    count="$(trr-count "$torrent")" || return 1
    if (( $start > $count )) ; then
        ecerr "$0: start is bigger than count"
        return 1
    fi

    local i
    for i in {$start..$count} ; do
        local aaMark="$(uuidm)"
        aaMark=$aaMark aa --on-download-complete aa-2drive.zsh --select-file=$i --bt-remove-unselected-file=true "$torrent"
        till-file "$aaMark"
        ec "The ${i}th file has been downloaded" >> aa.log | cat
        sleep 1 # to be able to CTRL-C. You might also want a delay here not to trigger a spam ban ...
    done
}
function rcrb60() {
    doc "Customize b6_0 to set destination, or just use rcrtrrx"
    local torrent="$1"
    local start="$2"

    test -n "$torrent" || return 1
    local name
    name="rcrb60_$(md5m $torrent)" || return 1
    tmuxnewsh2 $name rabbit="$b6_0" rcrtrr "$torrent" "$start"
    ec "Created tmux session '$name'"
}
noglobfn rcrb60
function rcrtrrx() {
    doc "[rabbit=] torrent [start]"
    b6_0="$rabbit" rcrb60 "$@"
}
noglobfn rcrtrrx