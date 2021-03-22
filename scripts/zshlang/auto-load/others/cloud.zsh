typeset -g rabbit1=0AF7KyzWP4KAlUk9PVA
typeset -g rabbit0=0AIWO9GXzMv3kUk9PVA
typeset  -g b6=0AFfTUWcPSmcdUk9PVA # short name might cause name conflict
typeset -g b6_0='1cAmBna4bKr7CHtO6Kfvr56UNOWHFHuLv'
typeset -g b7=0AG9i3iMiXl8WUk9PVA

function rcr() {
    doc "rclone - rudi
Note: rclone, as of yet, does not support resuming downloads.
Use rcrdl to copy from remote to local."

    local opts=()
    isI && opts+="--progress"
    RCLONE_CONFIG_RUDI_ROOT_FOLDER_ID="$(url-tail "$rudi")" RCLONE_CONFIG_RABBIT0_ROOT_FOLDER_ID="$(url-tail "$rabbit")" rclone "$opts[@]" --multi-thread-streams=0 --drive-server-side-across-configs "$@"
}
function rcrdl() {
    : "rclone auto-skips existing files, but we need special support for tags."
    : "Use 'rcrget.' to download a remote dir to PWD"

    local remote="$1" local="$2"
    local localpath="$local"

    if ! [[ "$destdir" =~ '^\w+:' ]] ; then # if is local
        if test -d "$localpath" ; then
            localpath+="/${remote:t}"
        fi
        localpath="$(ntag-recoverpath "$localpath")" # automaticallys skips if 'local' is actually another remote, no?
        if test -e "$localpath" ; then
            ecerr "$0:WARN: already exists: $localpath"
        fi
    else
        localpath+="/${remote:t}"
    fi

    : "rclone auto-skips existing files (testing by size and modification time or MD5SUM)."
    revaldbg rcr copyto "$remote" "$localpath"
}
function rcr-list() {
    fnswap isI false rcr lsf --recursive --files-only "${@}" | gsort
}
function rclonef() {
    doc "Warning: you need to supply exactly one / after the directory, or it won't work. Examples: 'rabbit0:' 'rabbit0:g/'"
    local rgquery=("${rclonef_rgquery[@]:-.}") query="${rclonef_query}" remote="${1:?}" local="${2:?}"

    local paths
    paths=( "${(@f)$(memoi_expire=${rfExpire:-$((3600*24*7))} memoi_skiperr=y memoi_key="$rudi||$rabbit" eval-memoi rcr-list "$remote" | rg --smart-case "$rgquery[@]" | fz --preview-window down:4:wrap --query "$query ")}" ) || return 1
    local i
    ##
    # old API design: <cmd>... <entry-of-fuzzy-paths>
    # rexa "rclone $*[1,-2]" "$paths[@]" # rexa can't handle '/'
    ##
    for i in $paths[@] ; do # skip empty paths
        local destdir="$local/${i:h:h:t}/${i:h:t}"
        if ! [[ "$destdir" =~ '^\w+:' ]] ; then # skip if 'local' is actually another remote
            mkdir -p "$destdir"
        fi
        reval-ec rcrdl "${remote}$i" "$destdir"
    done

}
function r1() {
    local root="${r1_root:-${r1_r}}" query='' rgquery="$(rg-createquery "$@")"
    # query="$(fz-createquery "$@")"

    local cache="${r1_path:-${r1_p:-$HOME/base/cache/}}"
    mkdir -p $cache
    rabbit="$root" @opts rgquery "$rgquery" query "$query" @ rclonef rabbit0: $cache
}
function r0() {
    # this supports resume but needs a mounted drive `rcrmount rabbit0: ~/r0`
    # ALT: r1 (rclonef)(faster, doesn't need mount)
    local cache="$HOME/base/cache/"
    mkdir -p $cache
    local paths
    paths=( "${(@f)$(memoi_expire=${rfExpire:-$((3600*24*7))} memoi_skiperr=y eval-memoi fd --type file . ~/r0 | fz)}" ) || return 1
    rgeval rsp-dl "$paths[@]" "$cache"
    # local i
    # for i in $paths[@] ; do # skip empty paths
    #     rgeval rsp-dl "$i" $cache
    # done
}
function rcrmount() {
    # needs cask osxfuse
    # vfs cache sucks for streaming, use r0 to copy it or mpv-cache
    # mount (with or without cache) generally sucks in macOS. It causes weird hangs (probably in the kernel) that can't be sudo killed -9.
    mkdir -p ~/tmp/cache
    # the daemon still leaves logs behind, though I don't know where
    rcr mount --daemon --vfs-cache-max-size 10G --vfs-cache-mode off --cache-dir ~/tmp/cache --vfs-cache-max-age $((24*14))h --vfs-cache-poll-interval 1h "$@"
}
function rcrmount-up() {
    # DEPRECATED: Use rcraa or rcrtrr
    mkdir -p ~/tmp/cache
    rcr mount --vfs-cache-max-size 3G --vfs-cache-mode writes --cache-dir ~/tmp/cache --vfs-cache-max-age $((1))h --vfs-cache-poll-interval 1h "$@"
}
function rcrget() {
    local id="$(url-tail "$1")" dest="${rcrget_dest:-rabbit0:g}"
    rudi="$id" rcr copy rudi: "$dest"/"$*[2,-1]"
}
function rcrget.() {
    rcrget_dest=. rcrget "$@"
}
##
function jdlrc() {
    jglob
    local i
    for i in "$@" ; do
        reval-ec rcr copy -v --no-traverse "./$i" "rabbit0:julia/$jrabbit"
    done
}
function aa-rc() {
    : "Alt: rcraa"
    
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
        till-file "$aaMark" # necessary; otherwise the next download will delete the current file.
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
