typeset -g rabbit1=0AF7KyzWP4KAlUk9PVA
typeset -g rabbit0=0AIWO9GXzMv3kUk9PVA
typeset  -g b6=0AFfTUWcPSmcdUk9PVA # short name might cause name conflict
typeset -g b7=0AG9i3iMiXl8WUk9PVA

function rcr() {
    # rclone - rudi

    local opts=()
    isI && opts+="--progress"
    RCLONE_CONFIG_RUDI_ROOT_FOLDER_ID="$rudi" RCLONE_CONFIG_RABBIT0_ROOT_FOLDER_ID="$rabbit" rclone "$opts[@]" --drive-server-side-across-configs "$@"
}
function rcrget() {
    rudi="$1" rcr copy rudi: rabbit0:g/"$*[2,-1]"
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
        aa-raw "$opts[@]" "$i"
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
