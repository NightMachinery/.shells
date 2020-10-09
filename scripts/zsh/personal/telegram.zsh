me="Arstar"
export alice='-1001179162919'
export arista='-1001154785017'
export water='-1001293952668'
export ephemeral='-1001404743282'
export tlogs='-1001460496622'
export tlg_notifs='-1001185370891'
export tlg_notifc="$water"
export tlg_amar='-1001286597974'
###
function alice() {
    tsend -- "$alice" "$*" && ec "Alicized successfully: $*" || ecerr 'Alicization failed!'
}
noglobfn alice
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al

function alicedate() {
    local log
    log="$(cellp 2>&1)" || { # to update reminders
        remj "cellp failed with $?: $log"
    }
    tlg-reminday "$alice"
}
##
function hb265-tlg() {
    local files=("$@")
    local rec="${hb265_tlg_rec:-${hb265_tlg_r:-$water}}" destdir="${hb265_tlg_destdir:-${hb265_tlg_d}}"
    { test -z "$rec" || test -z "$destdir" } && {
        ecerr "$0: Empty args"
        return 1
    }

    local f dest finaldest out link
    for f in "$files[@]" ; do
        dest="${f:r}_h265.mp4"
        if out="$(hb265 "$f" "$dest" 2>&1)" && test -e "$dest" ; then
            finaldest="${destdir}/${dest:t}"
            mv "$dest" "$finaldest"
            link="$(get-dl-link "${finaldest}")"
            tsend -- "$rec" "$link"
        else
            tsend -- "$water" "$0: Failed for: $f"$'\n\n'"$out"
        fi
    done
}
##
function hb265-tlg-bg() {
    local rec="${hb265_tlg_rec:-${hb265_tlg_r:-$water}}" destdir="${hb265_tlg_destdir:-${hb265_tlg_d}}"

    awaysh-bnamed-rp "$0" @opts rec "$rec" destdir "$destdir" @ hb265-tlg "$@"
}
@opts-setprefix hb265-tlg-bg hb265-tlg
function ocwvid-process() {
    local l="$1"

    local name="${$(url-tail "$l"|url-decode.py):r}"
    local caption="New video on OCW:"$'\n'"[$name]($(<<<$l url-encode.py))"

    tsend --link-preview --parse-mode markdown -- $water "$caption"
    tsend --link-preview --parse-mode markdown -- $tlg_amar "$caption"
    pushf "$amardir/wip/$(uuidm)"
    {
        getlinks-c -e '\.mp4$' "$l" | head -1 | inargsf aa -Z || return 3
        mv * "$name".mp4
        hb265-tlg-bg *
    } always { popf }
}
@opts-setprefix ocwvid-process hb265-tlg
##
amardir=~/Downloads/amar
alias withamar='@opts rec "$tlg_amar" destdir "$amardir" @'
aliasfn amar-process withamar ocwvid-process
##
