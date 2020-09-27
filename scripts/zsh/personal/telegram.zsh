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
    local files=("$@") rec="${hb265_tlg_rec:-${hb265_tlg_r:-$tlg_amar}}"
    test -z "$rec" && {
        ecerr "$0: Empty receiver"
        return 1
    }
    local f dest finaldest out link
    for f in "$files[@]" ; do
        dest="${f:r}_h265.mp4"
        if out="$(hb265 "$f" "$dest" 2>&1)" && test -e "$dest" ; then
            finaldest=~/Downloads/amar/"${dest:t}"
            mv "$dest" "$finaldest"
            link="$(get-dl-link "${finaldest}")"
            tsend -- "$rec" "$link"
        else
            tsend -- "$water" "$0: Failed for: $f"$'\n\n'"$out"
        fi
    done
}
function hb2amar() {
    transformer realpath 'awaysh-bnamed amar hb265-tlg' "$@"
}
##
