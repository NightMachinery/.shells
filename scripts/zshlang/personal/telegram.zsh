export me_tel="Arstar"
me="$me_tel"
export alice='-1001179162919'
export arista='-1001154785017'
export water='-1001293952668'
export ephemeral='-1001404743282'
export tlogs='-1001460496622'
export tlg_notifs='-1001185370891'
export tlg_notifc="$water"
# export tlg_amar='-1001286597974'
export tlg_amar="$water"
export tlg_podcastgen='-1001222930214'
###
function alice() {
    local i="$*"

    local res
    if res="$(borg-tt-mark "$i")" && test -n "$res" ; then # brishzr is invoked for 'borg-tt-mark' on isLocal automatically
        ec "$res"
        bell-pp-electricity
        @opts dur 10 @ alert "$res"
        # ec "Alicized successfully: $i"
        true
    else
        ec "$res"
        ecdate-err "Alicization failed"'!'
        redo2 2 tts-glados1-cached Alicization has failed
        return 1
    fi
}
noglobfn alice
alias al=alice #NAMECONFLICT: ../Cellar/mono/6.8.0.105/bin/al

function alicedate() {
    local log
    log="$(cellp 2>&1)" || { # to update reminders
        remj "$0, $(hostname), $(dateshort): cellp failed with $?: $log"
    }
    tlg-reminday "$alice"
    isServer && ensure wallpaper-auto-ipad @MRET
    true
}
##
function hb265-tlg() {
    local files=("$@")
    local rec="${hb265_tlg_rec:-${hb265_tlg_r:-$water}}" destdir="${hb265_tlg_destdir:-${hb265_tlg_d:-$HOME/tmp/hb265}}"
    assert-args files rec destdir @RET

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
    local rec="${hb265_tlg_rec:-${hb265_tlg_r:-$water}}" destdir="${hb265_tlg_destdir:-${hb265_tlg_d:?}}"

    awaysh-bnamed-rp "$0" failnoisily @opts rec "$rec" destdir "$destdir" @ hb265-tlg "$@"
}
@opts-setprefix hb265-tlg-bg hb265-tlg
function ocwvid-process() {
    local l="$1"

    local name="${$(url-tail "$l"|url-decode.py):r}"
    local caption="New video on OCW:"$'\n'"[$name]($(<<<$l url-encode.py))"

    tsend --link-preview --parse-mode markdown -- $water "$caption"
    tsend --link-preview --parse-mode markdown -- $tlg_amar "$caption"
    pushf "$amardir/wip/$(uuidm)" || return $?
    {
        getlinks-c -e '\.mp4$' "$l" | head -1 | inargsf aa -Z || return 3
	local dest="$name".mp4
        mv * $dest
        failnoisily hb265-tlg-bg $dest
    } always { popf }
}
@opts-setprefix ocwvid-process hb265-tlg
##
amardir=~/Downloads/amar
alias withamar='@opts rec "$tlg_amar" destdir "$amardir" @'
aliasfn amar-process withamar ocwvid-process
##
