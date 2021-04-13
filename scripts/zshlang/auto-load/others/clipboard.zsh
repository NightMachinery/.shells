export CLIPBOARD_RECORD_FILE=~/tmp/.clipboard
function clipboard-record() {
    local sleep=0.5

    ecdate "$0: Started; file=$(gq $CLIPBOARD_RECORD_FILE)"
    clipboard-removedups

    local old paste i counter=0
    while true ; do
        assert pbpaste-plus
        if test -n "$paste[*]" && [[ "$paste[*]" != "$old" ]] ; then
            for i in $paste[@] ; do
                ecdate "Recorded: "
                ec "$i" | prefixer --skip-empty -a $'\t\t\t\t\t| '
                ec
                clipboard-add "$i"
            done
            old="$paste[*]"
        fi
        if (( counter == 7200 )) ; then
            counter=0
            clipboard-removedups
        else
            counter=$(( counter + 1 ))
        fi
        sleep $sleep
    done
}
function clipboard-add() {
    local i="$*" file="$CLIPBOARD_RECORD_FILE"
    assert-args file @RET
    if test -z "$i" ; then
        return 0
    fi


    if ! test -e "$file" ; then
        touch "$file"
        clipboard-init
    fi

    sync-append "$file" $'\0'"$i" || ectrace
}
function clipboard-delall() {
    local file="$CLIPBOARD_RECORD_FILE"
    assert-args file @RET

    reval-ec command rm -rf "$file"
    # reval-ec clipboard-init
}
function clipboard-init() {
    local file="$CLIPBOARD_RECORD_FILE"
    touch "$file" # ensures that we won't get into a loop

    local i
    for i in "${(@ps.\C-^.)$(cat "$attic_emails")}" ; do
        # ectrace
        reval-ec clipboard-add "$(trim "$i")"
    done
    for i in "${(@ps.\C-^.)$(cat "$attic_temoji")}" ; do
        i=( "${(@f)i}" )
        clipboard-add "${i[1]}"$'\C-^\t\t'"${i[2]}"
    done

    local emoji_json=~/Base/_Code/Misc/unicode-emoji-json/data-by-emoji.json
    # https://github.com/muan/unicode-emoji-json
    # https://gitlab.com/NightMachinary/unicode-emoji-json
    if test -e "$emoji_json" ; then
        clipboard-add "$(cat "$emoji_json" | jq -r --nul-output --arg sep $'\C-^\t\t' 'keys[] as $k | "\($k)\($sep)\(.[$k] | .name + " (" + .group +")" )"')"
    else
        ectrace "emoji_trace does not exist" "$emoji_json"
    fi
}
function clipboard-removedups() {
    local file="$CLIPBOARD_RECORD_FILE"
    assert-args file @RET

    # `gtac --separator='\0'` was buggy :|
    local tmp="$(gmktemp)"
    cat "$file" | sponge | prefixer -i '\x00' -o '\x00' --tac --skip-empty | gawk 'BEGIN { RS="\0";  ORS="\0" } NF && !seen[$0]++' | prefixer -i '\x00' -o '\x00' --tac --skip-empty > "$tmp" || {
        ectrace
        return $?
    }
    assert flock "$file" mv "$tmp" "$file" @RET # flock is necessary or we can lose the file with parallel 'clipboard-add's
    ecgray "$0: completed"
}
##
function clipboard-fz-raw() {
    local copy="${clipboard_fz_copy:-y}"

    local res
    res="$(<$CLIPBOARD_RECORD_FILE fzp --read0 --tac --tiebreak=index --height='80%' "${@:-}")" @RET
    if bool $copy ; then
        assert pbcopy "$res"
    fi
    ecn "$res"
}
function clipboard-fz() {
    unset out # @global

    out=(${(@0)"$(clipboard_fz_copy=no clipboard-fz-raw --print0 "${@:-}")"}) @RET # do NOT preserve empty elements; there are stuff with line endings in a single element in some cases

    local i tmp
    for i in {1..${#out}} ; do
        tmp=( ${(@ps.\C-^\t\t.)out[$i]} )
        if (( $#tmp == 2 )) ; then
            out[$i]="$tmp[1]"
        fi
    done
}
function clipboard-fz-widget {
    local res i tmp

    zle-print-dots
    {
        clipboard-fz
        local res=("${out[@]}") ; unset out

        if (( ${#res} >= 2 )) ; then
            res="$(gq "$res[@]")"
        else
            res=${res[*]}
        fi
        LBUFFER="${LBUFFER}${res}"
        assert pbcopy "$res"
    } always {
        zle redisplay
    }
}
##
function iloop-clipboard-fz() {
    @opts e clipboard-fz t Clipper @ iloop "$@"
}
function iloop-chis() {
    @opts e [ sout chis ] t CHIS @ iloop "$@"
}
function iloop() {
    local engine=("${iloop_e[@]}") title="${iloop_t:-iloop}" prompt1="${iloop_p1:-ready: }"
    assert-args engine @RET

    tty-title "$title"

    local q sleep
    {
        while true ; do
            if [[ "$prompt1" == "MAGIC_SKIP" ]] ; then
                q=''
                sleep=y
            else
                ecn "$prompt1"
                read -k 1 q # you can press any whitespace to just trigger fzf
                sleep=''
            fi
            reval "$engine[@]" "${@}" "$(trim "$q")" || {
            # clipboard-fz --bind "change:reload:$(gq cat "$CLIPBOARD_RECORD_FILE")" "${@:-}" || {
                # reload resets selections https://github.com/junegunn/fzf/issues/2441

                test -n "$sleep" && sleep 0.2
                continue
            }
            out="${(j..)out}"
            if test -n "$out" ; then
                assert pbcopy  || continue
            fi
            hs-hyper-x
            if test -n "$out" ; then
                hs-cmd-v
            fi
        done
    } always {
        tty-title "$PWD"
    }
}
##
