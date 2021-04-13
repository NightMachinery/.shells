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
    cat "$file" | sponge | prefixer -i '\x00' -o '\x00' --tac --skip-empty | gawk 'BEGIN { RS="\0";  ORS="\0" } NF && !seen[$0]++' | prefixer -i '\x00' -o '\x00' --tac --skip-empty > "$file" || {
        ectrace
        return $?
    }
    ecgray "$0: completed"
}
##
function clipboard-fz() {
    local copy="${clipboard_fz_copy:-y}"

    local res
    res="$(<$CLIPBOARD_RECORD_FILE fzp --read0 --tac --tiebreak=index "${@:-}")" @RET
    if bool $copy ; then
        assert pbcopy "$res"
    fi
    ec "$res"
}
function clipboard-fz-widget {
    local res i tmp

    zle-print-dots
    {
        res=(${(@0)"$(clipboard_fz_copy=no clipboard-fz --print0 "")"}) @RET # do NOT preserve empty elements; there are stuff with line endings in a single element in some cases

        for i in {1..${#res}} ; do
            tmp=( ${(@ps.\C-^\t\t.)res[$i]} )
            if (( $#tmp == 2 )) ; then
                res[$i]="$tmp[1]"
            fi
        done

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
