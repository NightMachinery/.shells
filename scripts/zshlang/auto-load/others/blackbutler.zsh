##
function butler-p {
    tmux ls |
        rg --smart-case --quiet blackbutler
}
##
function blackbutler-boot {
    tmuxnewsh2 BlackButler BLACKBUTLER_DEBUGME="$BLACKBUTLER_DEBUGME" blackbutler "${@[2,-1]}"
}
##
function bb-say {
    local ret

    ret="$(in-or-args "$@" |
        bb_say.dash |
        jqm .retcode)" @RET

    return $ret
}
##
function h-web2audio-fetch {
    local urls=($@)

    if (( ${#urls} == 0 )) ; then
        return 0
    fi

    local tmp_dir="$HOME/tmp-kindle/web2audio"
    mkdir -p "$tmp_dir" @TRET

    local org_tmp
    org_tmp="$(gmktemp --tmpdir="${tmp_dir}" --suffix='.org')" @TRET

    local plain_tmp
    plain_tmp="$(gmktemp --tmpdir="${tmp_dir}" --suffix='.txt')" @TRET

    local url
    for url in "${urls[@]}" ; do
        readmoz_nosummary="${readmoz_nosummary:-y}" assert readmoz-org "$url" @RET
        ec $'\n'
    done > "$org_tmp"
    ecgray "scraped data to:"$'\n\t'"$org_tmp"

    cat "$org_tmp" | assert org2plain > "$plain_tmp" @RET

    cat "$plain_tmp"
}

function bb-web2audio {
    local out="$1" #: output_path
    shift @RET
    local urls=($@)
    assert-args out urls @RET

    local text
    text="$(reval-memoi h-web2audio-fetch "${urls[@]}")" @TRET

    ec "$text" |
        bb_say_split_mode="${bb_say_split_mode:-sentence}" \
        bb_say_speed="${bb_say_speed:-1}" \
            bb_say_out="${out}" \
            bb-say
}
##
