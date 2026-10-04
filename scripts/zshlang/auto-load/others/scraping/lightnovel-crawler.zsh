##
function lnc-epub {
    #: Download a novel with lightnovel-crawler 4.x into the current directory.
    #: 4.x writes its artifacts under its data dir, not the CWD, so we copy
    #: out whatever it generated during this run.
    #: Knobs: lnc_format (epub), lnc_chapters (--all; e.g. '--first 3').
    ##
    local url="$1"
    assert-args url @RET
    local fmt="${lnc_format:-epub}"
    local chapters=(${=lnc_chapters:---all})
    local data_dir="${LNCRAWL_DATA_PATH:-$HOME/.lncrawl}"

    if isJulia ; then
        jee
    fi

    reval-ecgray uv tool upgrade lightnovel-crawler

    local marker
    marker="$(mktemp)" @RET
    {
        $proxyenv reval-ec lightnovel-crawler crawl --noin "${chapters[@]}" --format "$fmt" "$url" @RET

        local files=("${data_dir}"/**/*."${fmt}"(.Ne:'[[ $REPLY -nt $marker ]]':))
        if (( ${#files} == 0 )) ; then
            ecerr "$0: no new .${fmt} under ${data_dir}"
            return 1
        fi
        reval-ec command cp -- "${files[@]}" .
    } always {
        command rm -f -- "$marker"
    }

    if isJulia ; then
        jup
        dir2k .
    fi
}
alias lnc='lnc-epub'

function lnc-mobi {
    lnc_format=mobi lnc-epub "$@"
}
##
