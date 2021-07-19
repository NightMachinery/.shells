function borg-req() {
    curl --fail --silent --location --header "Content-Type: application/json" --data '@-' $borgEndpoint/"$@"
}
##
function borg-tt-mark() {
    local received_at="${borg_tt_mark_at}"
    if isLocal ; then
        revaldbg brishzr @opts at "$received_at" @ "$0" "$@"
        return $?
    fi

    local out
    out="$(ec "$*" | text2num | jq --raw-input --arg received_at "$received_at" --slurp --null-input --compact-output 'inputs as $i | {"name": $i, "received_at": $received_at}' | borg-req timetracker/mark/)" @TRET

    assert reval-not isSpace "$out" @RET

    if [[ "$out" =~ 'cold shoulder|Julia encountered an exception\.' ]] ; then
        ecerr "$0: Encountered an error"$'\n'"$out"
        return 1
    else
        ec $out
        return 0
    fi
}

function borg-tt-last() {
    local count="${1:-6}"

    catsql --table activity --order id- --limit "$count" "$timetracker_db" | gsed -n '4,$p' | sd -f m '^\d+,' '' | sdlit $'\n' $'\n\n' | sdlit , $'\n    '
}
##
function borg-tt-cmdlog() {
    local input="$(cat)"

    local received_at cmd=() i
    for i in ${(@f)input} ; do # skips empty lines
        if [[ "$i" =~ '^\s*:::(.*)' ]] ; then
            if test -n "$received_at" && (( $#cmd >= 1 )) ; then
                reval-ec @opts at "$received_at" @ borg-tt-mark "${(@F)cmd}"
                ec
            fi
            received_at="$match[1]"
            cmd=()
        elif test -z "$received_at" ; then
            ecerr "input does not begin with received_at"
            return 1
        else
            cmd+="$i"
        fi
    done
    # @copypaste :
    if test -n "$received_at" && (( $#cmd >= 1 )) ; then
        reval-ec @opts at "$received_at" @ borg-tt-mark "${(@F)cmd}"
        ec
    fi
}
##
function cmdlog {
    local log="$cmdlog"
    assert-args log @RET

    ensure-dir "$log"

    cmdlog.py "$@" >&1 >> $log @TRET
}

function cmdlog-archive-current {
    local log="$cmdlog"
    assert-args log @RET

    if test -e "$log" ; then
        assert gmv --verbose --backup=numbered --suffix='.old' "$log" "${log:h}/oldlog_$(dateshort)" @RET
    else
        ecerr "$0: no current cmdlog exists"
    fi
}

function cmdlog-apply {
    local log="$cmdlog"
    assert-args log @RET

    assert test -e "$log" @RET

    cat "$log" | borg-tt-cmdlog @TRET

    reval-ec cmdlog-archive-current
}
##
