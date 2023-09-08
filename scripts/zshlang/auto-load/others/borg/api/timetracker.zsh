##
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
function tt-rename0 {
    local from="$1" to="$2"
    assert-args from to @RET

    local literal="${tt_rename_literal}"
    local reval_confirm_before=(backup-file "$timetracker_db")
    if bool $literal ; then
        reval-confirm sqlite3 "$timetracker_db" 'UPDATE activity SET name = REPLACE(name, '"$(gquote-dq "$from")"', '"$(gquote-dq "$to")"')'
    else
        if true ; then
            reval-confirm tt_renamer.py --path "${timetracker_db}" \
                --from "$from" --to "$to"
        else;
            local ext
            # [[id:1967b990-6b27-4597-a215-df816a3e76c6][sqlite/extensions/regex: Collection of Extension Functions for SQLite3]]
            if isLinux ; then
                ext=~cod/misc/sqlite3-extras/sqlite3-extras.so
            elif isDarwin ; then
                ext=~cod/misc/sqlite3-extras/sqlite3-extras.dylib
            else
                @NA
            fi
            assert test -e "$ext" @RET

            reval-confirm sqlite3 "$timetracker_db" "SELECT load_extension($(gquote-sq "$ext")) ; "'UPDATE activity SET name = SUB('"$(gquote-sq "$from")"', '"$(gquote-sq "$to")"', name)'
        fi
    fi
}
@opts-setprefix tt-rename0 tt_rename

aliasfn tt-rename tt-reval-diff tt-rename0

function tt-reval-diff {
    test -e "$timetracker_db" @TRET

    reval-ec backup-file "$timetracker_db" @TRET

    local old
    old="$(gmktemp)" @TRET
    catsql "$timetracker_db" > "$old" @TRET

    reval "$@" @RET

    git-diff "$old" =(catsql "$timetracker_db")
}
##
