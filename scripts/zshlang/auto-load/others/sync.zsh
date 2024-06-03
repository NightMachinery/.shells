##
function sync-append {
    # flock alts:
    # zsystem flock
    # lockfile (part of the procmail package)
    # sem which comes as part of the GNU parallel tools
    # https://github.com/tiian/flom

    local file="$1"
    local text="$2"
    local with_newline_p="${sync_append_wnl_p}"
    if test -z "$text" ; then
        return 0
    fi
    assert-args file @RET

    ensure-dir "${file}"

    local lock_fd
    {
        exec {lock_fd}>>$file
        flock -x "$lock_fd"

        if bool "${with_newline_p}" && ! file-ends-with-newline-p "${file}" ; then
            text=$'\n'"${text}"
        fi

        ecn "$text" >> $file
    } always {
        # test -z "$lock_fd" ||
        exec {lock_fd}>&-
        # https://stackoverflow.com/questions/62023144/how-do-i-make-flock-ignore-a-lock-if-its-older-than-x-minutes
        # It seems the file descriptor is automatically closed if the process is killed, so there will be no issue. (The lock will be automatically removed.)
    }
}

function sync-append-with-newline {
    sync_append_wnl_p=y sync-append "$@"
}

function sync-append-in {
    sync-append "$1" "${$(cat ; ecn .)[1,-2]}"
}
##
