function sync-append() {
    # flock alts:
    # zsystem flock
    # lockfile (part of the procmail package)
    # sem which comes as part of the GNU parallel tools
    # https://github.com/tiian/flom

    local file="$1"
    local text="$2"

    local lock_fd
    {
        exec {lock_fd}>>$file
        flock -x "$lock_fd"
        ec "$text" >> $file
    } always {
        # test -z "$lock_fd" ||
        exec {lock_fd}>&-
        # https://stackoverflow.com/questions/62023144/how-do-i-make-flock-ignore-a-lock-if-its-older-than-x-minutes
        # It seems the file descriptor is automatically closed if the process is killed, so there will be no issue. (The lock will be automatically removed.)
    }
}
