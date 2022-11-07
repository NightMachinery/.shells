browser_rec_dir=~/Downloads

function browser-recordings-process() {
    local recs=(${browser_rec_dir}/*.webm(.DN)) out_dir="${browser_recordings_process_outdir:-/Volumes/hyper-diva/video/uni}"

    if test -z "$recs[*]" ; then
        return
    fi

    if ! test -d "$out_dir" ; then
        out_dir=~/Downloads/Video/uni
        mkdir -p "$out_dir"
    fi

    local r
    for r in "$recs[@]"; do
        vid-fix "$r" "$out_dir/${r:t:r} :: $(jalalicli today --jalali-format='MMM w:W E dd').mkv" || {
            ecerr "$0: vid-fix failed $? for '$r'"
            continue
        }
        trs "$r"
    done
    ec "$0: Finished processing '$r'"
}

function browser-recordings-process-watch() {
    ec "$0: Started"

    browser-recordings-process
    while true ; do
        # @todo3 switch to 'fswatch'

        ec "$browser_rec_dir" | entr -dnr true
        # -d Track the directories of regular files provided as input and exit if a new file is added. This option also enables directories to be specified explicitly. Files with names beginning with ‘.’ are ignored.
        # -n Run in non-interactive mode. In this mode entr does not attempt to read from the TTY or change its properties.
        # -r Reload a persistent child process. As with the standard mode of operation, a utility which terminates is not executed again until a file system or keyboard event is processed. SIGTERM is used to terminate the utility before it is restarted. A process group is created to prevent shell scripts from masking signals. entr waits for the utility to exit to ensure that resources such as sockets have been closed. Control of the TTY is not transferred the child process.
        
        ec "$0: Triggered"
        sleep 20 # entr doesn't trigger on rename, so we need to wait for files to fully download https://github.com/eradman/entr/issues/65
        browser-recordings-process
        sleep 1
    done
}
##
function chrome-current-title {
    assert isDarwin @RET

    chrome-cli info | rget 'Title:\s+(.*)' | cat-copy-if-tty
}
aliasfn browser-current-title chrome-current-title # @darwinonly @chromeonly

function chrome-current-url {
    assert isDarwin @RET

    chrome-cli info |
        rget 'Url:\s+(.*)' |
        cat-copy-if-tty
}
aliasfn browser-current-url chrome-current-url # @darwinonly @chromeonly
##
function chrome-open() {
    ensure isDarwin @MRET # @darwinonly
    chrome-cli open "$@"
}

function chrome-open-file() {
    ensure isDarwin @MRET # @darwinonly
    local f="$1"
    ensure-args f @MRET
    shift
    ##
    local url
    url="$(file-unix2uri-rp "$f")" @TRET
    pbcopy "$url" && tts-glados1-cached 'copied'
    ##
    # works badly with Workona, otherwise works fine
    # revaldbg chrome-open $url "$@"
    ##
    # doesn't work
    open -a "/Applications/Google Chrome.app" "$@"
    ##
}
function chrome-open-pdf() {
    ensure isDarwin @MRET # @darwinonly
    local f="$1"
    ensure-args f @MRET

    local w opts=() specialWindow=''
    if test -n "$specialWindow" ; then
        if w="$(chrome-cli list windows | rg '.pdf' | rget '^\s*\[(\d+)\]')" ; then
            opts+=(-w "$w")
        else
            opts+=(-n) # new window
        fi
    fi
    chrome-open-file "$f" "$opts[@]"
}
reify chrome-open-pdf
##
