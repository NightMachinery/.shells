##
function chrome-cli {
    CHROME_BUNDLE_IDENTIFIER="${CHROME_BUNDLE_IDENTIFIER:-company.thebrowser.Browser}" \
    command chrome-cli "$@"
}

function with-chrome {
    CHROME_BUNDLE_IDENTIFIER='com.google.Chrome' reval-env "$@"
}

function with-arc {
    CHROME_BUNDLE_IDENTIFIER='company.thebrowser.Browser' reval-env "$@"
}

function with-edge {
    CHROME_BUNDLE_IDENTIFIER='com.microsoft.edgemac' reval-env "$@"
}
##
function browser-recording-postprocess {
    local f="$1"
    local name="${2-${f:r}}"
    assert-args f @RET

    if ! [[ "$name" =~ '_\d\d$' ]] ; then #: if it doesn't end with a date already
        if test -n "$name" ; then
            name+='_'
        fi
        name+="$(datej-named | str2filename)"
    fi
    name+=".${f:e}"

    if [[ "$f" != "$name" ]] ; then
        gmv --verbose "$f" "$name" @RET
    fi

    local path_fixed="${name:r}_FU.mp4" #: fixed, uncompressed
    vid-fix "$name" "$path_fixed" @RET
    command yes n | trs "$name"

    ecbold '--------------'

    local o="${name:r}.mp4"
    hb265 "${path_fixed}" "${o}" @RET
}
aliasfn viddate browser-recording-postprocess
##
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
function browser-current-html {
    assert isDarwin @RET

    chrome-cli source |
        command jq -re . |
        html-links-absolutify "$(browser-current-url)" |
        cat-copy-if-tty
}

function browser-current-links {
    browser-current-html |
        urls-extract |
        perl -ple 's/\\$//g' | #: Sometimes URLs are in the form of `\"http...\"`, and the last backslash is mistakenly detected as part of the URL
        duplicates-clean |
        cat-copy-if-tty
}

function browser-current-title {
    assert isDarwin @RET

    chrome-cli info | rget 'Title:\s+(.*)' | cat-copy-if-tty
}

function h-browser-current-url {
    assert isDarwin @RET

    chrome-cli info |
        rget 'Url:\s+(.*)' |
        cat-copy-if-tty
}
function browser-current-url {
    local url
    url="$(h-browser-current-url)" @RET

    ec "$url" |
        url_clean_redirects=n url-clean-unalix
    #: Or we can only use =url-clean-unalix= if the URL matches certain patterns, e.g., IMDB.
}

function browser-all-urls {
    chrome-cli list links | gcut -d' ' -f '2-'
}
##
aliasfn chrome-current-html with-chrome browser-current-html
aliasfn chrome-current-links with-chrome browser-current-links
aliasfn chrome-current-url with-chrome browser-current-url
aliasfn chrome-all-urls with-chrome browser-all-urls
aliasfn chrome-current-title with-chrome browser-current-title
aliasfn org-link-chrome-current with-chrome org-link-browser-current
##
aliasfn edge-current-url with-edge browser-current-url
aliasfn edge-all-urls with-edge browser-all-urls
aliasfn edge-current-title with-edge browser-current-title
aliasfn org-link-edge-current with-edge org-link-browser-current
##
aliasfn arc-current-html with-arc browser-current-html
aliasfn arc-current-links with-arc browser-current-links
aliasfn arc-current-url with-arc browser-current-url
aliasfn arc-all-urls with-arc browser-all-urls
aliasfn arc-current-title with-arc browser-current-title
aliasfn org-link-arc-current with-arc org-link-browser-current
##
function browser-open {
    ensure isDarwin @MRET # @darwinonly
    chrome-cli open "$@"
}

function browser-open-file {
    ensure isDarwin @MRET # @darwinonly
    local f="$1"
    ensure-args f @MRET
    shift
    ##
    local url
    url="$(file-unix2uri-rp "$f")" @TRET
    pbcopy "$url" && tts-glados1-cached 'copied'
    ##
    #: works badly with Workona, otherwise works fine
    revaldbg browser-open $url "$@"
    ##
    #: doesn't work
    # open -a "/Applications/Google Chrome.app" "$@"
    ##
}

function browser-open-pdf {
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
    browser-open-file "$f" "$opts[@]"
}
reify browser-open-pdf
##
