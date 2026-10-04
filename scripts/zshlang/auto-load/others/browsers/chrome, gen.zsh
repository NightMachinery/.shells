##
#: The default browser for all the browser-* functions. Override it to switch
#: defaults; [agfi:browser-focus-p] reads it, too, so both stay in sync.
typeset -g browser_default_bundle_id="${browser_default_bundle_id:-com.brave.Browser}"
# typeset -g browser_default_bundle_id="${browser_default_bundle_id:-com.vivaldi.Vivaldi}"

function chrome-cli {
    CHROME_BUNDLE_IDENTIFIER="${CHROME_BUNDLE_IDENTIFIER:-$browser_default_bundle_id}" \
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

function with-brave {
    CHROME_BUNDLE_IDENTIFIER='com.brave.Browser' reval-env "$@"
}

function with-vivaldi {
    CHROME_BUNDLE_IDENTIFIER='com.vivaldi.Vivaldi' reval-env "$@"
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
    local timeout="${browser_all_urls_timeout}"
    local bundle_id="${CHROME_BUNDLE_IDENTIFIER:-$browser_default_bundle_id}"
    setopt localoptions pipefail
    {
        if [[ -n "${timeout}" ]] ; then
            command gtimeout "${timeout}" env "CHROME_BUNDLE_IDENTIFIER=${bundle_id}" \
                chrome-cli list links
        else
            chrome-cli list links
        fi
    } | command gcut --delimiter=' ' --fields='2-'
}

function h-browser-running-bundle-ids {
    #: NSWorkspace does not launch applications or require System Events access.
    command gtimeout 2s osascript -l JavaScript - "$@" <<'JXA'
ObjC.import('AppKit');
function run(ids) {
    var apps = $.NSWorkspace.sharedWorkspace.runningApplications;
    var running = [];
    for (var i = 0; i < apps.count; i++) {
        var id = ObjC.unwrap(apps.objectAtIndex(i).bundleIdentifier);
        if (ids.indexOf(id) >= 0 && running.indexOf(id) < 0) running.push(id);
    }
    return running.join('\n');
}
JXA
}

function browsers-running-urls {
    : "List every tab URL in running, scriptable macOS browsers."
    local default_id="${CHROME_BUNDLE_IDENTIFIER:-$browser_default_bundle_id}"
    @darwinOnly
    ensure-cmd osascript gtimeout @RET

    local ids=(
        "${default_id}"
        com.google.Chrome com.google.Chrome.canary org.chromium.Chromium
        com.brave.Browser com.brave.Browser.beta com.brave.Browser.nightly
        com.microsoft.edgemac com.microsoft.edgemac.Beta
        com.microsoft.edgemac.Dev com.microsoft.edgemac.Canary
        company.thebrowser.Browser com.vivaldi.Vivaldi
        com.operasoftware.Opera com.apple.Safari com.apple.SafariTechnologyPreview
    )
    local running id urls
    running="$(h-browser-running-bundle-ids "${(@u)ids}")" @RET
    for id in "${(@f)running}" ; do
        [[ -n "${id}" ]] || continue
        case "${id}" in
            com.apple.Safari|com.apple.SafariTechnologyPreview)
                urls="$(command gtimeout 2s osascript -l JavaScript - "${id}" 2>/dev/null <<'JXA'
function run(ids) {
    var app = Application(ids[0]);
    if (!app.running()) return '';
    return app.windows.tabs.url().reduce(function(all, urls) {
        return all.concat(urls);
    }, []).filter(function(url) { return typeof url === 'string'; }).join('\n');
}
JXA
                )" || continue
                ;;
            *)
                #: Reuse [agfi:browser-all-urls], with a deadline per browser.
                #: Failure in one browser must not hide a meeting in another.
                ensure-cmd chrome-cli gcut @RET
                urls="$(CHROME_BUNDLE_IDENTIFIER="${id}" browser_all_urls_timeout=2s \
                    browser-all-urls 2>/dev/null)" || continue
                ;;
        esac
        [[ -z "${urls}" ]] || ec "${urls}"
    done
    return 0
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
aliasfn brave-current-html with-brave browser-current-html
aliasfn brave-current-links with-brave browser-current-links
aliasfn brave-current-url with-brave browser-current-url
aliasfn brave-all-urls with-brave browser-all-urls
aliasfn brave-current-title with-brave browser-current-title
aliasfn org-link-brave-current with-brave org-link-browser-current
##
aliasfn vivaldi-current-html with-vivaldi browser-current-html
aliasfn vivaldi-current-links with-vivaldi browser-current-links
aliasfn vivaldi-current-url with-vivaldi browser-current-url
aliasfn vivaldi-all-urls with-vivaldi browser-all-urls
aliasfn vivaldi-current-title with-vivaldi browser-current-title
aliasfn org-link-vivaldi-current with-vivaldi org-link-browser-current
##
function browser-open {
    @darwinOnly

    local urls=("$@")
    if isInTty && (( $# == 0 )); then
        local pasted_urls
        pasted_urls="$(pbpaste-urls)" @TRET

        if [[ -z "${pasted_urls}" ]]; then
            ecerr "No URLs provided and clipboard content is empty."
            return 1
        fi

        # Split clipboard content by newline into the urls array
        urls=("${(f@)pasted_urls}")
    fi
    urls=($@)  #: remove empty urls

    chrome-cli open "$urls[@]"
}
aliasfn chrome-open with-chrome browser-open
aliasfn brave-open with-brave browser-open
aliasfn vivaldi-open with-vivaldi browser-open

function browser-open-file {
    @darwinOnly

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
    @darwinOnly

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
function browser-open-mindful {
    local browser_command="${browser_open_mindful_engine:-browser-open}"
    local timer_duration="${browser_open_mindful_timer_duration:-10}"

    reval-ecgray "$browser_command" "$@" @RET

    o msg 'You need to re-examine what you are browsing.' @ timer "$timer_duration"
}
aliasfn bom browser-open-mindful
##
