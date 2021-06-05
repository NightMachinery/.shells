alias aacert='aa --ca-certificate=/etc/ssl/certs/ca-certificates.crt'
alias ysp='y-stream-pipe'
aliasfn aa-gateway aacookies
aliasfn aa-insecure aa-gateway --check-certificate=false
aliasfnq aa-multi run-on-each 'aa-gateway --conditional-get=true --allow-overwrite=true' # allows duplicate links without errors
###
function aaserver() {
    mkdir -p ~/Downloads/aas/
    aria2c --rpc-secret "$ARIA_SECRET" --enable-rpc --log-level debug -l ~/Downloads/aas/aria.log -d ~/Downloads/aas/ -D
}
function aac() {
    aria2p --secret "$ARIA_SECRET" "$@"
}
aa-raw() {
    local opts=('--stderr=true')
    # Redirect all console output that would be otherwise printed in stdout to stderr.  Default: false

    isI || opts+=(--show-console-readout false --summary-interval 0)
    test -n "$aaNoSplit" || opts+=(--enable-http-pipelining --split 6 --stream-piece-selector geom)


    local arg
    for arg in $@ ; do
        if url-match "$arg" ; then
            opts+=( --referer "$arg" )
            break
        fi
    done

    if ! isColor ; then
        opts+=( --enable-color=false )
    fi

    aria2c --user-agent "$useragent_chrome" --seed-time=0 --max-tries=0 --retry-wait=1 --file-allocation falloc --auto-file-renaming=false --allow-overwrite=false  $opts[@] "$@"
    local ret=$?
    #-Z has some unsavory sideeffects so I have not included it in this.

    # arger "${funcstack[@]}"
    if isI && @opts p [ aa- ] @ fn-isTop aa aacookies ; then
        ##
        # @done think of some way to only trigger this when we use `aa` directly on the shell.
        # one way is to rename all usages of aa to aa-gateway and then add the bell to aa itself.
        ##
        bell-dl
    fi
    return $ret
}
aagh() { aa "${(@f)$(gh-to-raw "$@")}" }
aacookies() {
    mdoc "$0 <aa-args>
Uses |theCookies| var or else feeds first URL to |cookies|." MAGIC

    aa-raw --header="$(cookies-auto "$@")" $@
}

function aas() {
    : "@deprecated Use y-stream instead."

    local out="$(md5m "$1")"
    aaNoSplit=y aa "$@" --dir "$out" --on-download-complete aa-stream &
    sleep 10
    retry-mpv "'$out'/*" || kill %
}
function aac-getlen() {
    aac call aria2.tellStatus -P "$1" | jqm .completedLength
}
function y-stream-pipe() {
    # https://github.com/mpv-player/mpv/issues/7716
    # https://github.com/ytdl-org/youtube-dl/issues/25344
    # https://github.com/ytdl-org/youtube-dl/issues/25345
    
    magic mdocu '<url>
You can resume the partial download of this function by using |y-stream|.
mpv breaks if you seek to undownloaded location.
Shows you how much is downloaded at least.' ; mret

    local url="$1"

    local out=( "${(@f)$(ybase --get-url --no-part -f best --get-filename -o "%(title)s.%(ext)s" "$url")}" )


    test -n "$out[1]" || { ecerr Could not get video\'s url. Aborting. ; return 1 }
    test -n "$out[2]" || { ecerr Could not get video\'s name. Aborting. ; return 1 }
    local name="$out[2]"
    # local tmuxname="$(<<<$out[2] gtr -cd ' [a-zA-Z0-9]_-')"
    # local dlurl="$out[1]"

    ybase -f best "$url" -o - | tee "$name" | mpv-cache -
}
function y-stream() {
    magic mdoc 'y-stream <url>
Alt: ys.py, y-stream-pipe' ; mret

    
    local out=( "${(@f)$(memoi_expire=$((3600*24)) y --get-url --no-part -f best --get-filename -o "%(title)s.%(ext)s" "$@")}" )


    test -n "$out[1]" || { ecerr Could not get video\'s url. Aborting. ; return 1 }
    test -n "$out[2]" || { ecerr Could not get video\'s name. Aborting. ; return 1 }
    local name="$out[2]"
    local tmuxname="$(<<<$out[2] str2tmuxname)"
    local dlurl="$out[1]"
    
    # '--split 6 --enable-http-pipelining --stream-piece-selector inorder'
    tmuxnewsh $tmuxname ybase --external-downloader aria2c --external-downloader-args '--file-allocation falloc' --no-part -f '(best[height<=800]/best)[protocol^=http]' -o "%(title)s.%(ext)s" "$@"
    # disabling allocation doesn't really add much value. Less errors, I guess, but the play experience doesn't change and it doesn't show the downloaded amount. So we might as well preallocate.

    # ysid="$!" # id of the backgrounded y. You can use it to kill it or whatever.

    ## new2
    {
        until (( ${$(filesizereal $name):-0} > 2000000 ))
        do
            ecerr Not big enough yet
            sleep 1
        done
        mpv-stream "$name"
    } always { tmuxzombie $tmuxname }
    return $?

    # ## new
    # true || {
    #     local gid="$(aac.add.py $dlurl $name)"
    #     until (( ${$(aac-getlen $gid):-0} > 2000000 ))
    #     do
    #         ecerr Not big enough yet
    #         sleep 1
    #     done
    #     mpv $name
    # }
    # ## old
    # true || {
    #     # ec Somehow stderr was being suppressed this one time ...
    #     ecerr trying to start mpv
    #     retry-mpv "${out:q}"
    #     kill -2 -$ysid
    #     # mpv bug here
    #     # kill $!
    #     # kill $! is your friend :))
    # }
}
function aaimg() {
    # do not use aa, as it will retry bad links forever
    getlinks-img "$@" | inargsf sout aria2c -Z
}
##
function aa-remotename() {
    : "supports only a single URL"
    local url="${@[-1]}" rename="${aa_rename}"

    local name=''
    name="$(url-filename "$url")"
    if test -n "$name" ; then
        # When the --force-sequential (-Z) option is used, --out is ignored.

        # --auto-file-renaming doesn't seem to work, so:
        if test -n "$rename" && test -e "$name" ; then
            name="${name:r}_$(uuidm).${name:e}"
        fi
        aa-gateway --out="$name" "$@"
        ## useful opts:
        # --auto-file-renaming=true
        # --conditional-get=true
        ##
        return $?
    else
        aa-gateway "$@"
        return $?
    fi
}
@opts-setprefix aa-remotename aa
##
