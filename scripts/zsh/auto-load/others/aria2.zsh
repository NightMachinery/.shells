alias aacert='aa --ca-certificate=/etc/ssl/certs/ca-certificates.crt'
alias ysp='y-stream-pipe'
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
    aria2c --seed-time=0 --max-tries=0 --retry-wait=1 --file-allocation falloc $opts[@] "$@" #-Z has some unsavory sideeffects so I have not included it in this.
}
aagh() { aa "${(@f)$(gh-to-raw "$@")}" }
aacookies() {
    mdoc "$0 <aa-args>
Uses |theCookies| var or else feeds first URL to |cookies|." MAGIC

    aa-raw --header="$(cookies-auto "$@")" $@
}

function aas() {
    # aa "$@" --on-download-start aa-stream
    local out="$(md5 <<<"$1")"
    aa "$@" --dir "$out" --on-download-complete aa-stream &
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
    local tmuxname="$(<<<$out[2] gtr -cd ' [a-zA-Z0-9]_-')"
    local dlurl="$out[1]"
    
    # '--split 6 --enable-http-pipelining --stream-piece-selector inorder'
    revaldbg tmuxnew $tmuxname $(expand-alias-strip ybase) --external-downloader aria2c --external-downloader-args '--file-allocation falloc' --no-part -f '(best[height<=800]/best)[protocol^=http]'  -o "%(title)s.%(ext)s" "$@"
    # disabling allocation doesn't really add much value. Less errors, I guess, but the play experience doesn't change and it doesn't show the downloaded amount. So we might as well preallocate.

    # ysid="$!" # id of the backgrounded y. You can use it to kill it or whatever.

    ## new2
    {
        until (( ${$(filesizereal $name):-0} > 2000000 ))
        do
            ecerr Not big enough yet
            sleep 1
        done
        mpv "appending://$name"
    } always { tmuxzombie $tmuxname }
    return $?

    ## new
    true || {
        local gid="$(aac.add.py $dlurl $name)"
        until (( ${$(aac-getlen $gid):-0} > 2000000 ))
        do
            ecerr Not big enough yet
            sleep 1
        done
        mpv $name
    }
    ## old
    true || {
        # ec Somehow stderr was being suppressed this one time ...
        ecerr trying to start mpv
        retry-mpv "${out:q}"
        kill -2 -$ysid
        # mpv bug here
        # kill $!
        # kill $! is your friend :))
    }
}
function retry-mpv() {
    # retry-eval "sleep 5 ; mpv --quiet $@ |& tr '\n' ' ' |ggrep -v 'Errors when loading file'"
    retry mpv-partial "${(Q)@}"
}
function mpv-partial() {
    ecerr "$0 started ..."
    local l=''
    # --quiet 
    mpv "$@" |& {
        while read -d $'\n' -r l
        do
            color 50 200 50 "$l" >&2
            [[ "$l" =~ '(Cannot open file|Errors when loading file|Invalid NAL unit size)' ]] && { return 1 }
        done
    }
    return 0
}
