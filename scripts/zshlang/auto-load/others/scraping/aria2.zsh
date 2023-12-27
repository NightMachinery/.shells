##
alias ysp='y-stream-pipe'

function aa-gateway {
    aacookies "$@"
    #: $proxyenv
}
aliasfn aa aa-gateway
alias aa='\noglob aa-gateway'

alias aacert='aa --ca-certificate=/etc/ssl/certs/ca-certificates.crt'
aliasfn aa-insecure aa-gateway --check-certificate=false
aliasfn aainsecure aa-insecure
aliasfn aai aa-insecure

aliasfnq aa-multi run-on-each 'aa-gateway --conditional-get=true --allow-overwrite=true' # allows duplicate links without errors
###
function dl-named {
    local url="$1" title="$2" daemon_p="${dl_named_daemon_p}"
    title="$(ec "$title" | str2filename)" @TRET
    assert-args url title @RET

    local ext=""
    ext="$(url-extension-get "$url")" || true
    if test -n "${ext}" ; then
        title="${title}.${ext}"
    fi

    local cmd=()
    if bool "$daemon_p" ; then
        cmd=(tmuxnewsh_proxy_forward_p=y tmuxnewsh2 "$(ec "dl-named $title ($(dateshort))" | str2tmuxname)")
    fi
    cmd+=(reval-ec retry aa-gateway --conditional-get=true --allow-overwrite=true --out="${title}" "$url")

    reval-env-ec "${cmd[@]}"
}
##
function aaserver {
    mkdir -p ~/Downloads/aas/
    aria2c --rpc-secret "$ARIA_SECRET" --enable-rpc --log-level debug -l ~/Downloads/aas/aria.log -d ~/Downloads/aas/ -D
}

function aac {
    aria2p --secret "$ARIA_SECRET" "$@"
}
##
function aa-raw {
    local opts=('--stderr=true') split="${aa_split:-8}" no_split="${aaNoSplit}"
    local refer_mode="${aa_refer_mode:-url}"
    #: Redirect all console output that would be otherwise printed in stdout to stderr.  Default: false

    if bool "${aa_log_p}" ; then
        opts+=(--log='-')
        #: outputs the log to the stdout
        #: the log is by default disabled
        #: very verbose
    fi

    local top_p="${aa_top_p}"
    isI || opts+=(--show-console-readout false --summary-interval 0)
    if test -z "$top_p" && isI && @opts p [ aa- ] @ fn-isTop aa aacookies ; then
        top_p=y
    fi

    local save_invocation="${aa_si}"
    if test -z "$save_invocation" && bool "$top_p" ; then
        #: Saving the invocation all the time can create junk files when 'aa' is used by other functions, so we are only doing it without an explicit request if 'aa' is being invoked interactively.
        save_invocation=y
    fi

    if ! isColor ; then
        opts+=( --enable-color=false )
    fi

    if [[ "${@[-1]}" =~ '(^magnet:\?|\.torrent$)' ]] ; then
        ecgray "$0: torrent detected; @warn torrents no longer seem resumeable by 'aa' at least. Try 'aria2c --seed-time=0 ...'?"
        # opts=("${opts[@]}")
    else
        if ! bool "$no_split" ; then
            opts+=(--enable-http-pipelining --split "$split" --stream-piece-selector geom)
            #: @idk if the split options work for torrents or not, but it seems very unlikely, as torrents are naturally downloaded in chunks.
        fi

        opts=(--user-agent "$useragent_chrome" "${opts[@]}")
        #: --user-agent will throw an error with torrent downloads

        if [[ "${refer_mode}" == browser ]] ; then
            opts+=(--referer "$(browser-current-url)")
            ecgray "$0: --referer $(browser-current-url)"
        elif [[ "${refer_mode}" == url ]] ; then
            local arg
            for arg in $@ ; do
                if url-match "$arg" ; then
                    opts+=( --referer "$arg" )
                    break
                fi
            done
        elif bool "${refer_mode}" ; then
            ecerr "$0: unknown refer mode: '${refer_mode}'; aborting"
            return 1
        fi
    fi

    local cmd server_stats="${HOME}/.aria2_server_stats"
    cmd=(aria2c
         --connect-timeout="${aa_connect_timeout:-10}"
         --timeout="${aa_timeout:-30}"
         --seed-time=0
         --max-tries="${aa_max_tries:-0}" --retry-wait=1
         --file-allocation falloc
         --auto-file-renaming=false
         --allow-overwrite=false
         --uri-selector=adaptive
         #: If  adaptive is given, selects one of the best mirrors for the first and  reserved connections. For supplementary ones, it returns mirrors  which has not been tested yet, and if each of them has already been  tested, returns mirrors which has to be tested again. Otherwise, it  doesn't select anymore mirrors. Like feedback, it uses a performance  profile of servers.
         # --uri-selector=feedback
         #: If feedback is given, aria2 uses  download speed observed in the previous downloads and choose fastest  server in the URI list. This also effectively skips dead mirrors. The  observed download speed is a part of performance profile of servers  mentioned in --server-stat-of and --server-stat-if options.
         "--server-stat-of=${server_stats}" "--server-stat-if=${server_stats}"
         --server-stat-timeout=$((3600*24*365)) #: Specifies timeout in seconds to invalidate performance profile of the  servers since the last contact to them.
         "$opts[@]" "$@")

    if bool "$save_invocation" ; then
        invocation-save aa "${cmd[@]}"
    fi

    revaldbg "$cmd[@]"
    local ret=$?
    #-Z has some unsavory sideeffects so I have not included it in this.

    if bool "$top_p" ; then
        bell-dl
    fi

    return $ret
}

function aacookies {
    mdoc "$0 <aa-args>
Uses |theCookies| var or else feeds first URL to |cookies|." MAGIC

    aa-raw --header="$(cookies-auto "$@")" $@
}
##
function aagh() { aa "${(@f)$(gh-to-raw "$@")}" }
##
function aas() {
    : "@deprecated Use y-stream instead."

    local out="$(md5m "$1")"
    aaNoSplit=y aa "$@" --dir "$out" --on-download-complete aa-stream &
    sleep 10
    retry-mpv "'$out'/*" || kill %
}
##
function aac-getlen() {
    aac call aria2.tellStatus -P "$1" | jqm .completedLength
}
##
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

function y-stream {
    magic mdoc 'y-stream <url>
Alt: ys.py, y-stream-pipe' ; mret

    
    local out
    out=( "${(@f)$(memoi_expire=$((3600*24)) y --get-url --no-part -f best --get-filename -o "%(title)s.%(ext)s" "$@")}" ) || true
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
        reval-ec mpv-stream "$name"
    } always {
        ecgray "$0: You can stop the download with:"$'\n\t'"tmuxzombie $tmuxname"
        # reval-ec tmuxzombie $tmuxname
    }
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
##
function aaimg() {
    # do not use aa, as it will retry bad links forever
    getlinks-img "$@" | inargsf sout aria2c -Z
}
##
function aa-remotename {
    : "supports only a single URL"
    local url="${@[-1]}" rename="${aa_rename}"

    local name="${aa_out_name}"
    if test -z "$name" ; then
        name="$(url-filename "$url")"
    fi

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

function aa-hash-name {
    : "supports only a single URL"
    local url="${@[-1]}"

    local name="${aa_out_name}"
    if test -z "$name" ; then
        name="$(url-filename "$url")"
    fi

    local hash
    hash="$(md5m "$url")" @TRET

    if test -n "$name" ; then
        name="${name:r}_${hash}.${name:e}"
    else
        name="$hash"
    fi

    aa-gateway --conditional-get=true --out="$name" "$@" @RET
}
@opts-setprefix aa-hash-name aa
##
function aa-2stdout {
    fhMode=aa full-html2 "$@"
}

function aa-2dest {
    fhMode=curl full-html "$@"
}
##
function aa2tlg-book {
    local aaMark="$(uuidm)"
    aaMark="$aaMark" aa_helper_e='tsendf-book' aa-gateway --on-download-complete aa-helper-gen.zsh "$@"
    till_file_del_p=y till-file "$aaMark"
}
renog aa2tlg-book
##
