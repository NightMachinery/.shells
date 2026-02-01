##
function pbcopy-lilf {
    local inargs=''
    in-or-args2 "$@"
    local tmp
    tmp="$(gmktemp)" || ectrace @RET
    {
        ecn "${inargs[*]}" > $tmp @RET

        assert scp -C "$tmp" ${lilf_user}@${lilf_ip}:tmp/.remote_cp @RET
        # -C      Compression
    } always {
        silent trs-rm "$tmp"
    }
}

function pbpaste-lilf {
    assert scp -C ${lilf_user}@${lilf_ip}:tmp/.remote_cp ~/tmp/.remote_cp @RET

    pbpaste-remote
}

function pbpaste-remote {
    cat ~/tmp/.remote_cp @RET
}
aliasfn popr pbpaste-remote
##
function clipboard-remote-listen {
    local port="${1:-6070}"

    tmuxnew "clipboard-listen-${port}" \
        tcp_to_process -u --bind=127.0.0.1 --port=${port} MAGIC_COPY
        # tcp_to_process -u --bind=127.0.0.1 --port=${port} brishz_in=MAGIC_READ_STDIN brishzq.zsh h-clipboard-remote-listen
        # socat -d -d -u "TCP-LISTEN:${port},bind=127.0.0.1,fork" 'EXEC:env brishz_in=MAGIC_READ_STDIN brishzq.zsh h-clipboard-remote-listen'
    # 'EXEC:zsh -fc \"tee >(pbcopy)\"'
}

function h-clipboard-remote-listen {
    ensure-dir ~/logs/

    {
        # ecgray 'clipboard received input'

        local isColor_override=y

        local in
        # ecgray 'clipboard: loading stdin'
        in="${$(</dev/stdin ; print -n .)[1,-2]}"
        # ecgray 'clipboard: finished loading'

        {
            ec-sep-h
            ecn "$in"
            ec-sep-h
        } >&2

        if [[ "$in" =~ '^MAGIC_BELL_(bell\S?-?\S*)$' ]] ; then
            bell_name="${match[1]}"

            if isdefined "$bell_name" ; then #: @securityRisk9 @abundanceOfCaution
                reval "${bell_name}"
            fi
        else
            ecn "$in" |
                pbcopy
        fi
    } |& tee -a ~/logs/"clipboard_listen"
}
##
pbcopy-remote() {
    #: immediately and verbosely fails if nobody's listening on the port
    #: `echo test | copy_port=18023 pbcopy-remote`
    ##
    local port="${copy_port:-6030}"

    socat - "tcp:127.0.0.1:${port}"
}
alias pcr="pbcopy-remote"

pbcopy-remote-from-local() {
    pbpaste | pbcopy-remote
}

bell-call-remote() {
    local bell_name="${1}"
    local port="${bell_port:-6030}"

    if isLocal ; then
        reval "${bell_name}"
    else
        echo "MAGIC_BELL_${bell_name}" |
            copy_port="$port" pbcopy-remote
    fi
}
##
