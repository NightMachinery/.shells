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
