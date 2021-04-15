function cp2eva() {
    local inargs=''
    in-or-args2
    local tmp
    tmp="$(gmktemp)" || ectrace @RET
    {
        ecn "${inargs[*]}" > $tmp @RET

        assert scp -C "$tmp" eva@82.102.11.148:tmp/.remote_cp @RET
        # -C      Compression
    } always {
        silent trs-rm "$tmp"
    }
}
function pbpaste-remote() {
    cat ~/tmp/.remote_cp @RET
}
aliasfn popr pbpaste-remote
##
