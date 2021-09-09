##
function inplace-io {
    # inplace-input-output:
    #   the input command should accept '<input> <output>'
    ##
    local cmd=("${=1}") inputs=(${@[2,-1]})

    local i
    for i in $inputs[@] ; do
        assert test -e "$i" @RET

        local o
        o="$(gmktemp --suffix ".${i:e}")" @TRET

        reval-ec "$cmd[@]" "$i" "$o" @RET
        command gmv -v "$o" "$i" @RET
    done
}
##
