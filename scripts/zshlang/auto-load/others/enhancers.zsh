##
function inplace-io {
    # * modes
    # ** last_i_o: The input command should accept '<input> <output>'.
    ##
    local cmd=("${=1}") inputs=(${@[2,-1]})
    local mode="${inplace_io_m:-last_i_o}"
    local verbose="${inplace_io_v}"

    local reval_cmd='reval'
    if bool "$verbose" ; then
        reval_cmd='reval-ec'
    fi
    cmd=( "$reval_cmd" "$cmd[@]" )

    local i
    for i in $inputs[@] ; do
        local o
        o="$(gmktemp --suffix ".${i:e}")" @TRET

        if bool "$verbose" ; then
            ecgray "$0: output file: $(gq "$o")"
        fi
        if [[ "$mode" == 'last_i_o' ]] ; then
            "$cmd[@]" "$i" "$o" @RET
        elif [[ "$mode" == 'last_i_stdout' ]] ; then
            "$cmd[@]" "$i" > "$o" @RET
        elif [[ "$mode" == 'last_stdin_stdout' ]] ; then
            cat "$i" | "$cmd[@]" > "$o" @RET
        else
            ecerr "$0: unknown mode, aborting"

            silent trs-rm "$0"

            return 1
        fi

        local mv_opts=()
        if bool "$verbose" ; then
            mv_opts+='-v'
        fi
        command gmv "${mv_opts[@]}" "$o" "$i" @RET
    done
}
##
