function jupytergarden-p {
    # @todo1 do sth more intelligent
    ##
    if isMBP ; then
        ec y
    else
        ec n
    fi
}
##
function jg-eval-json {
    jg_eval_sh_json_output=y jg_eval.sh "$@"
}
@opts-setprefix jg-eval-json jg_eval_sh

function jg-eval {
    local res
    res="$(jg-eval-json "$@")" @TRET
    ec $res | jqm .out
    ec $res | jqm .err >&2
    local retcode
    retcode="$(ec $res | jqm .retcode)" || retcode=991009
    return "$retcode"
}
@opts-setprefix jg-eval jg_eval_sh
##
