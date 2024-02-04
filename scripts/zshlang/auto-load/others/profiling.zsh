##
aliasfn time2 time_engine=reval-env time2-unquoted
function time2-unquoted {
    local cmd=("$@") engine=("${time_engine[@]:-eval}")
    test -z "$cmd[*]" && return 0
    local time2_start=$EPOCHREALTIME

    {
        "$engine[@]" "$cmd[@]"
        # (eval "$cmd") # subshell protects against premature exits because of, e.g., glob failure

        local ret=$?
        return $ret
    } always {
        ecbold $'\n\n'"Took $(( EPOCHREALTIME - time2_start ))" >&2
    }
}
@opts-setprefix time2-unquoted time
aliasfn time2-uq time2-unquoted

function p-time() {
    time2-unquoted "$(pbpaste)"
}
##
aliasfn hfd hyperfine_shell=dash hf-gen
aliasfn hfz hyperfine_shell=brishz_para.dash hf-gen
function hf-gen() {
    local shell="${hyperfine_shell:-bash}"
    local w="${hyperfine_w:-5}"

    hyperfine --shell "$shell" --warmup $w --export-markdown=$HOME/tmp/hyperfine.md "$@"
    # [FR] Allow caching of the shell startup time https://github.com/sharkdp/hyperfine/issues/378
}
@opts-setprefix hf-gen hyperfine
##
