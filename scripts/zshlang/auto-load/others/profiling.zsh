function time2() {
    local cmd="$(gquote "$@")"
    time2-unquoted "$cmd"
}
function time2-unquoted() {
    local cmd="$*"
    test -z "$*" && return 0
    local time2_start=$EPOCHREALTIME

    eval "$cmd"
    # (eval "$cmd") # subshell protects against premature exits because of, e.g., glob failure

    ec $'\n\n'"Took $(( EPOCHREALTIME - time2_start ))"
}
aliasfn time2-uq time2-unquoted
function time-p() {
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
