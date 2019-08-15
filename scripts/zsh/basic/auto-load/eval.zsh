alias seval='ge_ecdbg=y geval'
function geval() {
    local cmd="$@"
    test -z "$ge_ecdbg" && {
        test -z "$ge_no_ec"  && ec "$cmd"
        test -z "$ge_no_hist" && print -r -S -- "$cmd" #Add to history
    } || ecdbg "$cmd"
    eval -- "$cmd"
}
function aget() {
    doc "aget does not wait for all forked processed. Probably unsolable unless we invoke zsh -c"
    local u="$(uuidgen)"
    local erri jufile
    mkdir -p "$u" && ec "$(realpath "$u")" >> "$deleteus"
    test -e "$ag_f" && {
        cp "$ag_f" ./"$u"/
        ecdbg ag_f: "$ag_f"
    }
    cd "$u"
    test -e "$ag_f" && jufile=(./*(D))
    ecdbg jufile: "$jufile"
    eval "$@" && {
        wait
        test -n "$ag_no_rm" || {
             cd ..
            \rm -r "$u"
        }
    } || { err="$?" && ecerr aget "$@" exited "$err"; l ; cd .. ; (exit "$err") }
}
function reval() {
    ecdbg revaling "$(gq "$@")"
    eval "$(gq "$@")"
}
function nulterm() {
    reval "$@"
    ec $'\0'
}
function ruu() {
    doc helper function to expand aliases for commands like sudo, nohup, etc
    local f=()
    [[ "$1" =~ '^\s*$' ]] || f+="${=1}"
    local a="$(force-expand "$2")"
    comment @lilbug strip-left
    a="$(strip "$a" 'noglob ')"
    a="$(strip "$a" 'nocorrect ')"
    seval "$f[@]" "$=a" "$(gquote "${@:3}")"
}
function eval-dl()
{
    case "$(uname)" in
        Darwin)
            eval "$1"
            ;;
        Linux)
            eval "$2"
            ;;esac
}
function eval-darwinq()
{
    #input should be quoted.
    case "$(uname)" in
        Darwin)
            eval "${@}"
            ;;
        Linux)

            ;;esac
}
function eval-darwin()
{
    case "$(uname)" in
        Darwin)
            eval "$(gquote "$@")"
            ;;
        Linux)

            ;;esac
}
function eval-linux()
{
    case "$(uname)" in
        Darwin)

        ;;
        Linux)
            eval "$(gquote "$@")"
        ;;esac
}
function psource()
{
    if [[ -r "$1" ]]; then
        source "$1"
    fi
}
