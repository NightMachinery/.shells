## external sources:
# * https://github.com/zsh-users/zsh-completions/tree/master/src
##
complete -o nospace -C /Users/evar/go/bin/bitcomplete bit # needs bashcompinit
##
# Just reuse eval's engine
# function comp-reval() {
#     if (( $#words <= 2 )) ; then
#         _which "$@"
#         return $?
#     else
#         local subcommand=$words[2]
#         local words=("${(@)words[2,-1]}")
#         ((CURRENT = CURRENT -2))
#         local compfun="_${subcommand}"
#         (( $+functions[$compfun] )) || compfun=_files
#         service="$subcommand" $compfun
#         local ret=$?
#         return $ret
#     fi
# }
##

function comp-wh-set() {
    comp-set '=which' "$@"
}
function comp-set() {
    (( $#@ >= 1 )) || {
        ectrace "Not enough args"
        return 1
    }
    local engine="$1" ; shift

    local i j
    for i in $@ ; do
        unset out
        preEnhNames "$i"
        for j in $out[@] ; do
            if [[ "$engine" == '='* ]] ; then
                compdef "$j""$engine"
            else
                compdef "$j" "$engine"
            fi
        done
    done
}
comp-wh-set cee ceer whichm whdeep whdeep-words wh whh whz lesh emn ffman ffcommands rp p tldr agf agfi ags h_noglob_agsi # realpath2

comp-set '=eval' ruu reval rgeval revaldbg reval-ec reval-copy seval geval eval-memoi memoi-eval eval-timeout reval-timeout fi-rec assert hyperfine hfd hfz para parad brishz bsh onlc onlm printz-quoted run-on-each re-async redo inargs-gen inargsE-gen filterE-gen filter filter0 p pope pz pf px proxychains4 bell-auto bell-repeat time2
##
rexa "compdef _=ls" pbadd mv # mv had a bug I think?
rexa "compdef _=man" mn
((${+commands[rclone]})) && silent rexa "compdef _=rclone" rcr
# rexa "compdef _=xargs"
##
# isExpensive && {
#     [[ -e $asdf_dir ]] && . $asdf_dir/etc/bash_completion.d/asdf.bash
# }
##
function _indir() {
    # BEWARE: This completion function has a rather creepy eval in it.

    ## Special vars:
    #  words: 1:function 2:directory 3:pattern
    #  CURRENT: number of words
    #  curcontext (idk)
    #  service: seemingly name of first arg?

    local diract=$words[2]
    # dbg dvar diract
    local dirdir="$(eval print -nr -- $diract)"
    local subcommand=$words[3]

    # dbg re dvar words CURRENT dirdir diract

    local words=( "${(@)words[3,-1]}" )
    ((CURRENT = CURRENT -2))

    # ec -------
    # dbg re dvar words CURRENT dirdir diract

    local OLDPWD=$PWD
    local PWD=$dirdir

    local compfun="_${subcommand}"
    (( $+functions[$compfun] )) || compfun=_files
    cdz $PWD || return 0 # returning non-zero causes some retries which we don't want.
    {
        service="$subcommand" $compfun
        local ret=$?
    } always {
        cd $OLDPWD
    }
    return $ret

}
compdef _indir indir
## foo has grouped completions:
_foo() {
    A=( -h --help foo bar baz )
    compadd -J group1 -X expl1 -x msg1 -a A
    B=( clown-fish hippo )
    compadd -J group2 -X expl2 -x msg2 -a B
}
function foo { echo $* }
compdef _foo foo
##
kitty + complete setup zsh | source /dev/stdin
##
