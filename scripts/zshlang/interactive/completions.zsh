## external sources:
# [[id:cf763f59-db43-4712-b186-bd4143833092][libraries/completion.org]]
##
# 'bit is an experimental modernized git CLI built on top of git' https://github.com/chriswalz/bit
complete -o nospace -C /Users/evar/go/bin/bitcomplete bit # needs bashcompinit
##
# this generates generic completions from the manpage, but it is incomplete, e.g., `--check-certificate` is not detected for aria2c
compdef _gnu_generic aria2c
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
comp-wh-set cee ceer whichm whdeep whdeep-words wh whh whz lesh emn ffman ffcommands rp tldr agf agfi agfi-ni ags h_noglob_agsi pxaify-fn pxaify-command pxify-command # realpath2

comp-set '=eval' ruu reval reval-to reval-to-gpt3t reval-to-gpt4t reval-to-gpt4 rgeval revaldbg loop reval-ec reval-copy seval geval eval-memoi memoi-eval eval-timeout reval-timeout reval-2json reval-retcode fi-rec assert hyperfine hfd hfz para parad brishz bsh onlc onlm run-on-each re-async redo inargs-gen inargsE-gen filterE-gen filter filter0 p enh-addfinder pf pope clipboard-add-quoted pcz printz-quoted pf px proxychains4 bell-auto bell-repeat time2 silence serr sout sdbgerr soutdbg soutdbg serrdbg mn lesh inplace-io jcolor jah jahun ansi2img withemc1 withemcgui openai-complete-with-prompt
# @todo8 '@opts' needs a custom completor that feeds the items after '@' to the evil completor
##
comp-set '=rsync' rsp-safe rsp-dl
##
rexa "compdef _=ls" pbadd mv # mv had a bug I think?
# rexa "compdef _=man" mn # @alt =eval
((${+commands[rclone]})) && silent rexa "compdef _=rclone" rcr
# rexa "compdef _=xargs"
##
# isExpensive && {
#     [[ -e $asdf_dir ]] && . $asdf_dir/etc/bash_completion.d/asdf.bash
# }
##
silent comp-set '=brew' bii # might not be present on Linux
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
if test -n "${commands[kitty]}" ; then
    kitty + complete setup zsh | source /dev/stdin
fi
##
function _words {
    # @bug "$words[-1]" always selects the last arg, but we want the current arg instead

    _values 'words' "${(f@)$(cat "$WORDLIST0" | rg "$words[-1]")}"
}
compdef _words sp spi ffdict ffdict-wn sdc di dwn
##
