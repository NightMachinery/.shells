complete -o nospace -C /Users/evar/go/bin/bitcomplete bit # needs bashcompinit
##
function comp_wh() {
    local i j
    for i in $@ ; do
        unset out
        preEnhNames "$i"
        for j in $out[@] ; do
            compdef "$j"=which
        done
    done
}
comp_wh run-on-each re-async cee ceer whichm whdeep whdeep-words wh whh whz lesh emn ffman ffcommands rp p tldr re inargs-gen inargsE-gen agf agfi ags h_noglob_agsi fi-rec brishz bsh onlc onlm printz-quoted # realpath2
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