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
comp_wh cee ceer whichm whdeep whdeep-words wh whh whz lesh emn ffman ffcommands rp p tldr re inargs-gen inargsE-gen agf agfi ags agsi fi-rec
##
rexa "compdef _=ls" pbadd mv # mv had a bug I think?
rexa "compdef _=man" mn
rexa "compdef _=rclone" rcr
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
    cd $PWD
    {
        service="$subcommand" $compfun
        local ret=$?
    } always {
        cd $OLDPWD
    }
    return $ret

}
compdef _indir indir
##
