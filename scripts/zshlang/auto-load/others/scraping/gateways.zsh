##
function dl-dir {
    local i
    for i in $@ ; do
        bella_zsh_disable1=y reval-ec lftp-dir "$i"
        ##
        # bella_zsh_disable1=y reval-ec wget-dir "$i"
        ##
    done

    if test -z "$bella_zsh_disable1" && isI && fn-isTop dl-dir
    then
        bell-dl
    fi
}
reify dl-dir
##
