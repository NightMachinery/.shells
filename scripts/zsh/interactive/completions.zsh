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
rexa "compdef _=ls" pbadd mv # mv had a bug I think?
rexa "compdef _=man" mn
rexa "compdef _=rclone" rcr
# rexa "compdef _=xargs"
isExpensive && {
    [[ -e $asdf_dir ]] && . $asdf_dir/etc/bash_completion.d/asdf.bash
}
