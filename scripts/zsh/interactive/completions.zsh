rexa "compdef _=which" cee ceer whichm whdeep whdeep-words wh whh whz lesh emn ffman ffcommands rp p f fr tldr re inargs-gen inargsE-gen
rexa "compdef _=ls" pbadd mv # mv had a bug I think?
rexa "compdef _=man" mn
# rexa "compdef _=xargs"
isExpensive && {
    [[ -e $asdf_dir ]] && . $asdf_dir/etc/bash_completion.d/asdf.bash
}
