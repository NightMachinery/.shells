rexa "compdef _=which" cee ceer whichm whdeep whdeep-words wh whh whz lesh mn emn ffman ffcommands rp p f fr tldr re
rexa "compdef _=ls" pbcopy mv
rexa "compdef _=xargs" f fr
isExpensive && {
    [[ -e $asdf_dir ]] && . $asdf_dir/etc/bash_completion.d/asdf.bash
}
