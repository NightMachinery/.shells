re autoload add-zsh-hook expand-or-complete insert-unicode-char

expand-aliases() {
    unset 'functions[_expand-aliases]'
    functions[_expand-aliases]=$BUFFER
    (($+functions[_expand-aliases])) &&
        BUFFER=${functions[_expand-aliases]#$'\t'} &&
        CURSOR=$#BUFFER
}

zle -N expand-aliases
bindkey '\e^E' expand-aliases

zle -N insert-unicode-char
bindkey '^Xi' insert-unicode-char
