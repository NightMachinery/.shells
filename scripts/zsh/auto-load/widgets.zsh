expand-aliases() {
    unset 'functions[_expand-aliases]'
    functions[_expand-aliases]=$BUFFER
    (($+functions[_expand-aliases])) &&
        BUFFER=${functions[_expand-aliases]#$'\t'} &&
        CURSOR=$#BUFFER
}

zle -N expand-aliases
bindkey '\e^E' expand-aliases

autoload insert-unicode-char
zle -N insert-unicode-char
bindkey '^Xi' insert-unicode-char
