re autoload add-zsh-hook expand-or-complete insert-unicode-char

expand-aliases-widget() {
    local expanded
    expanded="$(expand-aliases $BUFFER)" &&
        BUFFER=${expanded} &&
        CURSOR=$#BUFFER
}

zle -N expand-aliases-widget
bindkey '\e^E' expand-aliases-widget

zle -N insert-unicode-char
bindkey '^Xi' insert-unicode-char
