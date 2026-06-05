## Array/text output helpers.
function arr0() {
    print -nr -- "${(pj.\0.)@}"
}

function arrN() {
    print -nr -- "${(pj.\n.)@}"
}
alias arrn='arrN'
function arrNN() { print -r -- "${(pj.\n.)@}" }
alias arrnn='arrNN'
