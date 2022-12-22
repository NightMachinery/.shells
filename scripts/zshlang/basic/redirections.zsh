alias silent='&>/dev/null'

function silence() {
    { eval "$(gquote "$@")"  } &> /dev/null
}

sout() {
    { eval "$(gquote "$@")" } > /dev/null
}

serr() {
    { eval "$(gquote "$@")" } 2> /dev/null
}

alias nisout='nig sout'
alias niserr='nig serr'
alias nis='nig silence'
