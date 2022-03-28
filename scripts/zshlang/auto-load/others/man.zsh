##
#:  https://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized

function man-colored1 {
    local -x LESS_TERMCAP_mb="$(colorfg 100 255 0)"
    local -x LESS_TERMCAP_md="$(colorfg 10 90 20 ; Bold)"
    local -x LESS_TERMCAP_me="$(colorreset)"
    local -x LESS_TERMCAP_so="$(colorfg 255 120 0 ; colorbg 255 255 255 ; Bold)"
    local -x LESS_TERMCAP_se="$(colorreset)"
    local -x LESS_TERMCAP_us="$(colorfg 100 0 200)"
    # local -x LESS_TERMCAP_us="$(colorfg 0 100 200)"
    local -x LESS_TERMCAP_ue="$(colorreset)"

    command man "$@"
}
aliasfn man man-colored1

function man-colored2 {
    LESS_TERMCAP_mb=$(printf "\x1b[38;2;255;200;200m") \
        LESS_TERMCAP_md=$(printf "\x1b[38;2;255;100;200m") \
        LESS_TERMCAP_me=$(printf "\x1b[0m") \
        LESS_TERMCAP_so=$(printf "\x1b[38;2;60;90;90;48;2;40;40;40m") \
        LESS_TERMCAP_se=$(printf "\x1b[0m") \
        LESS_TERMCAP_us=$(printf "\x1b[38;2;150;100;200m") \
        LESS_TERMCAP_ue=$(printf "\x1b[0m") \
        command man "$@"
}

function man-colored3 {
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        command man "$@"
}
##
function mn() {
    local LESS=$LESSMIN
    export LESS

    bella_zsh_disable1

    man "$@" || lesh "$@"
}
##
function man-pdf-open {
    : "creates and opens a PDF version of the man page"

    assert isDarwin @RET

    man -t zshall | open -f -a Preview
}
##
