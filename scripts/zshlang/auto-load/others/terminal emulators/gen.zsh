function terminal-unsupported() {
    if isTmux ; then
        # I don't know what else to do except ignoring this
        return 1
    fi
    ectrace "Unsupported terminal emulator"
    return 1
}
##
function terminal-activate-tab() {
    if isSSH ; then
        return 0
    fi

    if isKitty ; then
        kitty-tab-activate "$@"
    elif isiTerm ; then
        iterm-tab-activate "$@"
    else
      terminal-unsupported
    fi
}
function terminal-session-is-focused() {
    if isKitty ; then
        kitty-tab-is-focused "$@"
    elif isiTerm ; then
        iterm-session-is-active "$@"
    else
      terminal-unsupported
    fi
}
function terminal-is-focused() {
    if isKitty ; then
        kitty-is-focused
    elif isiTerm ; then
        iterm-focus-is "$@"
    else
      terminal-unsupported
    fi
}
##
function terminal-ansi-test() {
    echo -e '\e[1mbold\e[22m'
    echo -e '\e[2mdim\e[22m'
    echo -e '\e[3mitalic\e[23m'
    echo -e '\e[4munderline\e[24m'
    echo -e '\e[4:1mthis is also underline (new in 0.52)\e[4:0m'
    echo -e '\e[21mdouble underline (new in 0.52)\e[24m'
    echo -e '\e[4:2mthis is also double underline (new in 0.52)\e[4:0m'
    echo -e '\e[4:3mcurly underline (new in 0.52)\e[4:0m'
    echo -e '\e[5mblink (new in 0.52)\e[25m'
    echo -e '\e[7mreverse\e[27m'
    echo -e '\e[8minvisible\e[28m <- invisible (but copy-pasteable)'
    echo -e '\e[9mstrikethrough\e[29m'
    echo -e '\e[53moverline (new in 0.52)\e[55m'

    echo -e '\e[31mred\e[39m'
    echo -e '\e[91mbright red\e[39m'
    echo -e '\e[38:5:42m256-color, de jure standard (ITU-T T.416)\e[39m'
    echo -e '\e[38;5;42m256-color, de facto standard (commonly used)\e[39m'
    echo -e '\e[38:2::240:143:104mtruecolor, de jure standard (ITU-T T.416) (new in 0.52)\e[39m'
    echo -e '\e[38:2:240:143:104mtruecolor, rarely used incorrect format (might be removed at some point)\e[39m'
    echo -e '\e[38;2;240;143;104mtruecolor, de facto standard (commonly used)\e[39m'

    echo -e '\e[46mcyan background\e[49m'
    echo -e '\e[106mbright cyan background\e[49m'
    echo -e '\e[48:5:42m256-color background, de jure standard (ITU-T T.416)\e[49m'
    echo -e '\e[48;5;42m256-color background, de facto standard (commonly used)\e[49m'
    echo -e '\e[48:2::240:143:104mtruecolor background, de jure standard (ITU-T T.416) (new in 0.52)\e[49m'
    echo -e '\e[48:2:240:143:104mtruecolor background, rarely used incorrect format (might be removed at some point)\e[49m'
    echo -e '\e[48;2;240;143;104mtruecolor background, de facto standard (commonly used)\e[49m'

    echo -e '\e[21m\e[58:5:42m256-color underline (new in 0.52)\e[59m\e[24m'
    echo -e '\e[21m\e[58;5;42m256-color underline (new in 0.52)\e[59m\e[24m'
    echo -e '\e[4:3m\e[58:2::240:143:104mtruecolor underline (new in 0.52) (*)\e[59m\e[4:0m'
    echo -e '\e[4:3m\e[58:2:240:143:104mtruecolor underline (new in 0.52) (might be removed at some point) (*)\e[59m\e[4:0m'
    echo -e '\e[4:3m\e[58;2;240;143;104mtruecolor underline (new in 0.52) (*)\e[59m\e[4:0m'
}

##
function tty-link() {
    local link="$1"
    local desc="${2:-${link:t}}"

    if test -e "$link" ; then
        link="file://$(hostname)$(grealpath "$link")"
    fi

    printf '\e]8;;%s\e\\%s\e]8;;\e\\\n' "$link" "$desc"
    # https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
    ## tests:
    # `tty-link 'https://www.bing.com' bing`
    # `tty-link ~/tmp "Temp dir"`
    ##
}
##
