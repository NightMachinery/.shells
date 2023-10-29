##
alias color-force-env='local isColor_override=y ; ensure-array ugrep_opts ; local ugrep_opts=("$ugrep_opts[@]" --color=always)'
##
autoload -U colors && colors
##
typeset -ag gray=( 170 170 170 )
## Functions
# @See terminal-ansi-test for more good stuff
Bold () { ! isColorTty || print -n -- '\e[1m' }
Italic () { ! isColorTty || print -n -- '\e[3m' }
Underline () { ! isColorTty || print -n -- '\e[4m' }
Strikethrough () { ! isColorTty || print -n -- '\e[9m' }
Flash () { ! isColorTty || print -n -- '\e[5m' } # doesn't work on my iTerm
Invert () { ! isColorTty || print -n -- '\e[7m' }
Invisible () { ! isColorTty || print -n -- '\e[8m' } # again doesn't work
##
function palette {
    local i
    local -a colors
    for i in {000..255}; do
        colors+=("%F{$i}$i%f")
    done
    print -cP $colors
}
alias tui-color256-test='palette'

function paletteget {
    magic mdocu '<COLOR_CODE>' ; mret

    local color="%F{$1}"
    echo -E ${(qqqq)${(%)color}}
}
##
function color-name-to-hex {
    local name="${1}"
    assert-args name @RET

    local res
    res="$(emc-eval "(etcc--color-name-to-hex $(emc-quote "$name"))")" @TRET
    if [[ "$res" == nil ]] ; then
        ecerr "$0: returned nil"
        return 1
    fi

    ec "$res" | cat-copy-if-tty
}
##
function color-cursor {
    # [help:etcc--make-cursor-color-seq]
    #
    # * does work on tmux
    # ** try adding =set -ga terminal-overrides ',*:Ss=\E[%p1%d q:Se=\E[2 q'= to .tmux.conf to pass cursor codes, if it doesn't already work for you
    # * @issues
    # ** [[https://github.com/mobile-shell/mosh/issues/352][does not currently work on mosh]]
    ##
    local color_hex="${1:-#00000000eeee}" # blue
    if [[ "${color_hex}" != '#'* ]] ; then
        color_hex="$(color-name-to-hex "$color_hex")" @TRET
        if isOutTty ; then
            reval-ecgray pbcopy "$color_hex"
        fi
    fi

    if isColorTty ; then
        printf "\x1b]12;%s\a" "$color_hex"

        # ecgray "#funcstack: ${#funcstack}"
        if (( ${#funcstack} <= 1 )) ; then # allows us to set the next prompt's color if we invoke this function directly
            zsh_cursor_color_disable1
        fi
    fi
}
function cursor-color { color-cursor "$@" }
alias cursor-color='color-cursor' # to not increase ${#funcstack}

function colorfg() { ! isColorTty || printf "\x1b[38;2;${1:-0};${2:-0};${3:-0}m" }

function colorbg() { ! isColorTty || printf "\x1b[48;2;${1:-0};${2:-0};${3:-0}m" }

function colorb() {
    co_f=colorbg color "$@"
}

function color {
    true colorfg colorbg # whdeep hack, altly we can split on :- too
    local in inargs
    local noreset="$coNr"
    local nonewline="$coN"
    comment "Note that we need to first get stdin and then print the color, otherwise we'll print the color before anything has been outputted, resulting in race conditions."
    [[ "$1" =~ '^\d+$' ]] &&
        {
            # in="$(in-or-args "${@[4,-1]}")"
            in-or-args2 "${@[4,-1]}"
            "${co_f:-colorfg}" "$@"
        } || {
            # in="$(in-or-args "${@[2,-1]}")"
            in-or-args2 "${@[2,-1]}"
            isColorTty && printf %s "$fg[$1]"
        }
    in="$inargs"
    print -nr -- "$in"
    test -n "$noreset" || resetcolor
    test -n "$nonewline" || echo
}

function resetcolor() {
    # This var is builtin in zsh, but I guess it needs some module to be loaded
    if test -z "$reset_color" ; then
        typeset -g reset_color=$'\C-[[00m'
    fi
    ! isColorTty || printf %s "$reset_color"
}
function colorreset() { resetcolor }
##
function helloworld() {
    colorbg 0 0 255;colorfg 0 255; ec HELLO "$(colorfg 255 100)"BRAVE"$(colorfg 0 255)" $(colorbg 100 0 255)NEW$(colorbg 0 0 255) WORLD\!;resetcolor
}

function italic-test1 {
    Italic ; ecn hello world '<= ' ; Bold ; ec world ; colorreset ; ecn hello world '<= ' ; Bold ; ec world
}

function printcolors() {
    printf "\x1b[${bg};2;${red};${green};${blue}m\n"
    helloworld
    comment awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
}'
    ec 'https://github.com/johan/zsh/blob/master/Functions/Misc/colors
# Text color codes:
  30 black                  40 bg-black
  31 red                    41 bg-red
  32 green                  42 bg-green
  33 yellow                 43 bg-yellow
  34 blue                   44 bg-blue
  35 magenta                45 bg-magenta
  36 cyan                   46 bg-cyan
  37 white                  47 bg-white
# 38 iso-8316-6           # 48 bg-iso-8316-6
  39 default                49 bg-default'
}
##
function random-color() {
    randomColor.js "$@" |jq -re '.'
    # --seed "$(head -c 100 /dev/random)" 
}
function random-color-arr() {
    #shuf -i 0-255 -n 3
    # subshell doesn't change OUR seed. #ec $(($RANDOM % 256)) $(($RANDOM % 256)) $(($RANDOM % 256))
    [[ "$(randomColor.js -f rgbArray "$@")" =~ '\[(\d+),(\d+),(\d+)\]' ]] && ec "$match[@]"
}
##
function ecrainbow-n() {
    local hue="$(random-color -f hex)"
    print -nr -- "$(colorfg $(random-color-arr -l dark --hue "$hue"))$(colorbg $(random-color-arr -l light --hue "$hue"))""$@" >&2
}
function ecrainbow() { ecrainbow-n "$@" ; echo }
function ecalt1() { print -nr -- "$(colorfg 0 255 100)$(colorbg 255 255 255)${*:-EMPTY_HERE} " }
function ecalt2() { print -nr -- "$(colorfg 255 255 255)$(colorbg 0 255 100)${*:-EMPTY_HERE} " }
function h_ecalternate() {
    if ! isColorTty ; then
        ec "$(gq "$@")"
        return 0
    fi
    (($#)) || { resetcolor ; echo ; return 0 }
    ecalt1 "$1"
    shift 1
    (($#)) || { resetcolor ; echo ; return 0 }
    ecalt2 "$1"
    shift 1
    $0 "$@"
}
function ecalternate() {
    {
        local o
        o="$(h_ecalternate "$@")" @RET
        if isColorTty ; then
            # Removes last whitespace  char:
            ecn "${o[1,-7]}" ; resetcolor ; ec
        else
            ec "$o"
        fi
    } >&2
}
##
function ec-sep-h {
    ecbold $'\n''-----------'
}
alias seph='ec-sep-h'
##
