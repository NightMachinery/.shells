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
function color-hex-to-rgb {
    : "prints the decimal R G B of a #rrggbb (or #rgb) colour, space separated"
    #: The direction [agfi:color-name-to-hex] does not go, for the truecolor
    #: escapes of [agfi:colorfg] and [agfi:colorbg], which want numbers.
    ##
    local hex="${1#\#}"
    assert-args hex @RET

    if (( ${#hex} == 3 )) ; then
        #: `#abc' is shorthand for `#aabbcc'.
        hex="${hex[1]}${hex[1]}${hex[2]}${hex[2]}${hex[3]}${hex[3]}"
    fi

    #: Spelled out rather than with a repeat count, which would need
    #: EXTENDED_GLOB to be on in the caller.
    if [[ "${hex}" != [0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F] ]] ; then
        ecerr "$0: not a hex colour: ${1}"
        return 1
    fi

    printf '%d %d %d\n' "0x${hex[1,2]}" "0x${hex[3,4]}" "0x${hex[5,6]}"
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

#: Candidate background washes, for telling one terminal window or tmux pane
#: apart from another without making it harder to read.
#:
#: The `solar-' entries are computed to sit at exactly the CIELAB lightness of
#: Solarized Light's background, base3 `#fdf6e3' (L* 97), and differ from it
#: only in hue. That is the property worth having: contrast against the
#: theme's own text is left alone. Solarized body text (base00, `#657b83')
#: measures 4.13 against base3 and between 4.12 and 4.14 against every one of
#: these, so none of them costs any legibility. Violet is the most
#: recognisable per unit of colour, base3's hue being a warm yellow and violet
#: its opposite; cyan and green are gentler still.
#:
#: The entries without the prefix are ordinary pale washes that predate the
#: measurement, kept so the two can be compared.
typeset -gA color_background_palette=(
    solar-violet-faint  '#f7f5fd'
    solar-violet        '#f8f5ff'
    solar-violet-deep   '#f9f4ff'
    solar-lilac-faint   '#f5f6fe'
    solar-lilac         '#f4f6ff'
    solar-cyan-faint    '#edf9f9'
    solar-cyan          '#e6fafb'
    solar-teal-faint    '#eef9f6'
    solar-green-faint   '#f2f8f1'
    solar-green         '#eff9ed'
    solar-slate-faint   '#f0f7fd'
    solar-slate         '#ebf8ff'
    purple-faint        '#faf8fc'
    purple              '#f6f2f8'
    green-faint         '#f8fbf7'
    rose                '#fdf4f7'
    amber               '#fff6ec'
)
#: An associative array has no order of its own, and these want reading
#: faintest first within each hue.
typeset -ga color_background_palette_order=(
    solar-violet-faint solar-violet solar-violet-deep
    solar-lilac-faint solar-lilac
    solar-cyan-faint solar-cyan solar-teal-faint
    solar-green-faint solar-green
    solar-slate-faint solar-slate
    purple-faint purple green-faint rose amber
)

function h-color-tty-write {
    : "writes to the controlling terminal, falling back to stdout"
    #: Run as `! color-background ...' inside an agent session, our stdout is a
    #: pipe the agent reads, so an escape written there is captured and shown
    #: as text instead of reaching the terminal. The controlling terminal is
    #: still the pane, and stays writable even when stdout is not a tty.
    ##
    local text="${1}"

    if test -w /dev/tty ; then
        printf '%s' "${text}" > /dev/tty
    else
        printf '%s' "${text}"
    fi
}

function color-background {
    : "sets the terminal background: a name from =color_background_palette=, or a #rrggbb"
    #: OSC 11. Under tmux 3.0 and later this applies to the sending pane alone,
    #: so a sibling pane is untouched; in a bare terminal window it applies to
    #: the window. Cells an application paints with a background of their own
    #: are unaffected, so it reads as a tint rather than a repaint.
    #: [agfi:color-background-reset] undoes it. With no argument it shows the
    #: palette instead. Silent, like [agfi:color-cursor]: the terminal changing
    #: colour is the feedback.
    ##
    local want="${1}"
    if test -z "${want}" ; then
        color-background-palette
        return 0
    fi

    local hex="${color_background_palette[$want]:-${want}}"

    #: Round-tripped through [agfi:color-hex-to-rgb], which both validates the
    #: colour and expands a `#abc' shorthand: terminals are not obliged to
    #: understand the short form in an OSC.
    local -a rgb
    if ! rgb=(${=$(color-hex-to-rgb "${hex}" 2>/dev/null)}) || (( ${#rgb} != 3 )) ; then
        ecerr "$0: not a palette name or a hex colour: ${want}"
        color-background-palette >&2
        return 1
    fi
    hex="$(printf '#%02x%02x%02x' "${rgb[@]}")"

    h-color-tty-write $'\e]11;'"${hex}"$'\a'
}
#: `aliasfn' is not defined yet this early in the load order, so these follow
#: the plain-wrapper style [agfi:cursor-color] already uses in this file.
function background-color { color-background "$@" }

function color-background-reset {
    : "restores the terminal's configured background, undoing [agfi:color-background]"
    #: OSC 111.
    h-color-tty-write $'\e]111;\a'
}
function background-color-reset { color-background-reset "$@" }

function color-background-palette {
    : "shows the candidate backgrounds as swatches, marking <hex> if one is given"
    #: Straight to the terminal for the reason [agfi:h-color-tty-write] gives:
    #: captured, a swatch is just an escape code in a transcript.
    ##
    if test -w /dev/tty ; then
        h-color-background-palette-body "$@" > /dev/tty
    else
        h-color-background-palette-body "$@"
    fi
}

function h-color-background-palette-body {
    local mark="${1}"

    local name hex marker
    local -a rgb
    for name in "${color_background_palette_order[@]}" ; do
        hex="${color_background_palette[$name]}"
        rgb=(${=$(color-hex-to-rgb "${hex}")}) || continue

        #: Dark text over the wash, because the question being asked of a
        #: colour this pale is whether it is comfortable to read on.
        colorbg "${rgb[@]}"
        colorfg 51 51 51
        printf '  The quick brown fox jumps over the lazy dog  '
        resetcolor

        marker=''
        [[ -n "${mark}" && "${hex}" == "${mark}" ]] && marker='  <- in use'
        printf ' %-20s %s%s\n' "${name}" "${hex}" "${marker}"
    done
}
##
function colorfg {
    if isColor && true-color-p ; then
        printf "\x1b[38;2;${1:-0};${2:-0};${3:-0}m"
    fi
}

function colorbg {
    if isColor && true-color-p ; then
        printf "\x1b[48;2;${1:-0};${2:-0};${3:-0}m"
    fi
}

function colorb {
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

function resetcolor {
    #: This var is builtin in zsh, but I guess it needs some module to be loaded
    if test -z "$reset_color" ; then
        typeset -g reset_color=$'\C-[[00m'
    fi

    # if true; then
    if isColor ; then
        #: =resetcolor= is almost always activated from subshells, so we can't check for a color tty.
        printf %s "$reset_color"
    fi
}
function colorreset { resetcolor }
##
function helloworld {
    colorbg 0 0 255;colorfg 0 255; ec HELLO "$(colorfg 255 100)"BRAVE"$(colorfg 0 255)" $(colorbg 100 0 255)NEW$(colorbg 0 0 255) WORLD\!;resetcolor
}

function italic-test1 {
    Italic ; ecn hello world '<= ' ; Bold ; ec world ; colorreset ; ecn hello world '<= ' ; Bold ; ec world
}

function printcolors {
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
function ecrainbow-n {
    local hue="$(random-color -f hex)"
    print -nr -- "$(colorfg $(random-color-arr -l dark --hue "$hue"))$(colorbg $(random-color-arr -l light --hue "$hue"))""$@" >&2
}
function ecrainbow { ecrainbow-n "$@" ; echo }

function ecalt1 { print -nr -- "$(colorfg 0 255 100)$(colorbg 255 255 255)${*:-EMPTY_HERE} " }

function ecalt2 { print -nr -- "$(colorfg 255 255 255)$(colorbg 0 255 100)${*:-EMPTY_HERE} " }

function h_ecalternate {
    if ! isColor ; then
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

function ecalternate {
    {
        if isColorTty ; then
            local o
            o="$(h_ecalternate "$@")" @RET

            ##
            #: Removes last whitespace char:
            # ecn "${o[1,-2]}" ; resetcolor ; ec
            #: This doesn't work, as there are ANSI color codes in the string.
            ##
            ecn "${o}" ; resetcolor ; ec
        else
            ec "$@"
        fi
    } >&2
}
alias ecalt='ecalternate'
##
function ec-sep-h {
    ecbold $'\n''-----------'
}
alias seph='ec-sep-h'
##
