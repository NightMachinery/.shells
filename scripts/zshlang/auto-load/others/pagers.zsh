##
export GIT_PAGER="delta --light --syntax-theme 'Solarized (light)' --line-numbers"
if isLocal ; then
    GIT_PAGER+=" --hyperlinks"
    #: [jalali:1404/05/27/23:34] somehow doesn't work on SSH on eva
fi

function diff-colorer {
    command delta --light --syntax-theme 'Solarized (light)' "$@"
    # --true-color=always
}
aliasfn git-pager diff-colorer
##
export LESSMIN='-RiF --mouse --wheel-lines=3 -j.3'
# F: --quit-if-one-screen ; R: maintain the ANSI colour sequences; i: smartcase searches (all lower=ignore case);  -N or --LINE-NUMBERS Causes a line number to be displayed at the beginning of each line in the display.
#
# -j.5 causes search matches to be centered at the specified fraction of the screen

alias lmin='LESS=$LESSMIN '
function less-min {
    LESS=$LESSMIN less "$@"
}

## @personal :
export LESS="${LESSMIN}" # -N for page numbers (this messes up word-wrapping)
isSSH && LESS="-RiF"
export PAGER="$commands[less]"
##
function jqless() {
    jq --color-output | pager-if-tty
}
alias jql=jqless
##
aliasfn pager less

function pager-if-tty {
    if isOutTty ; then
        pager
    else
        cat
    fi
}

function pager-if-overflow {
    #: Pages only when the content would not fit on the screen with =pager_overflow_margin= lines to spare (for the shell prompt drawn afterwards); otherwise prints it directly.
    #: Unlike =less --quit-if-one-screen=, which tests against the full screen height and so lets the prompt push the top of an almost-screenful out of view.
    #: (No mainstream pager supports such a margin; apps solve this client-side, e.g., psql's =pager_min_lines=.)
    local margin="${pager_overflow_margin:-2}"

    if ! isOutTty ; then
        cat
        return $?
    fi

    local content
    content="$(cat)"

    local lines
    lines="$(ec "$content" | text-wrap "$(terminal-width-get)" | wc -l | trim)" @TRET
    if (( lines > $(terminal-height-get) - margin )) ; then
        #: =-+F= unsets =--quit-if-one-screen= from =$LESS=, as we have already decided to page.
        ec "$content" | pager -+F
    else
        ec "$content"
    fi
}

function pager-if-overflow-streaming {
    #: [agfi:pager-if-overflow], but it does not wait for EOF to show anything. Same knob, same paging decision.
    #: For producers that emit their output in sections over time -- [agfi:agent-status] runs three reports under =parallelm --keep-order=, so the first section is ready seconds before the last. The plain version slurps everything first, and so shows a blank screen for as long as the slowest section takes.
    #: What it costs: when the content turns out *not* to fit, the lines already on the screen are erased before the pager opens. That is a redraw the plain version never does, and it assumes a terminal that honours cursor movement -- so this is the variant you ask for, not the default.
    local margin="${pager_overflow_margin:-2}"

    if ! isOutTty ; then
        cat
        return $?
    fi

    setopt localoptions extendedglob

    local width height
    width="$(terminal-width-get)" @TRET
    height="$(terminal-height-get)" @TRET
    local -i limit=$(( height - margin ))

    local -a buf=() pending=()
    local line item visible overflow_p=''
    #: =count=: wrapped lines seen so far. =printed=: how many of them actually reached the screen, i.e. what we would have to erase. =blanks=: empty lines held back, see below.
    local -i count=0 printed=0 blanks=0 i=0

    #: =|| test -n "$line"=: a last line without a trailing newline leaves =read= failing with the content still in =$line=. It cannot loop forever, because the next =read= empties =$line= before failing again.
    while IFS= read -r line || test -n "$line" ; do
        if test -z "$line" ; then
            #: Held back rather than emitted, so that *trailing* blank lines are dropped -- which is what [agfi:pager-if-overflow]'s =content="$(cat)"= does for free, and what [agfi:agent-status] relies on to end without a gap. A blank line in the middle is released by the next non-empty one.
            blanks+=1
            continue
        fi

        pending=()
        for (( i = 0 ; i < blanks ; i++ )) ; do
            pending+=('')
        done
        blanks=0
        pending+=("$line")

        for item in "${pending[@]}" ; do
            buf+=("$item")

            if test -n "$overflow_p" ; then
                #: Past the threshold we only collect; the pager gets all of it.
                continue
            fi

            #: Measured on the *visible* width, the way [agfi:text-wrap] would: CSI sequences (colours, and whatever else an =--color= producer emits) occupy no columns, and =${(m)#...}= counts a double-width character as the two columns it takes. OSC sequences (e.g. =delta --hyperlinks=) are not stripped, so a hyperlinked line over-counts -- erring towards paging, which is the safe side.
            visible="${item//$'\e'\[[0-9;?]#[a-zA-Z]/}"
            count+=$(( ${(m)#visible} == 0 ? 1 : (${(m)#visible} + width - 1) / width ))

            if (( count > limit )) ; then
                overflow_p=y
            else
                ec "$item"
                printed=count
            fi
        done
    done

    if test -z "$overflow_p" ; then
        #: It fit, and is already on the screen.
        return 0
    fi

    if (( printed > 0 )) ; then
        #: Erase what we streamed, so that the screen looks as though nothing had been printed: cursor up =printed= rows, then clear to the end of the screen. Guarded because =CUU 0= is not a no-op -- it moves one row, exactly like =CUU 1=.
        #: Raw CSI rather than =tput cuu=/=tput ed=, to save two forks on an interactive path.
        #: We stop printing at =limit=, so the erased region is always smaller than the screen. If the prompt sat near the bottom the printing will still have scrolled it, but the move is relative to the cursor, which is =printed= rows below the first line we wrote either way.
        printf '\e[%dA\e[J' "$printed"
    fi

    #: =-+F= unsets =--quit-if-one-screen= from =$LESS=, as we have already decided to page.
    ec "${(F)buf}" | pager -+F
}
##
