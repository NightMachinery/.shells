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
    local margin="${pager_overflow_margin:-5}"

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
##
