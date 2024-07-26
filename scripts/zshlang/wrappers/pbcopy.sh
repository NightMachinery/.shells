#!/usr/bin/env -S zsh -f

if (( ${+commands[pbcopy]} )) ; then
    if (( $# )) ; then
        # print -nr -- "${(pj.\n.)@}" |
        print -nr -- "${@}" |
            pbcopy
    else
        pbcopy
    fi
else
    echo "$0: pbcopy not found" >&2
    exit 1
fi
