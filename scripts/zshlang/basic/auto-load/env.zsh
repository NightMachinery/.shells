typeset +x FORCE_INTERACTIVE # unexporting FORCE_INTERACTIVE will stop polluting the children's env

##
if test -n "$KITTY_WINDOW_ID" ; then
    export TERM_PROGRAM=kitty
fi
typeset +x KITTY_WINDOW_ID
# I don't remember why we had unexported this, but doing that will essentially force us to either hardcode 'isKitty' or re-export this var in our wrappers
# Perhaps the unexporting helps with not having wrong IDs in the tmux sessions, which can cause problems.
##
