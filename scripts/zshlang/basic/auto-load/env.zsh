typeset +x FORCE_INTERACTIVE # unexporting FORCE_INTERACTIVE will stop polluting the children's env
typeset +x KITTY_WINDOW_ID
##
alias sbb='KITTY_WINDOW_ID=$KITTY_WINDOW_ID exec zsh'
##
