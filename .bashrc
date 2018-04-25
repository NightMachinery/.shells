if [ -f ~/.zshenv ]; then
    #Since this file is sourced by .bash_profile, that is fine, too.
    . ~/.zshenv
fi

# Git configuration
# Branch name in prompt
source ~/.git-prompt.sh
PS1='[\W$(__git_ps1 " (%s)")]\$ '
export PROMPT_COMMAND='echo -ne "\033]0;${PWD/#$HOME/~}\007"'
# Tab completion for branch names
source ~/.git-completion.bash

export VISUAL=nvim
export EDITOR="$VISUAL"

# if [[ $EMACS != "" ]]; then
#     export EDITOR=$emacsdir"/emacsclient -a --no-wait --socket-name="$socket
# else
#     export EDITOR=$emacsdir"/emacs"
# fi

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
