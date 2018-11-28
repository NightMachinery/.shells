if [ -f ~/.zshenv ]; then
    #Since this file is sourced by .bash_profile, that is fine, too.
    . ~/.zshenv
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion


