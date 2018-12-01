if [ -f ~/.zshenv ]; then
    #Since this file is sourced by .bash_profile, that is fine, too.
    . ~/.zshenv
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion



#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/eva/.sdkman"
[[ -s "/home/eva/.sdkman/bin/sdkman-init.sh" ]] && source "/home/eva/.sdkman/bin/sdkman-init.sh"
