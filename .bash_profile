if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/evar/anaconda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/evar/anaconda/etc/profile.d/conda.sh" ]; then
        . "/Users/evar/anaconda/etc/profile.d/conda.sh"
    else
        export PATH="/Users/evar/anaconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# source /Users/evar/Library/Preferences/org.dystroy.broot/launcher/bash/br

# export PATH="$HOME/.poetry/bin:$PATH"
if [ -e /Users/evar/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/evar/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
complete -C /Users/evar/go/bin/bitcomplete bit
