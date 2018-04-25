if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# added by Anaconda3 4.4.0 installer
# export PATH="/Users/evar/anaconda/bin:$PATH"

# source /Users/evar/.oh-my-git/prompt.sh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/evar/.sdkman"
[[ -s "/Users/evar/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/evar/.sdkman/bin/sdkman-init.sh"


# Setting PATH for Python 2.7
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH
