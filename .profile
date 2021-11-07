if [ -n "$BASH_VERSION" ] ; then
    if [ -f ~/.bash_profile ]; then
        . ~/.bash_profile
    fi
fi

export PATH="${HOME}/miniconda3/bin:${PATH}"
