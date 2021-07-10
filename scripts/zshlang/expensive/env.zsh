if ((${+commands[npm]})) ; then
    # redis can be down for the startup sessions, hence it's better to hardcode the dir. It's also faster.
    if isMBP ; then
        add-path NODE_PATH /usr/local/lib/node_modules
    else
        add-path NODE_PATH "$(memoi_skiperr=y memoi-eval npm root -g)"
    fi
fi
