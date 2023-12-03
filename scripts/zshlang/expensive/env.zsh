if ((${+commands[npm]})) ; then
    # redis can be down for the startup sessions, hence it's better to hardcode the dir. It's also faster.
    if isMBP ; then
        add-path NODE_PATH '/usr/local/lib/node_modules'
    elif isMB2 ; then
        add-path NODE_PATH '/opt/homebrew/lib/node_modules'
    else
        add-path NODE_PATH "$(memoi_skiperr=y memoi-eval npm root -g)"
    fi

    ##: I don't know about these:
    # re 'add-path NODE_PATH' '/usr/local/lib/node_modules/' '/usr/lib/nodejs' '/usr/lib/node_module' '/usr/share/javascript'
    # silence eval "add-path NODE_PATH /home/linuxbrew/.linuxbrew/Cellar/node/(^node_modules*/)#/node_modules"
    ##
fi
