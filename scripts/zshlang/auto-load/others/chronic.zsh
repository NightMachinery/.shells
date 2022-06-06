function chronic-all() {
    tmuxzombie-kill
    rm-caches
    chronic-backup
    chronic-update
    chronic-anticreep
    chronic-certs

    cleanup
}

function chronic-certs {
    if test -d ~/.znc ; then
        assert cat $HOME/.local/share/caddy/certificates/acme-v02.api.letsencrypt.org-directory/irc*.lilf.ir/*.(key|crt) > ~/.znc/znc.pem @RET
    fi
}

function chronic-update {
    brew update
    # re 'brew upgrade' googler # triggers upgrading everything, which we can't afford on weak servers

    pip install --upgrade ddgr fanficfare cloudscraper
    pip install --upgrade --force pytube ytmusicapi youtube-dl yt-dlp spotipy spotdl

    tldr --update

    if isServer ; then
        brew upgrade
        brew cleanup
    fi

    if isLocal ; then
        # brew upgrade microsoft-edge @STRUE #: needs @sudo
    fi
}

function chronic-backup {
    if isLilf ; then
        backup-private-common
    fi

    re backup-file $timetracker_db $HISTFILE
    backup-cron

    if isLocal && isMe ; then
        backup-startupSh
        ziib-all
        # backup-rsync
    fi
}

function chronic-anticreep() {
    ansifold-path-fix @STRUE
    ##
    pip uninstall -y enum34 # Since python 3.6 the enum34 library is no longer compatible with the standard library.
    pip uninstall -y typing # same
    pip uninstall -y dataclasses # same
}
##
