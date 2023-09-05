##
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
    pip install --upgrade ddgr fanficfare cloudscraper
    pip install --upgrade --force pytube ytmusicapi youtube-dl spotipy spotdl

    # pip install -U yt-dlp
    $proxyenv yt-dlp --update-to nightly

    tldr --update

    ##
    brew update

    if isServer ; then
        brew upgrade
        brew cleanup
    fi

    if isLocal ; then
        # re 'brew upgrade' googler # triggers upgrading everything, which we can't afford on weak servers

        # brew upgrade microsoft-edge @STRUE #: needs @sudo
    fi
    ##
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

    pip uninstall -y pyOpenSSL
    #: https://github.com/aws/aws-cli/issues/7325
    #: makes BrishGarden not work
}
##
function cron-commands-reboot-get {
    crontab -l |
        perl -nle 'print $1 if /^\s*\@reboot\s+(.+)/'
}

function cron-commands-reboot-run {
    local cmds
    cmds=(${(@f)"$(cron-commands-reboot-get)"})

    for cmd in $cmds[@] ; do
        eval-ec "$cmd"
    done
}
##
