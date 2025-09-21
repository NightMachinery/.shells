##
function chronic-all {
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
    pipx upgrade aider-chat

    pip-install fanficfare cloudscraper
    # ddgr
    pip install --upgrade --force tzdata pytube ytmusicapi youtube-dl spotipy spotdl

    yt-dlp-update

    # $proxyenv yt-dlp --update-to nightly

    tldr --update

    ##
    brew update

    if isServer ; then
        brew upgrade
        brew cleanup
    fi

    if isLocal ; then
        # re 'brew upgrade' sth # triggers upgrading everything, which we can't afford on weak servers

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
        backup-arc

        backup-startupSh

        # ziib-all

        # backup-rsync
    fi
}

function chronic-anticreep {
    ansifold-path-fix @STRUE
    ##
    pip uninstall -y enum34 # Since python 3.6 the enum34 library is no longer compatible with the standard library.
    pip uninstall -y typing # same
    pip uninstall -y dataclasses # same

    pip uninstall -y pyOpenSSL
    #: https://github.com/aws/aws-cli/issues/7325
    #: makes BrishGarden not work

    pip install -U 'urllib3<2'
    #: [[id:90489ec6-5e11-40c9-9f13-673942019b33][python - "cannot import name 'DEFAULT_CIPHERS' from 'urllib3.util.ssl_'" on AWS Lambda using a layer - Stack Overflow]]
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
