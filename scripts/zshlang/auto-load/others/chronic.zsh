function chronic-all() {
    tmuxzombie-kill
    rm-caches
    chronic-backup
    chronic-update
    chronic-anticreep
}
function chronic-update() {
    brew update
    re 'brew upgrade' googler
    re 'pip install -U' ddgr youtube-dl fanficfare cloudscraper
    tldr --update
    if isServer ; then
        brew upgrade
    fi
}
function chronic-backup() {
    if isLilf ; then
        backup-private-common
    fi

    re backup-file $timetracker_db $HISTFILE
    backup-cron

    if isLocal ; then
        backup-startupSh
        ziib-all
        # backup-rsync
    fi
}
function chronic-anticreep() {
    pip uninstall -y enum34 # Since python 3.6 the enum34 library is no longer compatible with the standard library.
    pip uninstall -y typing # same
    pip uninstall -y dataclasses # same
}
##
