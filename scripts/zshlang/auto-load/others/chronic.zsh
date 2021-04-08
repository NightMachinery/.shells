function chronic-all() {
    tmuxzombie-kill
    rm-caches
    chronic-backup
    chronic-update
    chronic-anticreep
}
function chronic-update() {
    brew update
    re 'brew upgrade' googler fanficfare
    re 'pip install -U' ddgr youtube-dl fanficfare
    tldr --update
    if isServer ; then
        brew upgrade
    fi
}
function chronic-backup() {
    re backup-file $timetracker_db
    backup-startupSh
    backup-cron
    ziib-all
    # backup-rsync
}
function chronic-anticreep() {
    pip uninstall -y enum34 # Since python 3.6 the enum34 library is no longer compatible with the standard library.
    pip uninstall -y typing # same
    pip uninstall -y dataclasses # same
}
##
