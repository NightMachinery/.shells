function backup-cron() {
    # aka cellar-getcron
    local d="$nightNotes/backups/crontabs/$(hostname)"
    mkdir -p "$d"
    crontab -l > "${d}/$(whoami)"
}
