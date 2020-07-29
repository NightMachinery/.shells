function backup-cron() {
    local d="$cellar/notes/dev/snippets/crontabs/$(hostname)"
    mkdir -p "$d"
    crontab -l > "${d}/$(whoami)"
}