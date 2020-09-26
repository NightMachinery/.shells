function backup-cron() {
    # aka cellar-getcron
    local d="$nightNotes/backups/crontabs/$(hostname)"
    mkdir -p "$d"
    crontab -l > "${d}/$(whoami)"
}

### zii
function ziib-all() {
    pushf $nightNotes/private/configs/zii/
    {
        mkdir -p discourse
        scp root@51.178.215.202:/var/discourse/containers/app.yml ./discourse/
        scp zii@51.178.215.202:/home/zii/Caddyfile .

        mkdir -p mailu
        scp root@51.178.215.202:/mailu/docker-compose.yml ./mailu/
        scp root@51.178.215.202:/mailu/mailu.env ./mailu/
    } always { popf }
}
###
