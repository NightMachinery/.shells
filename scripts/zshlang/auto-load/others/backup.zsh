##
function backup-rsync() {
    backup-rsync-greencase
}
function backup-rsync-greencase() {
    source="$GREENCASE_DIR/"
    dest=/Volumes/hyper-diva/backups/greencase
    aasert mkdir -p "$dest" @RET
    if test -d "$source" && test -d "$dest" ; then
        rspb --exclude='.git' "$source" "$dest"
    fi
}
##
function backup-startupSh() {
    local d="$nightNotes/private/configs/$(hostname)/$(whoami)"
    mkdir -p "$d"
    cp /Users/Shared/bin/startup.sh "${d}/"
}
##
function backup-cron() {
    # aka cellar-getcron
    local d="$nightNotes/backups/crontabs/$(hostname)"
    mkdir -p "$d"
    crontab -l > "${d}/$(whoami)"
}
##
function backup-file() {
    local f="${1:?}"
    if test -e "$f" ; then
        cp --verbose --backup=t --suffix='.bak'  $f ~/base/backup/auto/"${f:t} $(md5m "$f")/$(dateshort)/" || {
            local ret=$?
            ecerr "$0: Failed with '$ret' for '$f'"
            return $ret
        }
        # t: Always make numbered backups.
        # suffix somehow doesn't seem to work
    fi
}
### zii
function ziib-all() {
    assert-net @RET

    assert pushf $nightNotes/private/configs/zii/ @RET
    {
        mkdir -p discourse
        assert scp root@51.178.215.202:/var/discourse/containers/app.yml ./discourse/

        local res
        if res="$(full-html2 https://discourse.lilf.ir/admin/site_settings.json)" ; then
           # https://github.com/pfaffman/discourse-settings-uploader
           ec "$res" > ./discourse/site_settings.json
        else
            ecerr "$0: Could not download https://discourse.lilf.ir/admin/site_settings.json"
        fi


        assert scp zii@51.178.215.202:/home/zii/.muttrc .
        assert scp zii@51.178.215.202:/home/zii/Caddyfile .
        assert scp zii@51.178.215.202:/home/zii/.privateShell .

        mkdir -p v2ray
        assert scp zii@51.178.215.202:/usr/local/etc/v2ray/config.json ./v2ray/

        mkdir -p trojan
        assert scp root@51.178.215.202:/usr/local/etc/trojan/config.json ./trojan/

        mkdir -p mailu
        assert scp root@51.178.215.202:/mailu/docker-compose.yml ./mailu/
        assert scp root@51.178.215.202:/mailu/mailu.env ./mailu/
    } always { popf }
}
###
