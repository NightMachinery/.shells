##
function backup-rsync {
    backup-rsync-greencase
}

function backup-rsync-greencase {
    source="$GREENCASE_DIR/"
    dest=/Volumes/hyper-diva/backups/greencase
    assert mkdir -p "$dest" @RET
    if test -d "$source" && test -d "$dest" ; then
        rspb --exclude='.git' "$source" "$dest"
    fi
}
##
function backup-startupSh {
    local d="$nightNotes/private/configs/$(hostname)/$(whoami)"
    mkdir-m "$d"

    local i
    for i in /Users/Shared/bin/startup.sh ~/.privateStartup.sh ~/.startup..private..zsh ; do
        if test -e "$i" ; then
            cp "$i" "${d}/"
        fi
    done
}
##
function backup-cron {
    # aka cellar-getcron
    ##
    if ! { isLocal || isLilf } ; then
        return 1
    fi

    local d="$nightNotes/private/backups/crontabs/$(hostname)"
    mkdir -p "$d"
    crontab -l > "${d}/$(whoami)"
}
##
function backup-private-common {
    if isLocal ; then
        if ! ask "$0: This is a local computer. Are you sure you want to proceed?" N ; then
            return 1
        fi
    fi

    assert test -e "$nightNotes" @RET

    local d="$nightNotes/private/backups/$(hostname)_$(whoami)/private-common"

    pushf "$d"
    {
        local i
        for i in ~/.privateShell ~/.private-config.el ; do
            if test -e "$i" ; then
                reval-ec cp "$i" "$d"/
            fi
        done
    } always { popf }
}
##
function backup-file() {
    assert isdefined-cmd gdate @RET

    local f="${1:?}"

    if test -e "$f" ; then
        cp --verbose --backup=t --suffix='.bak'  $f ~/base/backup/auto/"${f:t} $(md5m "$f")/$(gdate +"%Y %b %d %H:%M:%S")/" || {
            local ret=$?
            ecerr "$0: Failed with '$ret' for '$f'"
            return $ret
        }
        # t: Always make numbered backups.
        # suffix somehow doesn't seem to work
    fi
}
### * eva
function eva-backup1 {
    assert-net @RET

    local ssh_host=eva

    assert pushf $nightNotes/private/configs/eva/misc @RET
    {
        assert ssh "${ssh_host}" crontab -l > ./crontab

        assert rsp-safe "${ssh_host}":.privateShell .

        mkdir -p .thelounge
        assert rsp-safe "${ssh_host}":.thelounge/config.js ./.thelounge/
        assert rsp-safe "${ssh_host}":.thelounge/users ./.thelounge/
    } always { popf }
}
### * zii
function ziib-all {
    ecerr "$0: zii currently dead" ; return 0

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


        assert ssh zii@51.178.215.202 crontab -l > ./crontab

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

        mkdir -p .thelounge
        assert scp root@51.178.215.202:/home/zii/.thelounge/config.js ./.thelounge/
        assert scp -r root@51.178.215.202:/home/zii/.thelounge/users ./.thelounge/
    } always { popf }

    reval-ec ziib-znc
}

function ziib-znc {
    assert-net @RET

    assert pushf $nightNotes/private/configs/zii/ @RET
    {
        # ZNC
        assert reval-ec rsp-dl --exclude '/**/log/' --exclude 'modules/' --exclude 'znc.pem' root@51.178.215.202:/home/zii/.znc ./
        # modules: the external, binary modules
        # znc.pem: the SSL certs
        # @note I've also added these files to the =.gitignore= file

        ## chat logs
        if test -e "$chat_logs_dir" ; then
            local dest=${chat_logs_dir}/znc
            mkdir -p "$dest"

            local exclusions=('##chat' '##chat-overflow' '##news' '##politics' '#libera')
            local opts=()
            local e
            for e in $exclusions[@] ; do
                opts+=(--exclude "$e")
            done
            assert reval-ec rsp-dl "$opts[@]" root@51.178.215.202:/home/zii/.znc/moddata/log "${dest}"
        else
            ecerr "$0: chat_logs_dir does not exists; Skipping downloading them."
        fi
    } always { popf }
}
### arvan_1
function arvan_1-all {
    local name="arvan_1" user="ubuntu"
    local ssh_add="${user}@nightmachinery.ir"

    assert-net @RET

    assert pushf "$nightNotes/private/configs/$name/" @RET
    {
        assert ssh $ssh_add crontab -l > ./crontab

        # assert scp $ssh_add:/home/$user/.muttrc .
        assert scp $ssh_add:/home/$user/Caddyfile .
        assert scp $ssh_add:/home/$user/various.zsh .
        assert scp $ssh_add:/home/$user/.privateShell .

        mkdir -p v2ray
        assert scp $ssh_add:/usr/local/etc/v2ray/config.json ./v2ray/

        mkdir -p trojan/server
        assert scp $ssh_add:/usr/local/etc/trojan/config.json ./trojan/server/

        # mkdir -p .thelounge
        # assert scp $ssh_add:/home/$user/.thelounge/config.js ./.thelounge/
        # assert scp -r $ssh_add:/home/$user/.thelounge/users ./.thelounge/
    } always { popf }

    # reval-ec arvan_1-znc
}
### behy
function backup-behy-all() {
    assert-net @RET

    local ip=51.89.107.137

    assert pushf $nightNotes/private/configs/behy/ @RET
    {
        assert reval-ec ssh "walle@$ip" crontab -l > ./crontab

        assert reval-ec rsp-dl "walle@$ip":/home/walle/Caddyfile .
        assert reval-ec rsp-dl "walle@$ip":/home/walle/.privateShell .

        mkdir -p v2ray
        assert reval-ec rsp-dl "walle@$ip":/usr/local/etc/v2ray/config.json ./v2ray/

        mkdir -p trojan
        assert reval-ec rsp-dl "walle@$ip":/usr/local/etc/trojan/config.json ./trojan/
    } always { popf }
}
###
function backup-vimium {
    assert reval-ec mv ~dl/vimium_c*.json ~nt/private/backups/'Vimium C'/vimium_c.json @RET
}
##
function backup-redirector {
    mv2 ~nt/private/backups/Redirector/ ~dl/Redirector.json
}

function backup-stylus {
    dl onlc mv2 ~nt/private/backups/userstyles/stylus.json
}

function backup-sftpgo {
    dl onlc mv2 ~nt/private/backups/sftpgo/sftpgo.json
}
##
function backup-arc {
    cp2 "${nightNotesPrivate}/backups/arc_browser" \
        ~'/Library/Application Support/Arc/'*.json~*/(StorableAuth|StorableArchive|StorableWindows)*(DN.)
}
##
function rclone-backup {
    cp -r ~/.config/rclone ${nightNotesPrivate}/backups/
}
##
function backup-simon-llm {
    local d="$nightNotes/private/configs/simon_llm/"
    # $(hostname)/$(whoami)/

    mkdir-m "$d"

    local fs=(
        ~/'Library/Application Support/io.datasette.llm/extra-openai-models.yaml'
    )

    local i
    for i in ${fs[@]} ; do
        if test -e "$i" ; then
            cp "$i" "${d}/"
        fi
    done
}
##
