#!/usr/bin/env zsh

##
powersaving_status_del
# wireguard_enabled_del
##
darwin-dns-set 8.8.8.8 1.1.1.1 # 127.0.0.1
##
if test -n "${MDNS_NAME}" ; then
    # mdns-init-darwin
fi
##

brishgarden-boot 64

if isMB2 ; then
    battery-charge-limit-restore-status
    # battery-charge-limit-enable
fi

if isMe || isGrayfur ; then
    tmuxnewsh2 serve-dl caddy run --config $NIGHTDIR/launchers/Caddyfile_darwin
fi

if isMe ; then
    ##
    tmuxnew lock-watcher lock_watcher.swift -v
    ##
    blackbutler-boot
    ##
    if isdefined-cmd sftpgo ; then
        sftpgo-boot || true
    fi
    ##
    # tmuxnewsh2 shared_smb loop reval-notifexit sudo /usr/sbin/smbd -no-symlinks false -stdout -debug
    # tmuxnewsh2 shared_ftp_books reval-notifexit python -m pyftpdlib -i '192.168.1.56' -p 8119 -d ~/Base/books --debug
    ##
    # @warn dav has no pass set on writable
    # @fatal wsgidav consumes way too much CPU (was at 98% even with no WiFi connected), and might even be hanging the laptop
    # tmuxnewsh2 shared_dav reval-notifexit wsgidav --config="$NIGHTDIR/launchers/wsgidav_darwin.yaml"
    # tmuxnewsh2 shared_dav wsgidav --host=192.168.1.56 --port=8113 --root=~/Base/shared --auth=anonymous
fi
##
# tmuxnew supercollider scsynth -u 57110 -a 1024 -i 2 -o 2 -R 0 -l 100  -B 127.0.0.1

if ot-enabled-p ; then
    ot-server-daemon
fi
###
if test -n "$NP_PASS1" ; then
    # tmuxnew naive-zii naive --listen="socks://127.0.0.1:1080" --proxy="https://alice:$NP_PASS1@np.zii.lilf.ir" --log  --concurrency=4
    ##
    # tmuxnew naive-arvan_1 naive --listen="socks://127.0.0.1:1080" --proxy="https://alice:$NP_PASS1@np.nightmachinery.ir" --log  --concurrency=4
fi

if test -n "$NP_PASS0" ; then
    # tmuxnew naive-${lilf_user} naive --listen="socks://127.0.0.1:1078" --proxy="https://alice:$NP_PASS0@np.lilf.ir" --log  --concurrency=4
fi

if isMe ; then
    ##
    tmuxnew direct-proxy gost -L socks5://0.0.0.0:3081 -L http://0.0.0.0:3087
    ##
    # tmuxnew v2ray v2ray -config $nightNotes/private/configs/zii/v2ray/v1.zii.json
    ##
    # tmuxnew v2ray-behy v2ray -config $nightNotes/private/configs/behy/v2ray/client.json
    ##
    # tmuxnewsh2 trojan trojan -c "$nightNotes"/private/configs/zii/trojan_client.json
    # tmuxnew trojan-client-v1-zii trojan -c $nightNotes/private/configs/zii/trojan_client_v1.json
    ##
    # tmuxnew trojan-client-arvan_1 trojan -c $nightNotes/private/configs/arvan_1/trojan/client.json
    ##
    v2-on # v2ray-genrouter
    ##
    # tmuxnewsh2 socks2http hpts --level info -s 127.0.0.1:1081 -p 1087 # https://github.com/oyyd/http-proxy-to-socks
    ##
fi
###
if isMe ; then
    # tmuxnewsh2 hotwords-porc porc-listen
    ##
    # tmuxnewsh2 browser-recordings-process-watch browser-recordings-process-watch
    ## These two might not load properly in the cron context
    # iterm-boot
    ##
    # tmuxnewsh kitty ITERM_SESSION_ID= loop env -i ${commands[kitty]} -o allow_remote_control=yes --listen-on unix:"$HOME/tmp/.kitty" --single-instance --session "$NIGHTDIR/configFiles/kitty/session.kitty"
    ##
    # indir borg borgp=1080 borg_session=session_m python stdborg.py
    ##
    # needs to run by cron instead:
    # tmux new -d -s books "$NIGHTDIR/zshlang/book-checker.zsh"
    ##
    # wallpaper-auto
fi
##
tmuxnewsh2 clipboard-record clipboard-record
##
psource ~/.startup..private..zsh
##
emc-launch-gui
##
chronic-all
