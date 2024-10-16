##
function sftpgo-boot {
    local sftpgo_config="${sftpgo_config}"
    if isMBP ; then
        sftpgo_config="$NIGHTDIR/launchers/sftpgo/MBP.json"
    elif isMB2 ; then
        sftpgo_config="$NIGHTDIR/launchers/sftpgo/MB2.json"
    fi

    if test -n "$sftpgo_config" ; then
        #: =log-file-path= is relative to ==--config-dir=, i.e., [[~/base/keys/sftpgo/sftpgo.log]].
        ##
        #: This approach has better error handling, but restarting the process via tmux will NOT load the latest config.

        # local config_tmp
        # config_tmp="$(gmktemp --suffix .json)" @TRET
        # { cat "${sftpgo_config}" | json5-to-json > "${config_tmp}" } @TRET

        # assert tmuxnewsh2 sftpgo_shared reval-ec reval-notifexit sftpgo serve --config-file "${config_tmp}" --config-dir ~/Base/keys/sftpgo --log-file-path sftpgo.log @RET
        ##
        local cmd
        cmd='TMPSUFFIX=.json && reval-ec reval-notifexit sftpgo serve --config-file =(cat '"$(gquote-dq "${sftpgo_config}")"' | json5-to-json) --config-dir ~/Base/keys/sftpgo --log-file-path sftpgo.log'
        ecgray "$0: cmd: $cmd"

        assert tmuxnew sftpgo_shared zsh -c "$cmd" @RET
        ##
    else
        return 1
    fi

    # tmuxnewsh2 shared_sftpgo indir ~/Base/keys/sftpgo sftpgo portable -d ~/Base/shared --permissions '*' --username "$SFTPGO_USER1" --password "$SFTPGO_PASS1" --webdav-port 8114 --sftpd-port 8115 --ftpd-port 8116 --log-verbose --log-file-path sftpgo.log --advertise-service
}
##
function sftpgo-portable {
    local serve_dir="${1:-$PWD}"
    local perms="${sftpgo_portable_perms:-list,download}" # use '*' to grant all
    # https://sftpgo.stoplight.io/docs/sftpgo/openapi.yaml/components/schemas/Permission

    local user="${sftpgo_portable_user:-alice}"
    local pass="${sftpgo_portable_pass}"
    local webdav="${sftpgo_portable_webdav}"
    local sftp="${sftpgo_portable_sftp}"
    local ftp="${sftpgo_portable_ftp}"
    assert-args user pass @RET

    local opts=()
    if test -n "$webdav" ; then
        opts+=( --webdav-port "$webdav" )
    fi
    if test -n "$sftp" ; then
        opts+=( --sftpd-port "$sftp" )
    fi
    if test -n "$ftp" ; then
        opts+=( --ftpd-port "$ftp" )
    fi

    local d=~/Base/keys/sftpgo
    assert mkdir -p "$d" @RET
    indir "$d" reval-ec sftpgo portable -d "$serve_dir" --permissions "$perms" --username "$user" --password "$pass" --log-level 'debug' --log-file-path "$(ecn "sftpgo__${serve_dir}.log" | gtr / _ | str2filename)" "$opts[@]"
    # --advertise-service
}

function sftpgo-serve-dl {
    #: `sudo ufw allow 9001 9002 9003`
    ##
    tmuxnewsh2 "$0" @opts sftp 9003 ftp 9002 webdav 9001 pass "${1:?}" @ sftpgo-portable ~/Downloads
}

function sftpgo-serve-base-writable {
    local pass="${1:-$(passgen)}"
    local d=~/base

    mkdir -p "$d"
    tmuxnewsh "$0" reval-ec @opts perms '*' sftp 9104 webdav 9100 pass "$pass"  @ sftpgo-portable "$d"
}
##
