##
function sftpgo-boot {
    local sftpgo_config="${sftpgo_config}"
    if isMBP ; then
        sftpgo_config="$NIGHTDIR/launchers/sftpgo/MBP.json"
    elif isMB2 ; then
        sftpgo_config="$NIGHTDIR/launchers/sftpgo/MB2.json"
    fi

    if test -n "$sftpgo_config" ; then
        assert tmuxnewsh2 sftpgo_shared reval-notifexit sftpgo serve --config-file "$sftpgo_config" --config-dir ~/Base/keys/sftpgo --log-file-path sftpgo.log @RET
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
    indir "$d" reval-ec sftpgo portable -d "$serve_dir" --permissions "$perms" --username "$user" --password "$pass" --log-verbose --log-file-path "$(ecn "sftpgo__${serve_dir}.log" | gtr / _ | str2filename)" "$opts[@]"
    # --advertise-service
}

function sftpgo-serve-dl {
    tmuxnewsh2 "$0" @opts sftp 9003 pass "${1:?}" @ sftpgo-portable ~/Downloads
}

function sftpgo-serve-base-writable {
    local pass="${1:-$(passgen)}"
    local d=~/base

    mkdir -p "$d"
    tmuxnewsh "$0" reval-ec @opts perms '*' sftp 9104 webdav 9100 pass "$pass"  @ sftpgo-portable "$d"
}
##
