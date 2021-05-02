function sftpgo-portable() {
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
function sftpgo-serve-dl() {
    tmuxnewsh2 sftpgo-dl @opts sftp 9003 pass "${1:?}" @ sftpgo-portable ~/Downloads
}
##
