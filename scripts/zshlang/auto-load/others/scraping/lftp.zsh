##
function lftp-dir {
    lftp -c mirror --verbose=3 "$@"
    # mirror [OPTS] [source [target]]
    # A lot of settings are in ~/.lftprc
    # By default the source is remote and the target is a local directory.  When using  -R, the source directory is local and the target is remote.  If the target directory is omitted, base name of the source  directory  is  used.
    # If the target directory ends with a slash (except the root directory) then base name of the source directory is appended.

    if test -z "$bella_zsh_disable1" && isI && fn-isTop dl-dir lftp-dir
    then
        bell-dl
    fi
}
##
