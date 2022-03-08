##
function wgetm() {
    : "--continue (not enabled here) will assume any smaller already present file is an incomplete version of the server's file, and wget will try to resume it."

    local opts=()

    opts+='--content-disposition'
    #: --content-disposition If this is set to on, experimental (not fully-functional) support for "Content-Disposition" headers is enabled. This can currently result in extra round-trips to the server for a "HEAD" request, and is known to suffer from a few bugs, which is why it is not currently enabled by default.
    #: This option is useful for some file-downloading CGI programs that use "Content-Disposition" headers to describe what the name of a downloaded file should be.
    #: When combined with --metalink-over-http and --trust-server-names, a Content-Type: application/metalink4+xml file is named using the "Content-Disposition" filename field, if available.

    opts+='--trust-server-names'
    #: --trust-server-names: If this is set, on a redirect, the local file name will be based on the redirection URL.  By default the local file name is based on the original URL.  When doing recursive retrieving this can be helpful because in many web sites redirected URLs correspond to an underlying file structure, while link URLs do not.

    $proxyenv wget -e robots=off --user-agent "$useragent_chrome" --header "$(cookies)" "$opts[@]" "$@"

    if test -z "$bella_zsh_disable1" && isI && @opts p [ wget ] @ fn-isTop
    then
        bell-dl
    fi
}

function wget-cookies() {
    wgetm --header "$(cookies-auto "$@")" "$@"
}
##
function url-dir-count {
    local url="$1"

    if [[ "$url" =~ '^(?:[^/]+://)?[^/]+/*(.*)$' ]] ; then
        # '\s' catches '\n', too. Deleting these help avoid errors like '../ ', where a careless mistake can cause a wrong answer.
        ecn "${match[1]}" | sd '\s' '' | prefixer -i / --skip-empty | count-lines
    else
        ecerr "$0: ill-formed URL"
        return 1
    fi
    ## tests
    # `url-dir-count https://files.zii.lilf.ir///tmp///a9///a//`
    # `url-dir-count files.zii.lilf.ir///tmp///a9///a//`
    ##
}
reify url-dir-count

function wget-dir {
    # @alt dl-dir
    ##
    # https://stackoverflow.com/questions/17282915/how-to-download-an-entire-directory-and-subdirectories-using-wget
    ##
    local depth="${wget_dir_d:-0}" url="${@[-1]}" opts=()

    local parent_count
    if parent_count="$(serr url-dir-count "$url")" ; then
        opts+=( --cut-dirs $((parent_count - 1)) )
    fi

    wgetm -r --level="$depth" --no-host-directories --no-parent --reject="index.html*" --no-clobber "${opts[@]}" "$@" @RET
    # --level (-l) determines the depth of the recursion (5 by default, use 0 or 'inf' for unlimited):
    #
    # + -l1 just download the directory (tzivi in your case)
    #
    # + -l2 download the directory and all level 1 subfolders ('tzivi/something' but not 'tivizi/somthing/foo')
    #
    # --no-host-directories (-nH) option with wget to prevent the hostname directory getting created by default with the download directory.
    #
    # --cut-dirs=X (cuts out X directories)(Big X acts effectively like --no-directories)
    #
    # --no-directories (-nd): do not create a hierarchy of directories when retrieving recursively. With this option turned on, all files will get saved to the current directory, without clobbering
    #
    # --no-clobber: When running Wget with -r or -p, but without -N, -nd, or -nc, re-downloading a file will result in the new copy simply overwriting the old.  Adding -nc will prevent this behavior, instead causing the original version to be preserved and any newer copies on the server to be ignored.
    #
    # --timestamping (-N): uses the timestamps of the file and the server to determine if it has changed. (Presumably.)
    # When running Wget with -N, with or without -r or -p, the decision as to whether or not to download a newer copy of a file depends on the local and remote timestamp and size of the file.  -nc may not be specified at the same time as -N.
    ##
}
##
function wget-multi {
    wgetm --continue "$@"
}
aliasfn dl-multi wget-multi
##
