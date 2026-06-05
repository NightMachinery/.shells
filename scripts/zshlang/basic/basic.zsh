##
source "${${(%):-%x}:A:h}/core.zsh"
##
##
##
function uuidpy {
    python3 -c 'import uuid ; print(uuid.uuid4().hex)' |
        cat-copy-if-tty
}

function uuidm {
    doc "This is the official interface to create new UUIDs."

    {
        ##
        # You need to `gtr -d '\n'` on bigger outputs
        xxd -l 16 -p /dev/urandom
        ## Alt:
        # uuidgen | gtr -d '-' # '-' causes problems with some usages
        ##
    } | cat-copy-if-tty
}
##
function md5m {
    print -nr -- "$1" | md5sum | awk '{print $1}' | cat-copy-if-tty || {
        echo "Could not get md5 of '$1'" >&2
        return 1
    }
}

function hash-file {
    local f="$1"
    assert-args f @RET
    local engine="${hash_file_engine:-gsha512sum}"

    reval "${engine}" -- $f | awkn 1 | cat-copy-if-tty
    ##
    # command md5 -q "$f" #: This was Darwin-only.
    ##
}
function md5-file {
    hash_file_engine=md5sum hash-file "$@"
}

function md5-file-first-bytes {
    local f="$1" bytes="$2"
    assert-args f bytes @RET

    command dd if="$f" bs="$bytes" count=1 2>/dev/null |
        md5sum |
        awkn 1 | cat-copy-if-tty
}
##
##
setopt autocd multios re_match_pcre extendedglob pipefail interactivecomments hash_executables_only # hash_executables_only will not hash dirs instead of executables, but it can be slow.
setopt long_list_jobs complete_in_word always_to_end
setopt append_history extended_history hist_expire_dups_first hist_ignore_dups hist_ignore_space hist_verify inc_append_history share_history
setopt TYPESET_SILENT # Without this, the local/typeset commands display the value of any variable which is already defined.
unsetopt autopushd
unsetopt AUTO_NAME_DIRS
# Any parameter that is set to the absolute name of a directory immediately  becomes  a
#               name  for that directory, that will be used by the `%~' and related prompt sequences,
#               and will be available when completion is performed  on  a  word  starting  with  `~'.
#               (Otherwise, the parameter must be used in the form `~param' first.)
##
unsetopt BG_NICE # Run all background jobs at a lower priority.
# having this enabled will cause some failures in BTT-issued background brishz commands
##
rehash # make hash_executables_only take effect
# hash_executables_only's effect sometimes gets lost when sourcing load-first, probably a zsh bug
# echo t: ${commands[zsh]}
##
##
