typeset -Ug path
function addToPATH {
    local newPath="$1"

    if ! test -d "$newPath" ; then # works for symlinks, too
        return 1
    fi
    ## too costly:
    # if (( ${+commands[grealpath]} )) ; then
    #     newPath="$(grealpath --canonicalize-existing "$newPath")" || return $?
    # fi
    ##
    # case ":$PATH:" in
    #     *":$1:"*) :;; # already there
    #     *) PATH="$1:$PATH";; # org/r PATH="$PATH:$1"
    # esac
    #path[1,0]="$1"
    #path=("$1" "$path[@]")
    ##
    PATH="$newPath:$PATH"
    # typeset -Ug path
    ##
    # rehash
}
function add-path {
    doc add-path NODE_PATH /some/path
    local p="$(eval 'ec $'"$1")"
    test -z "$p" && {
        eval "$1=$2:q"
    } ||
        { eval 'case ":$p:" in
        *":$2:"*) :;; # already there
        *) '"$1"'="$2:$p";; # org/r PATH="$PATH:$1"
    esac' }
    eval "export $1"
}
