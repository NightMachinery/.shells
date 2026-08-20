typeset -Ug path
function addToPATH {
    #: One assignment to the array, not one assignment to the string per
    #: argument. PATH and path are tied, so `PATH="$x:$PATH"' re-splits the
    #: whole (12k character, ~300 entry) string every time round the loop,
    #: which made the recursive add in ~/.shared.sh quadratic: 253 directories
    #: cost 42ms, against 0.3ms for a single array assignment.
    #:
    #: (Oa) reverses, because prepending one at a time leaves the arguments in
    #: reverse order and callers depend on which directory wins a name clash.
    #: Verified byte-identical to what the old loop produced.
    #:
    #: Assigning the array also applies the -U above, so duplicates go now
    #: rather than surviving until the `typeset -Ug path' at the end of
    #: ~/.shared.sh. Same final PATH, just without the bloat in between.
    (( $# )) || return 0
    path=( ${(Oa)@} $path )
}

function addToPATH-v1 {
    #: The original, kept because it is the portable shape: no zsh array
    #: syntax, so it is what a non-zsh shell would need.
    local newPath
    for newPath in $@ ; do
        ##
        # if ! test -d "$newPath" ; then # works for symlinks, too
        #     return 1
        # fi
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
    done

    # typeset -Ug path

    # rehash
}

function add-path {
    : "add-path NODE_PATH /some/path"

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
