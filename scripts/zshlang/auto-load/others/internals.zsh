##
function show-options {
    setopt localoptions
    local opt

    print "Current zsh options:"
    # print "-----------------"

    for opt in ${(k)options}; do
        printf "%-25s %s\n" $opt ${options[$opt]}
    done | sort
}
##
