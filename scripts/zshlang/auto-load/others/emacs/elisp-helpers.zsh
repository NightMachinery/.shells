function sly-doc-oneline() {
    local doc="$1"

    doc="$(ecn "$doc" | gsed '4,$p' | gtr $'\n' " ")"

    if [[ "$doc" =~ '\s*Documentation:\s+(.*)' ]] ; then
        doc="$match[1]"

    fi

    ecn "${doc[1,100]}"
    ## tests:
    # - equalp has docs
    ##
}
##
function wh-docstring() {
    local s="$1"
    local res
    res="$({
    if (( ${+functions[$s]} )) ; then
        ecn "fn: ${functions[$s]}"
    elif (( ${+aliases[$s]} )) ; then
        ecn "alias: ${aliases[$s]}"
    elif (( ${+commands[$s]} )) ; then
        ecn "${commands[$s]}"
    else
        local v
        if v="$(serr typeset -p "$s")" && [[ "$v" =~ '[^=]+=(.*)' ]] ; then
          ecn "${match[1]}"
        fi
    fi
    } | gtr $'\n' " " )"

    ecn "${res[1,80]}"
}
##
