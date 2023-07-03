function ls-by-added() {
    # Doesn't work well for files having '(null)' as their DateAdded, which some do.
    mdls -name kMDItemFSName -name kMDItemDateAdded -raw -- *(D) | \
        xargs -0 -I {} echo {} | \
        sed 'N;s/\n/ /' | \
        sort --reverse | \
        sed -E "s/^.*\\+0000 //" # removes the timestamps
}

function onxla(){
    last-added|gxargs -I _ "$(gquote "$@")"
}

function onxlc(){
    last-created|gxargs -I _ "$(gquote "$@")"
}

function first-file(){
    exa|head -n1
}

function on-g {
    # local engine="${on_g_engine:${on_g_e}}"
    local engine="$1" ; shift || return 1
    printz-quoted "$@" "$(grealpath -- "$(reval "$engine")")"
}
aliasfn onla on-g last-added
aliasfn onlac on-g last-accessed
aliasfn onlm on-g last-modified
aliasfn onlc on-g last-created
aliasfn onff on-g first-file
