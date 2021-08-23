function rename-rec() {
    local max_depth="${rename_rec_max_depth:-100}"

    local d
    for d in {${max_depth}..1} ; do
        assert rename-depth "$d" "$@" @RET
    done

    ## tests:
    # `rename-rec '(.*)evil([^/]*)' '{:1}happy{:2}'`
    ##
}
function rename-depth() {
    local depth="${1}" dir="${rename_depth_dir:-.}" undo="${rename_depth_undo:-undo/undo}"
    assert-args depth dir undo @RET

    undo="${undo}_${depth}.json"
    assert ensure-dir "$undo" @RET

    nomino --generate "$undo" --depth "$depth" --dir "$dir" --print --mkdir "${@[2,-1]}"

    # --regex : idk what this does really
    # --test : dry-run
}
##
function mv-date() {
    local f="$1"
    assert test -e "$f" @RET

    local ext="$(ecn $f | rget '(\.[^.]*$)')"

    gmv -v -i "$f" "${f:r}_$(gdate +"%Y-%b-%d")${ext}"
}
reify mv-date
##
