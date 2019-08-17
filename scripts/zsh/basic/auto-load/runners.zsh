function rexx(){
    xargs -d " " -n 1 -I _ "$=1" <<< "${@:2}"
}
function rex(){
    zargs --verbose -i _ -- "${@:2}" -- "$=1"
    #Using -n 1 fails somehow. Probably a zargs bug.
}
function rexa(){
    local i
    for i in "${@:2}"
    do
        eval "$(sed -e "s/_/${i:q:q}/g" <<< "$1")" #sed itself needs escaping, hence the double :q; I don't know if this works well.
    done
}
redoq() {
    local i
    for i in {1.."${@: -1}"}
    do
        eval "${@: 1:-1}"
    done
}
redo() redoq "$(gquote "${@: 1:-1}")" "${@: -1}"
