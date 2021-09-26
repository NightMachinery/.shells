##
get-dl-link() {
    print -rn -- "${dl_base_url}/$(realpath --relative-to ~/Downloads "$1")"|url-encode.py
}
##
function scp-lilf() {
    (( $#@ >= 2 )) || return 1
    ensure-net @MRET
    local files=(${@[1,-2]}) o="${@[-1]}"
    ensure-args files o @MRET

    scp -r "$files[@]" ${lilf_user}@82.102.11.148:/home/${lilf_user}/"$o"
}
aliasfn scpeva scp-lilf
##
