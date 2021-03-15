get-dl-link() {
    print -rn -- "${dl_base_url}/$(realpath --relative-to ~/Downloads "$1")"|url-encode.py
}
scpeva() {
    (( $#@ >= 2 )) || return 1
    ensure-net @MRET
    local files=(${@[1,-2]}) o="${@[-1]}"
    ensure-args f o @MRET

    scp -r "$files[@]" eva@82.102.11.148:/home/eva/"$o"
}
