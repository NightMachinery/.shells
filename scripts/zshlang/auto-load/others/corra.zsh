get-dl-link() {
    print -rn -- "${dl_base_url}/$(realpath --relative-to ~/Downloads "$1")"|url-encode.py
}
scpeva() {
    ensure-net @MRET
    scp -r "$@" eva@82.102.11.148:/home/eva/
}
