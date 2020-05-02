get-dl-link() {
    print -rn -- "${dl_base_url:-http://lilf.ir:8080}/$(realpath --relative-to ~/Downloads "$1")"|url-encode.py
}
scpeva() {
    scp -r "$@" eva@82.102.11.148:/home/eva/tmp/
}
