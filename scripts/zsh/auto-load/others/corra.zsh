get-dl-link() {
    print -rn -- "${dl_base_url:-http://lilf.ir:8080/}$(realpath --relative-to ~/Downloads "$1")"|url-encode.py
}
