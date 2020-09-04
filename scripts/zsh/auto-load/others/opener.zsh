function zopen() {
    local f="$*"
    local ext="${f:e}"

    test -z "$f" && return 1
    case "$ext" in
        wav) hearinvisible "$f" ;;
        *) fsay "Unsupported file" ;;
    esac
    sleep 20
}
aliasfn zop zopen
