function zopen() {
    local f files=("$@")
    for f in $files[@] ; do
        test -e "$f" || {
            ecerr "Nonexistent file: $f"
            continue
        }
        local ext="${f:e}" usemime=''
        case "$ext" in
            wav) hearinvisible "$f" ;;
            zip|rar|7z) unzip2dir "$f" ; bello ;;
            mobi|epub|azw*) awaysh ebook-viewer "$f" ;;
            *) usemime=y ;;
        esac
        if test -n "$usemime" ; then
            local m="$(mimetype2 "$f")"
            case "$m" in
                audio*) hearinvisible "$f" ;;
                */epub*) awaysh ebook-viewer "$f" ;;
                *) fsay "Unsupported file: $m" ;;
            esac
        fi
    done
    # sleep 20
}
aliasfn zop zopen
