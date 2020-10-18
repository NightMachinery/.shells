function zopen() {
    local f files=("$@") bell=''
    for f in $files[@] ; do
        test -e "$f" || {
            ecerr "Nonexistent file: $f"
            continue
        }
        local ext="${f:e:l}" usemime=''
        case "$ext" in
            wav|mp2|mp3|mp3test|m4a|ogg|au|flac) hearinvisible "$f" ;;
            zip|rar|7z) unzip2dir "$f" ; bell=y ;;
            mobi|epub|azw*) awaysh ebook-viewer "$f" ;;
            cbz) awaysh mpv-manga "$f" ;;
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
    test -n "$bell" && bello
    # sleep 20
}
aliasfn zop zopen
