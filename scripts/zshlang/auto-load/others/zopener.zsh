function zopen() {
    local f files=("$@") bell=''
    for f in $files[@] ; do
        test -e "$f" || {
            ecerr "Nonexistent file: $f"
            continue
        }
        local ext="${f:e:l}" usemime=''
        
        local text_exts="(${(j.|.)text_formats})"
        
        case "$ext" in
            wav|mp2|mp3|mp3test|m4a|ogg|au|flac) hearinvisible "$f" ;;
            zip|rar|7z) unzip2dir "$f" ; bell=y ;;
            mobi|epub|azw*) awaysh ebook-viewer "$f" ;;
            cbz) awaysh mpv-manga "$f" ;;
            pdf) chrome-open-pdf "$f" ;;
            ${~text_exts}) emc-nowait2 "$f" ;;
            # *) re typ text_exts ext f ; return 0 ;;
            *) usemime=y ;;
        esac
        if test -n "$usemime" ; then
            local m="$(mimetype2 "$f")"
            case "$m" in
                audio*) hearinvisible "$f" ;;
                */epub*) awaysh ebook-viewer "$f" ;;
                
                # *) fsay "Unsupported file: $m" ;;
                *) open "$f" ;;
            esac
        fi
    done
    if test -n "$bell" ; then
        bello
    fi
    # sleep 20

    return 0
}
aliasfn zop zopen
