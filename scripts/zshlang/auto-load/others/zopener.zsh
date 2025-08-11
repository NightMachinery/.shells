##
function zopen-audio {
    local f="$1"
    assert-args f @RET

    hear-loadfile "$f"
    # hearinvisible "$f"
}

function zopen-ebook {
    local f="$1"
    assert-args f @RET

    ##
    # awaysh ebook-viewer "${f}"
    ##
    open-sioyek "$f"
    #: [[id:4e51667e-8f0b-482a-8d1f-5962857cfa72][sioyek/EPUB]]
    ##
}

function zopen-text {
    local f="$1"
    assert-args f @RET

    if isTty ; then
        emc-nowait2 "$f"
    else
        withemcgui emc-nowait2 "$f"
    fi
}

function zopen {
    local f files=("$@") bell=''
    for f in $files[@] ; do
        local f_orig="$f"
        if ! test -e "$f" ; then
            f="$(path-unabbrev "$f")" @TRET

            if ! test -e "$f" ; then
            ecerr "$0: nonexistent file: ${f_orig}"
            continue
            fi
        fi

        local ext="${f:e:l}" usemime=''
        
        local text_exts="(${(j.|.)text_formats})"
        local video_exts="(${(j.|.)video_formats})"
        
        case "$ext" in
            wav|mp2|mp3|mp3test|m4a|ogg|au|flac)
                zopen-audio "$f"
                ;;
            zip|rar|7z) unzip2dir "$f" ; bell=y ;;
            mobi|epub|azw*) zopen-ebook "$f" ;;
            cbz) awaysh mpv-manga "$f" ;;
            pdf)
                open-sioyek "$f"
                # chrome-open-pdf "$f"
                ;;
            ${~text_exts})
                zopen-text "$f"
                ;;
            ${~video_exts}) awaysh mpv "$f" ;;
            # *) re typ text_exts ext f ; return 0 ;;
            *) usemime=y ;;
        esac
        if test -n "$usemime" ; then
            local m="$(mimetype2 "$f")"
            case "$m" in
                inode/x-empty) ecerr "$0: empty file" ;;
                audio*) zopen-audio "$f" ;;
                */epub*) zopen-ebook "$f" ;;
                
                *) fsay "Z-Open: unsupported file: $m" ;;
                # *) open "$f" ;;
                # Using =open= will cause an infinite loop if =zopen= is the file's default opener!
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
##
