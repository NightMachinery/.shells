##
function comic-print() {
    local f enhance="${comic_print_e:-y}"
    f=("${(@f)$(finder-sel-get)}") @RET


    if ! test -d "$d" ; then
        assert unzip2dir "$f[@]" @RET
    fi

    local i d
    for i in "${f[@]}" ; do
        d="$i"
        if ! test -d "$d" ; then
            d="${i:r}"
        fi
        if ! test -d "$d" ; then
            ecerr "$0: Dir $(gq "$d") does not exist. Aborting."
            return 1
        fi
        {
            assert pushf $d @RET
            ecgray "Entered $(gq "$d")" >&2

            ## enhancing the images:
            # https://legacy.imagemagick.org/Usage/resize/
            # https://legacy.imagemagick.org/Usage/filter/
            ##
            if bool $enhance ; then
                local de="${d}/enhanced"
                assert mkdir -p $de @RET
                local p o
                for p in ${~imageglob} ; do
                    o="$de/${p:r}.png"
                    if ! test -e "$o" ; then
                        assert reval-ec superres-anime-image "$d/$p" "$o" -f png @RET
                    else
                        ecerr "$0: File already exists, skipping: $(gq "$o")"
                    fi
                done
                assert cd "$de" @RET
            fi
            ##

            arrN ${~imageglob} | gsort >&1 >&2 | inargsf re "assert grealpath -e"
        } always { popf }
    done | sponge | inargsf open || {
        retcode
        return 1
    }

    tts-glados1-cached "Processing, complete"
}
alias manga-print='comic-print'
##
