function comic-print() {
    local f
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
        assert cd $d @RET
        ecgray "Entered $(gq "$d")" >&2

        ## enhancing the images:
        # https://legacy.imagemagick.org/Usage/resize/
        # https://legacy.imagemagick.org/Usage/filter/
        ##

        arrN ${~imageglob} | gsort >&1 >&2 | inargsf re "grealpath -e"
    done | sponge | inargsf open
}
