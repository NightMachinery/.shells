function zir {
    local dest="${$(removeTrailingSlashes "$1"):r}$2".zip
    \rm "$dest" &> /dev/null
    zip -r "$dest" "$1"
}

function unzip2dir() {
    local file="$1" y="${unzip2dir_y:-y}"
    file="$(grealpath -e "$file")" @TRET

    local head
    local opts=()
    if [[ "$file" == *.rar ]] && test -n "${commands[unrar]}" ; then
        head='unrar'
        opts+=(-ad)
        # -ad            Append archive name to destination path
    else
        head='7z'
        opts+=(-o"${file:r}/")
    fi

    if bool $y ; then
        opts+='-y'
        # -y     Assume Yes on all queries (works with both 7z and unrar)
    fi

    assert reval-ec "$head" x "$opts[@]" "$file" @RET
    ##
    # unzip "$file" -d "${file:r}/"
    ##
}
reify unzip2dir
aliasfn uzd unzip2dir
##
