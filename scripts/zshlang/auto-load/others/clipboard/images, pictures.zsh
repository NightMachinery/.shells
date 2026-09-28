function imgpop() {
    # @darwinonly
    imgcat =(pngpaste -)
}

function maccy-paste-images {
    : "usage: maccy-paste-images [n=1]
Save the most recently copied n images from Maccy to the current directory."

    local home="${HOME}" nightdir="${NIGHTDIR}"
    local db="${home}/Library/Containers/org.p0deje.Maccy/Data/Library/Application Support/Maccy/Storage.sqlite"

    if (( $# > 1 )) ; then
        ecerr "$0: expected at most one image count"
        return 2
    fi
    if ! isDarwin ; then
        ecerr "$0: Maccy is available only on macOS"
        return 1
    fi
    ensure-cmd python3 @RET

    command python3 "${nightdir}/python/maccy_paste_images.py" "${1-1}" "${db}" "${PWD}"
}
