##
function onedrive-dl-link {
    local urls=($@)

    if (( ${#urls} >= 1 )) ; then
        re onedrive_dl.py "${urls[@]}" @RET
    fi
}

function onedrive-dl {
    : "transformer url-final3 onedrive-dl ..."

    local urls=($@)

    onedrive-dl-link "${urls[@]}" | inargsf reval-ec aa-gateway -Z
}
##
