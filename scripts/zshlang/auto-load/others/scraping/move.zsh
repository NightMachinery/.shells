##
function image2here {
    local to="${1}"

    local target_dir="${image2here_dir}"
    if test -z "${target_dir}" ; then
        target_dir=~dl/
    fi

    local image
    image="$(indir "${target_dir}"  last-created)" @RET

    if ! (( ${image_formats[(Ie)${image:e:l}]} )); then
        # =${image:e}= gets the extension
        # =:l= lowercases it, so =PNG= also works
        # =[(Ie)...]= checks whether that exact string exists in the array
        # the result is an index, so nonzero means “found”
        ##
        ecerr "$0: '${image:t}' is not a valid image file"
        return
    fi

    to="${to:-${image:t}}"
    if ! [[ "${to:e}" == "${image:e}" ]] ; then
        to="${to}.${image:e}"
    fi

    local dest="${PWD}/${to}"

    ##
    #
    # if [[ "${dest:t}" =~ 'ChatGPT.*' ]] ; then
    #     #: @retired auto rename to number
    # fi
    ##

    reval-ecgray mv -i "${target_dir}/${image}" "${dest}" @RET

    icat "${dest}" || true
}
##
