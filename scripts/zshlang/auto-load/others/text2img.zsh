##
function preprompts-expand {
    local model="${codex_model}" reasoning_effort="${codex_reasoning_effort:-high}" color="${codex_color:-always}"

    local preprompts_dir="${1}"
    local out_dir="${2:-prompts}"
    assert-args preprompts_dir @RET

    mkdir -p "${out_dir}" @RET
    out_dir="${out_dir:A}"  #: absolute path

    (
        cd "${preprompts_dir}" @RET
        local inputs
        inputs="$(fd-m --extension .md)" @RET
        inputs=( ${(f@)inputs} )

        local i name dest
        for i in ${inputs[@]} ; do
            name="${i:r}"
            dest="${out_dir}/${name}.md"

            if test -s "${dest}" ; then
                ecgray "${0}: skipping ${dest} (already exists)"
                continue
            fi

            if { cat "${i}" |
                     codex-ask | sponge "$dest" } ; then
                ecgray "${0}: saved ${dest}"
            else
                ecerr "${0}: failed to process ${preprompts_dir}/${i}->${dest}; continuing"
            fi
        done
    )
}
##
function cat-files-to-lines {
    #: This function is used to aggregate prompts from multiple files, ensuring each prompt is separated by a newline.
    ##
    local -a inargs
    in-or-args3 "$@" @RET

    local f
    for f in ${inargs[@]} ; do
        if cat "${f}" |
                newline2space ; then
            ec
        fi
    done | prefixer --skip-empty
}
##
