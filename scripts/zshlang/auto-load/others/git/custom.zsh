##
function attr-update {
    local author='FM'
    local src=~cod/uni/AttributionSurvey
    local selected=(
        "${nightNotes}/private/subjects/research/attribution/survey/"
        "${nightNotes}/subjects/ML/interp/concept/CRAFT/"
    )

    for s in "${selected[@]}" ; do
        base="$(basename "$s")"
        dest="${src}/${base}/${author}/"
        mkdir-m "$dest"
        rsp-safe "${s}/" "$dest"
        fd -uuu --absolute-path '^\.gitignore$' "$dest" | file_filter.pl  -e '\.pdf$'
    done
}
##
