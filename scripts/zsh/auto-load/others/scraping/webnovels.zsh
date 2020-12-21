function novelfull-2e() {
    local l="${1:?}" page="${2:?}"
    local title="P${page} ƪ ${3:-$(url-title $l)}"

    getlinks-c "${l}?page=${page}" | rg chapter  | gsort -V | ghead -n -4 | inargsf w2e "$title"
}
