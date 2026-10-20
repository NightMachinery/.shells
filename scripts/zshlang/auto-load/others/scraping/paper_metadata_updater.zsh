##
function org-paper-metadata-updater {
    : "usage: org-paper-metadata-updater [--write] [--force] <directory-root>
Refresh existing Org paper headings, skipping metadata fetched within seven days."
    h-org-paper-tools-dep @RET
    local module_dir="${NIGHTDIR}/golang/org-paper-metadata"
    local helper_bin="$(whence -p org-paper-metadata)"
    local updater_bin="$(whence -p org-paper-metadata-updater)"
    #: The root helper watches the entire shared module. Its newer binary also
    #: invalidates this command when a shared package changes.
    if [[ -z "${updater_bin}" || "${helper_bin}" -nt "${updater_bin}" ]] ; then
        ( builtin cd -- "${module_dir}" && command go install ./cmd/org-paper-metadata-updater ) @RET
        rehash
    fi
    local -x SemanticScholar_API_key="${SemanticScholar_API_key}"
    local -x deepseek_api_key="${deepseek_api_key}"
    local -x semantic_scholar_api_p="${semantic_scholar_api_p}"
    local -x semantic_scholar_scraper="${semantic_scholar_scraper:-auto}"
    local -x fhMode="${fhMode}"
    local line
    command org-paper-metadata-updater "$@" 2> >(while IFS= read -r line ; do ecgray "${line}" ; done)
}
##
