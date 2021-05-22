function gh-release-get() {
    local url="$1"
    local repo="$url"
    local tag="${gh_release_get_tag}"
    assert-args repo @RET
    if [[ "$repo" =~ '(?:https://)?github.com/([^/]+/[^/]+)' ]] ; then
        repo="$match[1]"
        # dvar repo
    fi
    local desired="${gh_release_get_desired}" # regex
    if test -z "$desired" ; then
        if isDarwin ; then
            # desired="macos"
            desired="mac|darwin"
        else
            desired="linux|ubuntu"
        fi
    # elif [[ "$desired" == . ]] ; then
    #     desired=''
    fi

    local releases
    releases="$(revaldbg gurl https://api.github.com/repos/"$repo"/releases)" @TRET

    if test -z "$tag" ; then
        if [[ "$url" =~ 'tag/([^/?]+)' ]] ; then
            tag="${match[1]}"
        fi
        if test -z "$tag" ; then
            tag="$(ec "$releases" | jq -r ".[].tag_name"  | gsort --version-sort --reverse | ghead -n 1)" @TRET
            # @warn some repos have messed up version numbers where this won't work.
        fi
    fi
    ecgray "$0: selected tag: $(gq "$tag")"

    local assets
    assets="$(ec $releases | jq -r --arg tag "$tag" '.[] | if .tag_name == $tag then .assets else empty end' )" @TRET

    local my_asset
    my_asset="$(ec $assets | jq -r --arg desired "$desired" '.[] | select(.name | test($desired)) | .browser_download_url' | fz -1)" @TRET
    # my_asset="$(ec $assets | jq -r --arg desired "$desired" '.[] | select(.name | contains($desired)) | .browser_download_url' | fz -1)" @TRET

    local d="$HOME/.cache/dl/gh/$repo/"
    mkdir -p "$d"
    pushf "$d"
    {
        $proxyenv assert reval-ec aa-gateway "$my_asset" @RET
        unzip2dir_y=y assert unzip2dir ${~archiveglob} @RET

        bell-dl
    } always {
        # popf
    }
}
