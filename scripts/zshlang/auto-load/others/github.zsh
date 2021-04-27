function gh-release-get() {
    local repo="$1"
    assert-args repo @RET
    local desired="$2"
    if test -z "$desired" ; then
        if isDarwin ; then
            desired="macos"
        else
            desired="ubuntu"
        fi
    elif [[ "$desired" == . ]] ; then
        desired=''
    fi

    local releases
    releases="$(revaldbg gurl https://api.github.com/repos/"$repo"/releases)" @TRET

    local tag
    tag="$(ec "$releases" | jq -r ".[].tag_name"  | gsort --version-sort --reverse | ghead -n 1)" @TRET

    local assets
    assets="$(ec $releases | jq -r --arg tag "$tag" '.[] | if .tag_name == $tag then .assets else empty end' )" @TRET

    local my_asset
    my_asset="$(ec $assets | jq -r --arg desired "$desired" '.[] | select(.name | contains($desired)) | .browser_download_url')" @TRET

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
