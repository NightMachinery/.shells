function underline-dl {
    local url="$1"

    local proxy_disabled=y youtube_dl_c=y

    local info title dir slide_url=''
    info="$(youtube-dl --dump-json "$url")" @TRET
    title="$(ec "$info" | jqm '.title')" @TRET
    dir="./$(ec "$title" | str2filename)" @TRET

    slide_url="$(ec "$info" | jqm '.slide_info[0].url')" || true

    assert pushf "$dir"/ @RET
    {
        { ec "$info" | jq . > info.json } @TRET

        if test -n "$slide_url" ; then
            reval-ec aa-remotename "$slide_url"
        fi

        assert youtube-dl-format-fz -o '%(title)s.%(ext)s' "$url" @RET
    } always { popf }
}
