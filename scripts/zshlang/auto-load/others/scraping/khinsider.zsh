##
function khinsider-urls-mp3 {
    #: `p khinsider-urls-mp3 @f pxa aa -Z`
    ##
    local urls=("$@")
    assert-args urls @RET

    eval-memoi getlinks-c -e '\.mp3$' "${urls[@]}"  |
        gsort -u |
        prefixer --skip-empty |
        memoi_key="$urls[*]" reval-true eval-memoi @opts halt never @ para retry gurl |
        command pup 'audio attr{src}' |
        gsort -u
}
##
