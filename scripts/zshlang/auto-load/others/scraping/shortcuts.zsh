##
function h2e-stdin() {
    h2e "$@" =(</dev/stdin)
}
function w2e-rpaste() {
    : "Alt: w2e-dl"
    local html="$(pbpaste)"
    brishz_in="$html" brishzr h2e-stdin "$@"
}
# aliasfn weep w2e-rpaste
##
function w2e-dl() {
    pushf ~/tmp/
    {
        local file=( ~/Downloads/*.html(om) ) # Let it fail
        file=$file[1]
        ishtml-file "$file" || {
            return 1
        }
        local name="${file:t:r}"
        test -z "$name" && {
            ecerr "$0: Empty name for file: $file"
            return 1
        }
        reval-ec h2e "$name" "$file"
    } always { popf }
}
aliasfn weed w2e-dl

function p-w2e-browser-html-links {
    local title="$1"
    if test -z "$title" ; then
        title="$(browser-current-title)" @TRET

        ecgray "$0: title automaticaly set to $(gquote-sq "$title")"
    fi

    p-getlinks | inargsf rgeval w2e "$title"
}
##
