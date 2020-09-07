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
}
aliasfn weed w2e-dl
##
