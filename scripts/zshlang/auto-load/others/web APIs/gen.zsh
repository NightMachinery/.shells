function cov() { # covid
    gurl https://coronavirus-19-api.herokuapp.com/countries/"${1:-iran}" | jq .
}
##
function code2img-url() {
    local lang="${1:-shellscript}" theme="${2:-solarized-dark}" mode="${code2img_mode:-url}"
    local endpoint
    if [[ "$mode" == url ]] || isOutTty ; then
       endpoint='https://sourcecodeshots.com/api/image/permalink'
   else
       endpoint='https://sourcecodeshots.com/api/image'
    fi

    gfold -w100 | jq --raw-input --slurp --null-input --arg lang "$lang" --arg theme "$theme" 'inputs as $i | {
  "code": $i,
  "settings": {
    "language": $lang,
    "theme": $theme
  }
}' | $proxyenv revaldbg curl --fail --silent --location --header "Content-Type: application/json" --data '@-' "$endpoint"
}
@opts-setprefix code2img-url code2img
function code2img() {
    : "Usage: @opts o OUTPUT ... @ code2img [lang] [theme] ..."
    local o="${code2img_o}"
    if test -z "$o" ; then
        isBorg && local -x TMPDIR="."
        o="$(gmktemp --suffix .png)"
        ec "$0: output set to '$o'" >&2
    elif test -e "$o" ; then
       ecerr "$0: destination exists: $o ; aborting"
       return 1
    fi
    @opts mode img @ code2img-url "$@" > "$o"
    pbadd "$o"
}
##
