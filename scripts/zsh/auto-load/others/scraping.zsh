function wread() {
    local file=''
    [[ "$1" == '--file' ]] && {
        test -e "$2" && file="$2" || return 33
        shift 2
    }
    setopt local_options pipefail
    local title author
    test "${2:=markdown}" = 'html' && title='"<h1>"+.title+"</h1>"' || title='"# "+.title'
    test "${2}" = 'html' && author='"<p>By: <b>"+.author+"</b></p>"' || author='"By: **"+.author+"**"'
    { test -z "$file" && { # No file, downloading
          test -z "$wr_force" && mercury-parser --format="${2}" "$1" || {
                  fu_wait="${fu_wait:-60}" aget "full-html $1:q ./a.html
# l
# cat ./a.html
mercury-html $1:q ./a.html $2:q"
              } } || {
          # File supplied
          aget "cat ${(q@)file} > a.html ; mercury-html $1:q ./a.html $2:q"
      } } |jq -e --raw-output 'if .content then [
    (if .title then '"$title"' else empty end),
    (if .author then '"$author"' else empty end),
    .content
] | join("\n\n") else null end'
}
function mercury-html() {
    doc USAGE: url html-file output-mode
    serr mercury-html.js "$@"
}
function full-html() {
    doc splash should be up. https://splash.readthedocs.io
    doc 'wait always waits the full time. Should be strictly < timeout.'
    curl --silent "http://localhost:8050/render.html?url=$1&timeout=90&wait=${fu_wait:-10}" -o "$2"
}
function random-poemist() {
    curl -s https://www.poemist.com/api/v1/randompoems |jq --raw-output '.[0].content'
}
xkcd() wget `wget -qO- dynamic.xkcd.com/comic/random | sed -n 's/Image URL.*: *\(\(https\?:\/\/\)\?\([\da-z\.-]\+\)\.\([a-z\.]\{2,6\}\)\([\/\w_\.-]*\)*\/\?\)/\1/p'`
wayback() {
    comment -e, --exact-url
    comment "-t, --to TIMESTAMP Should take the format YYYYMMDDhhss, though
                        you can omit as many of the trailing digits as you
                        like. E.g., '201501' is valid."
    comment '-p, --maximum-snapshot NUMBER    Maximum snapshot pages to consider (Default is 100)'
    comment '-d, --directory PATH             Directory to save the downloaded files into
    Default is ./websites/ plus the domain name'

    wayback_machine_downloader -e -t "${wa_t:-20170505152803}" -d ./ -p 1 "$@"
}
wayback-out() {
    aget "wayback $(gquote "$@") ; cat *.html"
}
wread-wayback() {
    wayback-out "$1" | wread --file /dev/stdin "$@"
}
wayback-url() {
    waybackpack --to-date "${wa_t:-2017}" --list "$@" |tail -n1
}
w2e-curl() {
    h2ed=html2epub-pandoc-simple we_dler=wread-curl w2e "$@"
}
wread-curl() {
    gurl "$1"
}
w2e-gh() {
    w2e-curl "$1" "${(@f)$(gh-to-readme "${@:2}")}"
}
gh-to-readme() {
    local urls=() i i2 readme url
    for i in "$@"
    do
        ! [[ "$i" =~ 'github.com' ]] || [[ "$i" == *.(md|rst) ]] ||
            {    i2="${i}.md"
                 comment we hope to handle wiki pages with method, but beware that nonexistent wiki pages trigger create a new page, not the desired not existent response.
                 url-exists "$i2" ||
                     { i2="${i}/blob/master/README.md"
                       url-exists "$i2" ||
                           { i2="${i}/blob/master/README.rst"
                             url-exists "$i2" ||
                                 { i2="${i}/blob/master/readme.md"
                                   url-exists "$i2" ||
                                       { i2="${i}/blob/master/readme.rst"
                                         url-exists "$i2" ||
                                             {
                                                 for readme in "${(0@)$(permute-case readme)}"
                                                 do
                                                     i2="${i}/blob/master/${readme}.md"
                                                     url-exists "$i2" && break
                                                     i2="${i}/blob/master/${readme}.rst"
                                                     url-exists "$i2" && break
                                                 done
                                             } } } } }
                 i="$i2"
            }
        url-exists "$i" && urls+="$i" || color red "$i does not seem to exist." >&2
    done
    rex 'rgx _ blob raw' "$urls[@]"
}
