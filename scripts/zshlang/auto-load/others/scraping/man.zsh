# * https://superuser.com/questions/1654879/how-do-i-convert-manpages-info-files-to-epub
#
# - man2epub.bash
#
# - https://github.com/nerab/manbook
#
# - Search the web for HTML versions of the manpages
#   + https://docs.oracle.com/cd/E36784_01/html/E36870/zshall-1.html
#
# - info2html (I couldn't find this program, actually)
#
# - https://github.com/linux-test-project/lcov/blob/master/bin/genhtml
# This script generates HTML output from .info files as created by the
#   geninfo script. Call it with --help and refer to the genhtml man page
#   to get information on usage and available options.
##
function wread-man() {
    local m=""
    m="$(MAN_KEEP_FORMATTING=1 COLUMNS=70 serr man "$1")" && m="$(<<<"$m" command ul)" || m="$(2>&1 "$1" --help)" || { ecerr "$0 failed for $1" ; return 1 }
    <<<"$m" aha --title "$1"
}

function tlman() {
    # man2epub
    uf_idem=y we_dler="wread-man" w2e "$1" "$@"
}
##
