###
alias imd='img2md-imgur'
###
function jrlt() {
    local today="$(date +"%Y.%b.%d") $(datej|tr / -)"
    local dest="$nightJournal/$today.md"
    test -e "$dest" || {
        ec "# $today"$'\n\n'"* " >> $dest
    }
    ! isI || $EDITOR[@] $dest
}
##
function img2md() {
    mdoc "$0 <picture-file> [<description>]
Outputs the image in markdown format, hardcoded in base64. Large images (~0.3 MB) will probably crash the system though." MAGIC

    jglob

    local file="$1" desc="$2"
    local compressed="$(gmktemp --suffix .jpg)" # ".${file:e}"
    convert $file -define jpeg:extent=150kb $compressed # 200kb didn't work
    file=$compressed
    doc use base64 from brew to ensure consistency
    ## python base64 (might work in ${lilf_user}):
    # encoded_string= base64.b64encode(img_file.read())
    # print(encoded_string.decode('utf-8'))

    # somehow breaks in eva_aget ...
    print -r -- "![$desc](data:$(file -b --mime-type $file);base64,$(base64 "$file" | tr -d '\r\n'))"
}

function img2md-imgur() {
    mdoc "$0 <picture-file> [<description>]
Outputs the image in markdown format, hosted on imgur." MAGIC

    jglob
    local file="$1" desc="$2"

    print -r -- "![$desc]($(imgurNoD=y imgur.bash $file))"
}
##
function unt() {
    local engine=("${(@)url2note_e:-url2org}")

    test -z "$*" && set -- "$(pbpaste)" # || set -- "$(trim "$1")" # we are using urls-extract so ne need
    local note="$(cleanedhtml=no "${engine[@]}" "${(f@)$(<<<"$*" urls-extract)}")"
    ec $note
    if isI ; then
        pbcopy $note
    fi
}
@opts-setprefix unt url2note
noglobfn unt
aliasfn-ng untr brishzr unt # the server can do a unt almost instantaneously, so we will benefit from proxying to it. Besides, this bypasses the small firewall.

function unt-md {
    url2note_e=(url2md) unt "$@"
}
noglobfn unt-md
aliasfn-ng untr-md brishzr unt-md
##
