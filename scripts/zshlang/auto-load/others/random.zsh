##
aliasfn cat-random cat_random.pl
aliasfn cat-rnd cat-random

# aliasfn cat-random-n cat_random.py
function cat-random-n {
    local n="${1:-1}"
    silent shift || true

    gshuf -n "$n" -- "$@"
}
##
function random-int {
    mdoc "Usage: $0 <from> <to> <how-many>
Doesn't output duplicates. (Outputs permutations.)
The from-to range is inclusive." MAGIC
    local from="$1" to="$2" n="${3:-1}"
    assert-args from to n

    shuf -i "${from}-${to}" -n "$n"
}
aliasfn rndint random-int

function random-int2 {
    command dd if=/dev/urandom count=1 2> /dev/null |
        command cksum |
        awkn 1
}
##
function array-random {
    if (( ${#@} == 0 )) ; then
        return 1
    fi

    local i="$(rndint 1 ${#@})"
    ec "${@[$i]}"
}
aliasfn rndarr array-random
aliasfn arrrnd array-random
aliasfn arr-rnd array-random
##
function rnd-b64() {
    doc "<BYTE-LENGTH> [<OUTPUT-FORMAT>=base64,hex]"
    openssl rand -"${2:-base64}" "$1"
}
