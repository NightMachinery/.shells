function realpath-ife() {
    if test -e "$1" ; then
        realpath "$1"
    else
        ec "$1"
    fi
}
reify realpath-ife
function get-tmpdir() {
    dirname "$(gmktemp -u)"
}
function prefix-files() {
    for file in "${@:2}"
    do
        mv "$file" "${file:h}/$1${file:t}"
    done
}
function globexists() {
    (silent eval ": $@")
}
noglobfn globexists

ensure-empty() {
    doc Use 'jee' in code.
    local dir="$PWD"

    if ! dir-isempty "$dir" ; then
        ecerr Directory "$dir" not empty
        return 1
    fi
}
typeset -ag pushf_stack
pushf() {
    # mkdir -p "$1"
    pushf_stack+="$(pwd)"
    cdm "$1"
}
popf() {
    cd "${pushf_stack[-1]}"
    pushf_stack=("${(@)pushf_stack[1,-2]}")
}
function rename-numbered() {
    mdocu "rfp_dry=<dry-run?> <to-dir> <file> ..." MAGIC
    local c=1
    local i
    for i in "${@:2}"
             {
                 local dest="$1/$(printf '%03d' $c) ${i:t}"
                 echo "$i to "  $dest
                 [[ -z "$rfp_dry" ]] && mv "$i" $dest
                 c=$(($c + 1))
             }
}
function filesize() {
    magic mdocu '<file>
returns size in bytes.' ; mret

    local FILENAME="$1"
    test -e "$FILENAME" || { ecerr "File i$FILENAME doesn't exist." ; return 1 }

    local SIZE="$(gdu -sb $FILENAME | awk '{ print $1 }')"
    ec $SIZE
}
function filesizereal() {
    local file="$1"
    test -e "$file" || { ecerr "File $file doesn't exist." ; return 1 }
    local zerobytes
    # zerobytes=$(( $( ggrep -aPo '\0*$' $file | wc -c ) - 1 ))
    zerobytes="${$(trailingzeroes.rs $file)}"
    ec $(( ${$(filesize $file):-0} - $zerobytes )) 
}
function check-for-partial-files() {
    local dir="${1:-.}"
    pushf $dir
    { lm | serr inargsf re 'labeled trailingzeroes.rs' } always { popf }
}
function till-file() {
    local file="$1"
    local time="${2:-30}"

    while true ; do
        test -e "$file" && { 
            command rm "$file"
            break
        }
        ecerr "$0: File '$file' does not yet exist ..."
        sleep "$time" 
    done
}
function ext-all() {
    # useful for, e.g., seeing what to track with git lfs
    fd . | inargsf mapg '${i:e}' | gsort --uniq
}
##
function vidir() {
    # org has the right comment syntax (to delete), and can autocomplete well
    edir --all --trash --suffix .org "$@"
}
##
function progress() {
    # @wrapper
    command progress --additional-command gcp --additional-command gmv --additional-command gcat --additional-command gdd --additional-command curl --additional-command aria2c --additional-command wget --monitor "$@" # --monitor-continuously
}
aliasfn prg progress
##
function fd-count() {
    local dir="$PWD"
    fd -uuu . "$dir" | wc -l
}
function dir-isempty() {
    # @alt `! (silence eval '\: *(D)')`
    local dir="$PWD"
    local c
    c="$(fd-count "$dir")" @RET
    (( c == 0 ))
}
##
