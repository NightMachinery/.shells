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
    fnswap z-add true cdm "$1"
}
popf() {
    fnswap z-add true cd "${pushf_stack[-1]}"
    pushf_stack=("${(@)pushf_stack[1,-2]}")
}
##
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
    command progress --additional-command gcp --additional-command gmv --additional-command gcat --additional-command gdd --additional-command curl --additional-command aria2c --additional-command wget --additional-command ffmpeg --additional-command youtube-dl --monitor "$@" # --monitor-continuously
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
function list-dirs() {
    local d=(${@}) multi_fs="${list_dirs_m}" # multi = disable one-file-system
    local depth="${list_dirs_d}"
    typeset -ag list_dirs_fd
    local opts=( "${list_dirs_fd[@]}" )

    if test -z "$multi_fs" ; then
        opts+="--one-file-system"
    fi
    if test -n "$depth" ; then
        opts+=(--max-depth "$depth")
    fi
    arr0 $d[@] | filter0 test -d | inargs0 fd "$opts[@]" --follow --absolute-path --type d .
}
function list-dirs-d1() {
    ## @profile
    # `hyperfine --shell "brishz_para.dash" --warmup 5 --export-markdown=$HOME/tmp/hyperfine.md 'list-dirs-d1 ~/' 'arrN ~/*(/N)'`
    #     Benchmark #1: list-dirs-d1 ~/
    #   Time (mean ± σ):      33.8 ms ±  14.2 ms    [User: 0.7 ms, System: 0.6 ms]
    #   Range (min … max):    21.0 ms …  77.8 ms    22 runs
    #
    # Benchmark #2: arrN ~/*(/N)
    #   Time (mean ± σ):       6.6 ms ±  15.0 ms    [User: 1.0 ms, System: 0.7 ms]
    #   Range (min … max):     0.0 ms …  58.7 ms    28 runs
    #
    # Summary
    #   'arrN ~/*(/N)' ran
    #     5.14 ± 11.92 times faster than 'list-dirs-d1 ~/'
    ##
    list_dirs_d=1 list-dirs "$@"
}
function list-dirs-parents() {
    : "No realpath is used"

    local i="$1" p
    p="${i:h}"
    if test -n "$p" && [[ "$i" != "$p" ]] ; then
        ec "$p"
        "$0" "$p"
        return $?
    fi
}
##
function trs-empty-files() {
    local d="${1}"
    assert-args d @RET

    gfind "$d" -empty | inargsf trs
}
##
function h_path-abbrev() {
    @inargsf

    local dir="$1"
    assert test -d "$dir" @RET

    pushf "$dir"
    {
        eval 'ec ${(%):-%~}' # supports named directories
    } always { popf }
}
aliasfn path-abbrev fnswap z-add true h_path-abbrev # using 'cd' triggers z-add which causes an infinite loop
aliasfn path2tilde path-abbrev

function path-unabbrev() {
    @inargsf

    arrN ${~@} # do NOT quote this
}
aliasfn tilde2path path-unabbrev


function path-abbrev-simple () {
    ec "$(in-or-args "$@")" | perl -lpe 's/^\Q$ENV{HOME}\E/~/g'
    ## tests:
    # `path2tilde $PWD | tee /dev/tty | tilde2path`
    # `path2tilde $PWD | tee /dev/tty |HOME='$0' tilde2path`
    ##
}

function path-unabbrev-simple () {
    ec "$(in-or-args "$@")" | perl -lpe 's/^~/$ENV{HOME}/g'
}
##
