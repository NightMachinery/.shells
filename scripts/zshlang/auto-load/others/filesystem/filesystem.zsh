##
function rm-empty {
    local dirs=($@)

    local dir
    for dir in ${dirs[@]} ; do
        fd --type=directory --type=empty . "$dir" | inargsf trs-rm
    done
}

function rm-dir-only-child-v2 {
    local dir="${1:-.}"
    if [[ ! -d "$dir" ]]; then
        ecerr "$0: not a directory or does not exist: $dir"
        return 1
    fi
    dir="${dir:A}" #: realpath

    local d
    for d in "${dir}"/*(/DN) ; do
        rm-dir-only-child-v1 "$d"
        #: This way, we won't actually remove only-child dirs in the current root. E.g., if we have =games/x= and start from =games=, we won't remove =x=.
    done
}

function rm-dir-only-child-v1 {
    local dir="${1:-.}"

    if [[ ! -d "$dir" ]]; then
        ecerr "$0: not a directory or does not exist: $dir"
        return 1
    fi
    dir="${dir:A}" #: realpath

    local children children_dir only_child_dir only_child_dir_content
    while true; do
        revaldbg rm-empty "${dir}"

        children=("${dir}"/*(DN))
        children_dir=("${dir}"/*(/DN))
        #: /: only dirs, N: null glob, D: allow hidden

        if (( ${#children} == 1 && ${#children_dir} == 1 )) ; then
            #: There is an only-child dir here.
            ##
            only_child_dir="${children_dir[1]}"
            only_child_dir_content=("${only_child_dir}"/*(DN))

            if (( ${#only_child_dir_content} >= 1 ))  ; then
                ecgray "$0: getting rid of the only-child dir: ${only_child_dir:t}"

                # if ! ask "Confirm" Y ; then
                #     return 1
                # fi

                reval-ec mv-merge "${only_child_dir_content[@]}" "${dir}"/ @RET
            fi

            revaldbg rm-empty "${dir}"

            continue
        else
            local d
            for d in ${children_dir[@]} ; do
                if [[ "$d" == '.git' ]] ; then
                    continue
                fi

                revaldbg "$0" "$d"
            done

            break
        fi
    done
}
aliasfn rm-dir-only-child rm-dir-only-child-v1
##
function file-unix2uri-rp-v2 {
    in-or-args "$@" |
        inargsf grealpath -- |
        file-unix2uri
}
##
function realpath-ife() {
    if test -e "$1" ; then
        grealpath -- "$1"
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

function till-file {
    local file="$1"
    local time="${2:-30}"
    local del_p="${till_file_del_p}"

    while true ; do
        test -e "$file" && {
            if bool "$del_p" ; then
                command rm -f "$file"
            fi

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
function edir-m {
    #: [[id:0d2bf2a5-c287-4389-b000-83d924f6af9d][bulletmark/edir: Program to rename, remove, and copy files and directories using your editor (vidir alternative)]]
    ##
    edir --all --trash --trash-program rip --suffix .org "$@"
    #: --suffix .org: org-mode has the right comment syntax (to delete), and can autocomplete well
}
aliasfn vidir edir-m
##
function progress-m {
    # @wrapper
    command progress --additional-command gcp --additional-command gmv --additional-command gcat --additional-command gdd --additional-command curl --additional-command aria2c --additional-command wget --additional-command ffmpeg --additional-command youtube-dl --monitor "$@" # --monitor-continuously
}
aliasfn prg progress-m
aliasfn monitor-downloads progress-m
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
    ensure-array list_dirs_fd
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
function h_path-abbrev {
    # * @perf this is way too slow
    # * it doesn't work for non-existent dirs
    #
    # @todo2 We need to reimplement this ourselves, by registering the directories in `aliasdir'. We can then also have a version ala '${my_dir}/...' instead of '~my_dir/...', as well. This version is easier to use when copy-pasting to the terminal.
    ##
    local inargs
    in-or-args3 "$@" @RET

    local f
    for f in ${inargs[@]} ; do
        local dir="${f}" tail=""
        if ! test -d "$dir"; then
            dir="${f:h}"
            tail="${f:t}"
        fi
        assert test -d "$dir" @RET

        pushf "$dir"
        {
            local dir_abbrev
            dir_abbrev="$(eval 'ec ${(%):-%~}')" @TRET #: supports named directories

            if test -n "$tail" ; then
                ec "${dir_abbrev}/${tail}"
            else
                if [[ "${f[-1]}" == '/' ]] ; then
                    ec "${dir_abbrev}/"
                else
                    ec "${dir_abbrev}"
                fi
            fi
        } always { popf }
    done | cat-copy-if-tty
}
aliasfn path-abbrev fnswap z-add true h_path-abbrev # using 'cd' triggers z-add which causes an infinite loop
aliasfn path2tilde path-abbrev

function path-unabbrev {
    local inargs
    in-or-args3 "$@" @RET

    local i head tail
    for i in ${inargs[@]}; do
        if [[ "$i" =~ '^(~[^/]*)(.*)' ]] ; then
            head="${match[1]}"
            tail="${match[2]}"
            ec ${~${head}}"${tail}" # do NOT quote this
        else
            ec "$i"
        fi
    done | cat-copy-if-tty
}
aliasfn tilde2path path-unabbrev

function path-abbrev-simple {
    ec "$(in-or-args "$@")" | perl -lpe 's/^\Q$ENV{HOME}\E/~/g'
    ## tests:
    # `path2tilde $PWD | tee /dev/tty | tilde2path`
    # `path2tilde $PWD | tee /dev/tty |HOME='$0' tilde2path`
    ##
}

function path-unabbrev-simple {
    ec "$(in-or-args "$@")" | perl -lpe 's/^~/$ENV{HOME}/g'
}
##
function f-size {
    local f="$1" h="${f_size_h}"
    assert test -e "$f" @RET

    gstat --printf="%s" "$f" | { # in bytes
        if bool "$h" ; then
            numfmt-humanfriendly-bytes
        else
            cat
        fi
    }
}
reify f-size

function f-size-labeled {
    gdu -h --apparent-size "$@"
}
##
function mktemp-exact {
    local name="$1"
    assert-args name @RET

    local tmpdir
    tmpdir="$(gmktemp --directory)" @TRET
    ec "${tmpdir}/${name}"
}
##
function windows-newlines-to-unix {
    cat-paste-if-tty |
        sd '\r\n' '\n' |
        cat-copy-if-tty
}
##
function find-other-writable-dirs {
  local dir="${1:-"."}"
  shift  # shift arguments, so $@ contains only regexes

  local find_cmd=("find" "$dir" "-type" "d" "-perm" "-002")

  for regex in "$@"; do
    find_cmd+=("-not" "-regex" "$regex")
  done

  reval "${find_cmd[@]}"
}
##
function fd-size {
    local opts=("$@")

    # Use fd to list files, respecting .gitignore by default, then pass to xargs for size calculation
    command fd --type f --hidden --ignore-vcs --exclude '.git' -0 "${opts[@]}" |
        xargs -0 gdu -h |
        gsort -rh | tac
}
##
function file-ends-with-newline-p {
    local file="$1"
    if [[ ! -s "$file" ]]; then
        #: If the file doesn't exist or it is empty, we can concat material to it without needing to add a newline, so return true.
        return 0
    fi

    #: If the last char is a newline, it will be stripped away and we will be left with an empty string:
    [[ "$(tail -c 1 "$file")" == "" ]] ; return $?
    ##
    # perl -e 'exit((`tail -c 1 $ARGV[0]` eq "\n") ? 0 : 1)' -- "$file" ; return $?
    ##
}
##
function du-video {
    du-h --all | rg -i "\\.(?:${(pj.|.)video_formats})\$" | gsort -h
}
aliasfn duv du-video

function ffduv {
    du-video | gcut -f 2- | tac | fz | inargsf rgeval m
}
##
