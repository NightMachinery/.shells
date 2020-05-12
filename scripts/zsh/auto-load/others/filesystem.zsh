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

    (silence eval '\: *(D)') && {
        ecerr Directory "$(pwd)" not empty
        return 1
    } || return 0
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
