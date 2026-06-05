### Initiate the Darkness
export NIGHTDIR="${${(%):-%x}:A:h:h}/" # echo "_: $_ 0: $0 bs: $BASH_SOURCE"
export night_prompt_dir="${NIGHTDIR}/PE"

source "${NIGHTDIR}/zshlang/basic/basic-full.zsh" || return $?

function jinit() {
    if [[ "$PWD" == *borg*/dls/* ]] ; then
        export jufile=(*(.DN))
        export j=(${jufile[@]})
        export jd="$PWD"
    fi
}
jinit
##
