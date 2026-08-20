### Initiate the Darkness
#: No trailing slash: every use of it writes its own, so carrying one here put
#: a `//' into 259 of the 307 PATH entries and into everything else built from
#: it, night_prompt_dir on the next line included. Nothing concatenates a name
#: directly onto ${NIGHTDIR}, so dropping it is safe; the non-shell readers
#: (madmailer.py, hammerspoon/boot.lua) prepend a slash of their own too.
export NIGHTDIR="${${(%):-%x}:A:h:h}" # echo "_: $_ 0: $0 bs: $BASH_SOURCE"
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
