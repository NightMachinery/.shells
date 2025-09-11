### Initiate the Darkness
export NIGHTDIR="${0:h:h}/" # echo "_: $_ 0: $0 bs: $BASH_SOURCE"
export night_prompt_dir="${NIGHTDIR}/PE"

source-basic() {
    local i
    for i in "$@" ; do
        source "$NIGHTDIR"/zshlang/basic/"$i".zsh
    done
}
source-basic basic
# malice is the alias module. :D
source-basic variables compatibility magicmacros deps cached conditions crossplatform args colors debug text-manipulation ssh malice history eval enhancers redirections functional macros redis
for i in "$NIGHTDIR"/zshlang/basic/auto-load/**/*.zsh(.) ; do
    source "$i"
done
source-basic proxy

function jinit() {
    if [[ "$PWD" == *borg*/dls/* ]] ; then
        export jufile=(*(.DN))
        export j=(${jufile[@]})
        export jd="$PWD"
    fi
}
jinit
##
@opts-setprefix assert ensure
##
