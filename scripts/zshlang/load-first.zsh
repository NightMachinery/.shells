### Initiate the Darkness
export NIGHTDIR="${0:h:h}/" # echo "_: $_ 0: $0 bs: $BASH_SOURCE"
source-basic() {
    local i
    for i in "$@" ; do
        source "$NIGHTDIR"/zshlang/basic/"$i".zsh
    done
}
source-basic basic
# malice is the alias module. :D
source-basic magicmacros cached conditions crossplatform args colors debug text-manipulation ssh malice eval enhancers redirections functional macros redis
for i in "$NIGHTDIR"/zshlang/basic/auto-load/**/*.zsh(.) ; do
    source "$i"
done
function jinit() {
    [[ "$(pwd)" != *borg* ]] || {
        # For use with the Julia module.
        unset jufile j jd
        export jufile=""
        silence eval 'export jufile=(*)' && export j="$jufile" || export j=""
        export jd="$(pwd)"
    }
}
jinit
